import struct as st
import sys

def read_uleb128(f, retlen=False):
	val = 0
	i = 0
	while True:
		b = f.read(1)[0]
		val |= ((b & 0x7f) << (i*7))
		i += 1
		if b & 0x80 == 0: break
	if retlen: return val, i
	return val

def read_string(f, idx):
	f.seek(strings_ids_off + idx*4)
	string_data_off = st.unpack('I', f.read(4))[0]

	f.seek(string_data_off)
	utf16_size = read_uleb128(f)
	string = f.read(utf16_size).decode('utf8')

	return string

def read_descriptor_idx(f, idx):
	f.seek(type_ids_off + idx*4)
	return st.unpack('I', f.read(4))[0]

def read_encoded_fields(f, base, count):
	fields = []
	prev_field_idx = 0

	for i in range(count):
		f.seek(base + i*8)
		field_idx_diff = read_uleb128(f)
		access_flags = read_uleb128(f)

		field_idx = field_idx_diff + prev_field_idx
		prev_field_idx = field_idx

		f.seek(field_ids_off + field_idx*8)
		class_idx = st.unpack('H', f.read(2))[0]
		type_idx = st.unpack('H', f.read(2))[0]
		name_idx = st.unpack('I', f.read(4))[0]

		class_name = read_string(f, read_descriptor_idx(f, class_idx))
		type_name = read_string(f, read_descriptor_idx(f, type_idx))
		name = read_string(f, name_idx)

		fields.append({
			'definer': class_name,
			'type': type_name,
			'name': name
		})

	return fields

def read_encoded_methods(f, base, count):
	methods = []
	prev_method_idx = 0

	for i in range(count):
		f.seek(base + i*12)
		method_idx_diff = read_uleb128(f)
		access_flags = read_uleb128(f)
		code_off = read_uleb128(f)

		method_idx = prev_method_idx + method_idx_diff
		prev_method_idx = method_idx

		f.seek(method_ids_off + method_idx*8)
		class_idx = st.unpack('H', f.read(2))[0]
		type_idx = st.unpack('H', f.read(2))[0]
		name_idx = st.unpack('I', f.read(4))[0]

		class_name = read_string(f, read_descriptor_idx(f, class_idx))
		type_name = read_string(f, read_descriptor_idx(f, type_idx))
		name = read_string(f, name_idx)

		methods.append({
			'definer': class_name,
			'type': type_name,
			'name': name
		})

	return methods


class_name = sys.argv[1]
f = open('client/classes.dex', 'rb')

f.seek(8 + 4 + 20 + 4 + 4 + 4 + 4 + 4 + 4)
strings_ids_size = st.unpack('I', f.read(4))[0]
strings_ids_off = st.unpack('I', f.read(4))[0]
type_ids_size = st.unpack('I', f.read(4))[0]
type_ids_off = st.unpack('I', f.read(4))[0]
f.seek(4 + 4, 1)
field_ids_size = st.unpack('I', f.read(4))[0]
field_ids_off = st.unpack('I', f.read(4))[0]
method_ids_size = st.unpack('I', f.read(4))[0]
method_ids_off = st.unpack('I', f.read(4))[0]
class_defs_size = st.unpack('I', f.read(4))[0]
class_defs_off = st.unpack('I', f.read(4))[0]

found_it = False
class_def_off = 0

for i in range(class_defs_size):
	class_def_off = class_defs_off + i*32
	f.seek(class_def_off)
	class_idx = st.unpack('I', f.read(4))[0]

	descriptor_idx = read_descriptor_idx(f, class_idx)
	type_name = read_string(f, descriptor_idx)
	if class_name in type_name and not '$' in type_name:
		found_it = True
		break

if not found_it:
	print("Couldn't find class")
	exit()

f.seek(class_def_off)
f.seek(4 + 4 + 4 + 4 + 4 + 4, 1)
class_data_off = st.unpack('I', f.read(4))[0]
static_fields_size = read_uleb128(f)
instance_fields_size = read_uleb128(f)
direct_methods_size = read_uleb128(f)
virtual_methods_size = read_uleb128(f)

static_fields_off = f.tell()
static_fields = read_encoded_fields(f, static_fields_off, static_fields_size)
instance_fields = read_encoded_fields(f, static_fields_off + static_fields_size*8, instance_fields_size)

direct_methods_off = f.tell()
direct_methods = read_encoded_methods(f, direct_methods_off, direct_methods_size)
virtual_methods = read_encoded_methods(f, direct_methods_off + static_methods_off*12, virtual_methods_size)

print('Static Fields:')
print(static_fields)
print('\n******\n')

print('Instance Fields:')
print(instance_fields)
print('\n******\n')

print('Direct Methods:')
print(direct_methods)
print('\n******\n')

print('Virtual Methods:')
print(virtual_methods)
print('\n******\n')

f.close()
