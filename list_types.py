import struct as st

def read_uleb128(f):
	val = 0
	i = 0
	while True:
		b = f.read(1)[0]
		val |= ((b & 0x7f) << (i*7))
		if b & 0x80 == 0: break
		i += 1
	return val


f = open('client/classes.dex', 'rb')

f.seek(8 + 4 + 20 + 4 + 4 + 4 + 4 + 4 + 4)
strings_ids_size = st.unpack('I', f.read(4))[0]
strings_ids_off = st.unpack('I', f.read(4))[0]
type_ids_size = st.unpack('I', f.read(4))[0]
type_ids_off = st.unpack('I', f.read(4))[0]

for i in range(type_ids_size):#//4):
	f.seek(type_ids_off + i*4)
	descriptor_idx = st.unpack('I', f.read(4))[0]
	
	f.seek(strings_ids_off + descriptor_idx*4)
	string_data_off = st.unpack('I', f.read(4))[0]


	f.seek(string_data_off)
	utf16_size = read_uleb128(f)
	type_name = f.read(utf16_size).decode('utf8')

	if type_name[0] == 'L':
		type_name = type_name[1:-1]

	print(type_name)
	if i == 20: break

f.close()
