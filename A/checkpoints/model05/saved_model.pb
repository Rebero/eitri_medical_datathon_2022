??
??
D
AddV2
x"T
y"T
z"T"
Ttype:
2	??
^
AssignVariableOp
resource
value"dtype"
dtypetype"
validate_shapebool( ?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
h
ConcatV2
values"T*N
axis"Tidx
output"T"
Nint(0"	
Ttype"
Tidxtype0:
2	
8
Const
output"dtype"
valuetensor"
dtypetype
^
Fill
dims"
index_type

value"T
output"T"	
Ttype"

index_typetype0:
2	
?
FusedBatchNormV3
x"T

scale"U
offset"U	
mean"U
variance"U
y"T

batch_mean"U
batch_variance"U
reserve_space_1"U
reserve_space_2"U
reserve_space_3"U"
Ttype:
2"
Utype:
2"
epsilonfloat%??8"&
exponential_avg_factorfloat%  ??";
data_formatstringNHWC:
NHWCNCHWNDHWCNCDHW"
is_trainingbool(
.
Identity

input"T
output"T"	
Ttype
\
	LeakyRelu
features"T
activations"T"
alphafloat%??L>"
Ttype0:
2
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
?
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool("
allow_missing_filesbool( ?
?
Mul
x"T
y"T
z"T"
Ttype:
2	?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetype?
[
Reshape
tensor"T
shape"Tshape
output"T"	
Ttype"
Tshapetype0:
2	
?
ResourceGather
resource
indices"Tindices
output"dtype"

batch_dimsint "
validate_indicesbool("
dtypetype"
Tindicestype:
2	?
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ??
@
StaticRegexFullMatch	
input

output
"
patternstring
?
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?"serve*2.10.02v2.10.0-rc3-6-g359c3cdfc5f8??
?
"Adam/readmission_prediction/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*3
shared_name$"Adam/readmission_prediction/bias/v
?
6Adam/readmission_prediction/bias/v/Read/ReadVariableOpReadVariableOp"Adam/readmission_prediction/bias/v*
_output_shapes
:*
dtype0
?
$Adam/readmission_prediction/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*5
shared_name&$Adam/readmission_prediction/kernel/v
?
8Adam/readmission_prediction/kernel/v/Read/ReadVariableOpReadVariableOp$Adam/readmission_prediction/kernel/v*
_output_shapes
:	?*
dtype0

Adam/dense_8/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_8/bias/v
x
'Adam/dense_8/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_8/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_8/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_8/kernel/v
?
)Adam/dense_8/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_8/kernel/v*
_output_shapes
:	j?*
dtype0

Adam/dense_7/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_7/bias/v
x
'Adam/dense_7/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_7/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_7/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*&
shared_nameAdam/dense_7/kernel/v
?
)Adam/dense_7/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_7/kernel/v* 
_output_shapes
:
??*
dtype0

Adam/dense_6/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_6/bias/v
x
'Adam/dense_6/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_6/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_6/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_6/kernel/v
?
)Adam/dense_6/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_6/kernel/v*
_output_shapes
:	j?*
dtype0

Adam/dense_5/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_5/bias/v
x
'Adam/dense_5/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_5/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_5/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*&
shared_nameAdam/dense_5/kernel/v
?
)Adam/dense_5/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_5/kernel/v* 
_output_shapes
:
??*
dtype0

Adam/dense_4/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_4/bias/v
x
'Adam/dense_4/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_4/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_4/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_4/kernel/v
?
)Adam/dense_4/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_4/kernel/v*
_output_shapes
:	j?*
dtype0

Adam/dense_3/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_3/bias/v
x
'Adam/dense_3/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_3/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_3/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*&
shared_nameAdam/dense_3/kernel/v
?
)Adam/dense_3/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_3/kernel/v* 
_output_shapes
:
??*
dtype0

Adam/dense_2/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_2/bias/v
x
'Adam/dense_2/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_2/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_2/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_2/kernel/v
?
)Adam/dense_2/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_2/kernel/v*
_output_shapes
:	j?*
dtype0

Adam/dense_1/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_1/bias/v
x
'Adam/dense_1/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_1/bias/v*
_output_shapes	
:?*
dtype0
?
Adam/dense_1/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_1/kernel/v
?
)Adam/dense_1/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_1/kernel/v*
_output_shapes
:	j?*
dtype0
?
Adam/layer_normalization/beta/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*0
shared_name!Adam/layer_normalization/beta/v
?
3Adam/layer_normalization/beta/v/Read/ReadVariableOpReadVariableOpAdam/layer_normalization/beta/v*
_output_shapes
:j*
dtype0
?
 Adam/layer_normalization/gamma/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*1
shared_name" Adam/layer_normalization/gamma/v
?
4Adam/layer_normalization/gamma/v/Read/ReadVariableOpReadVariableOp Adam/layer_normalization/gamma/v*
_output_shapes
:j*
dtype0
z
Adam/dense/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*"
shared_nameAdam/dense/bias/v
s
%Adam/dense/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense/bias/v*
_output_shapes
:j*
dtype0
?
Adam/dense/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:jj*$
shared_nameAdam/dense/kernel/v
{
'Adam/dense/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense/kernel/v*
_output_shapes

:jj*
dtype0
?
Adam/embedding_4/embeddings/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?<@*.
shared_nameAdam/embedding_4/embeddings/v
?
1Adam/embedding_4/embeddings/v/Read/ReadVariableOpReadVariableOpAdam/embedding_4/embeddings/v*
_output_shapes
:	?<@*
dtype0
?
Adam/embedding_3/embeddings/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*.
shared_nameAdam/embedding_3/embeddings/v
?
1Adam/embedding_3/embeddings/v/Read/ReadVariableOpReadVariableOpAdam/embedding_3/embeddings/v*
_output_shapes

:*
dtype0
?
Adam/embedding_2/embeddings/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*.
shared_nameAdam/embedding_2/embeddings/v
?
1Adam/embedding_2/embeddings/v/Read/ReadVariableOpReadVariableOpAdam/embedding_2/embeddings/v*
_output_shapes

:*
dtype0
?
Adam/embedding_1/embeddings/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:

*.
shared_nameAdam/embedding_1/embeddings/v
?
1Adam/embedding_1/embeddings/v/Read/ReadVariableOpReadVariableOpAdam/embedding_1/embeddings/v*
_output_shapes

:

*
dtype0
?
Adam/embedding/embeddings/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*,
shared_nameAdam/embedding/embeddings/v
?
/Adam/embedding/embeddings/v/Read/ReadVariableOpReadVariableOpAdam/embedding/embeddings/v*
_output_shapes

:*
dtype0
?
"Adam/readmission_prediction/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*3
shared_name$"Adam/readmission_prediction/bias/m
?
6Adam/readmission_prediction/bias/m/Read/ReadVariableOpReadVariableOp"Adam/readmission_prediction/bias/m*
_output_shapes
:*
dtype0
?
$Adam/readmission_prediction/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*5
shared_name&$Adam/readmission_prediction/kernel/m
?
8Adam/readmission_prediction/kernel/m/Read/ReadVariableOpReadVariableOp$Adam/readmission_prediction/kernel/m*
_output_shapes
:	?*
dtype0

Adam/dense_8/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_8/bias/m
x
'Adam/dense_8/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_8/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_8/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_8/kernel/m
?
)Adam/dense_8/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_8/kernel/m*
_output_shapes
:	j?*
dtype0

Adam/dense_7/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_7/bias/m
x
'Adam/dense_7/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_7/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_7/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*&
shared_nameAdam/dense_7/kernel/m
?
)Adam/dense_7/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_7/kernel/m* 
_output_shapes
:
??*
dtype0

Adam/dense_6/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_6/bias/m
x
'Adam/dense_6/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_6/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_6/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_6/kernel/m
?
)Adam/dense_6/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_6/kernel/m*
_output_shapes
:	j?*
dtype0

Adam/dense_5/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_5/bias/m
x
'Adam/dense_5/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_5/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_5/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*&
shared_nameAdam/dense_5/kernel/m
?
)Adam/dense_5/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_5/kernel/m* 
_output_shapes
:
??*
dtype0

Adam/dense_4/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_4/bias/m
x
'Adam/dense_4/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_4/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_4/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_4/kernel/m
?
)Adam/dense_4/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_4/kernel/m*
_output_shapes
:	j?*
dtype0

Adam/dense_3/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_3/bias/m
x
'Adam/dense_3/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_3/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_3/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*&
shared_nameAdam/dense_3/kernel/m
?
)Adam/dense_3/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_3/kernel/m* 
_output_shapes
:
??*
dtype0

Adam/dense_2/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_2/bias/m
x
'Adam/dense_2/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_2/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_2/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_2/kernel/m
?
)Adam/dense_2/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_2/kernel/m*
_output_shapes
:	j?*
dtype0

Adam/dense_1/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*$
shared_nameAdam/dense_1/bias/m
x
'Adam/dense_1/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_1/bias/m*
_output_shapes	
:?*
dtype0
?
Adam/dense_1/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*&
shared_nameAdam/dense_1/kernel/m
?
)Adam/dense_1/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_1/kernel/m*
_output_shapes
:	j?*
dtype0
?
Adam/layer_normalization/beta/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*0
shared_name!Adam/layer_normalization/beta/m
?
3Adam/layer_normalization/beta/m/Read/ReadVariableOpReadVariableOpAdam/layer_normalization/beta/m*
_output_shapes
:j*
dtype0
?
 Adam/layer_normalization/gamma/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*1
shared_name" Adam/layer_normalization/gamma/m
?
4Adam/layer_normalization/gamma/m/Read/ReadVariableOpReadVariableOp Adam/layer_normalization/gamma/m*
_output_shapes
:j*
dtype0
z
Adam/dense/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*"
shared_nameAdam/dense/bias/m
s
%Adam/dense/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense/bias/m*
_output_shapes
:j*
dtype0
?
Adam/dense/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:jj*$
shared_nameAdam/dense/kernel/m
{
'Adam/dense/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense/kernel/m*
_output_shapes

:jj*
dtype0
?
Adam/embedding_4/embeddings/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?<@*.
shared_nameAdam/embedding_4/embeddings/m
?
1Adam/embedding_4/embeddings/m/Read/ReadVariableOpReadVariableOpAdam/embedding_4/embeddings/m*
_output_shapes
:	?<@*
dtype0
?
Adam/embedding_3/embeddings/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*.
shared_nameAdam/embedding_3/embeddings/m
?
1Adam/embedding_3/embeddings/m/Read/ReadVariableOpReadVariableOpAdam/embedding_3/embeddings/m*
_output_shapes

:*
dtype0
?
Adam/embedding_2/embeddings/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*.
shared_nameAdam/embedding_2/embeddings/m
?
1Adam/embedding_2/embeddings/m/Read/ReadVariableOpReadVariableOpAdam/embedding_2/embeddings/m*
_output_shapes

:*
dtype0
?
Adam/embedding_1/embeddings/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:

*.
shared_nameAdam/embedding_1/embeddings/m
?
1Adam/embedding_1/embeddings/m/Read/ReadVariableOpReadVariableOpAdam/embedding_1/embeddings/m*
_output_shapes

:

*
dtype0
?
Adam/embedding/embeddings/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*,
shared_nameAdam/embedding/embeddings/m
?
/Adam/embedding/embeddings/m/Read/ReadVariableOpReadVariableOpAdam/embedding/embeddings/m*
_output_shapes

:*
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
x
Adam/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *#
shared_nameAdam/learning_rate
q
&Adam/learning_rate/Read/ReadVariableOpReadVariableOpAdam/learning_rate*
_output_shapes
: *
dtype0
h

Adam/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name
Adam/decay
a
Adam/decay/Read/ReadVariableOpReadVariableOp
Adam/decay*
_output_shapes
: *
dtype0
j
Adam/beta_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_2
c
Adam/beta_2/Read/ReadVariableOpReadVariableOpAdam/beta_2*
_output_shapes
: *
dtype0
j
Adam/beta_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_1
c
Adam/beta_1/Read/ReadVariableOpReadVariableOpAdam/beta_1*
_output_shapes
: *
dtype0
f
	Adam/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_name	Adam/iter
_
Adam/iter/Read/ReadVariableOpReadVariableOp	Adam/iter*
_output_shapes
: *
dtype0	
?
readmission_prediction/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*,
shared_namereadmission_prediction/bias
?
/readmission_prediction/bias/Read/ReadVariableOpReadVariableOpreadmission_prediction/bias*
_output_shapes
:*
dtype0
?
readmission_prediction/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*.
shared_namereadmission_prediction/kernel
?
1readmission_prediction/kernel/Read/ReadVariableOpReadVariableOpreadmission_prediction/kernel*
_output_shapes
:	?*
dtype0
q
dense_8/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_8/bias
j
 dense_8/bias/Read/ReadVariableOpReadVariableOpdense_8/bias*
_output_shapes	
:?*
dtype0
y
dense_8/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*
shared_namedense_8/kernel
r
"dense_8/kernel/Read/ReadVariableOpReadVariableOpdense_8/kernel*
_output_shapes
:	j?*
dtype0
q
dense_7/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_7/bias
j
 dense_7/bias/Read/ReadVariableOpReadVariableOpdense_7/bias*
_output_shapes	
:?*
dtype0
z
dense_7/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*
shared_namedense_7/kernel
s
"dense_7/kernel/Read/ReadVariableOpReadVariableOpdense_7/kernel* 
_output_shapes
:
??*
dtype0
q
dense_6/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_6/bias
j
 dense_6/bias/Read/ReadVariableOpReadVariableOpdense_6/bias*
_output_shapes	
:?*
dtype0
y
dense_6/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*
shared_namedense_6/kernel
r
"dense_6/kernel/Read/ReadVariableOpReadVariableOpdense_6/kernel*
_output_shapes
:	j?*
dtype0
q
dense_5/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_5/bias
j
 dense_5/bias/Read/ReadVariableOpReadVariableOpdense_5/bias*
_output_shapes	
:?*
dtype0
z
dense_5/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*
shared_namedense_5/kernel
s
"dense_5/kernel/Read/ReadVariableOpReadVariableOpdense_5/kernel* 
_output_shapes
:
??*
dtype0
q
dense_4/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_4/bias
j
 dense_4/bias/Read/ReadVariableOpReadVariableOpdense_4/bias*
_output_shapes	
:?*
dtype0
y
dense_4/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*
shared_namedense_4/kernel
r
"dense_4/kernel/Read/ReadVariableOpReadVariableOpdense_4/kernel*
_output_shapes
:	j?*
dtype0
q
dense_3/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_3/bias
j
 dense_3/bias/Read/ReadVariableOpReadVariableOpdense_3/bias*
_output_shapes	
:?*
dtype0
z
dense_3/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:
??*
shared_namedense_3/kernel
s
"dense_3/kernel/Read/ReadVariableOpReadVariableOpdense_3/kernel* 
_output_shapes
:
??*
dtype0
q
dense_2/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_2/bias
j
 dense_2/bias/Read/ReadVariableOpReadVariableOpdense_2/bias*
_output_shapes	
:?*
dtype0
y
dense_2/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*
shared_namedense_2/kernel
r
"dense_2/kernel/Read/ReadVariableOpReadVariableOpdense_2/kernel*
_output_shapes
:	j?*
dtype0
q
dense_1/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*
shared_namedense_1/bias
j
 dense_1/bias/Read/ReadVariableOpReadVariableOpdense_1/bias*
_output_shapes	
:?*
dtype0
y
dense_1/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	j?*
shared_namedense_1/kernel
r
"dense_1/kernel/Read/ReadVariableOpReadVariableOpdense_1/kernel*
_output_shapes
:	j?*
dtype0
?
layer_normalization/betaVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*)
shared_namelayer_normalization/beta
?
,layer_normalization/beta/Read/ReadVariableOpReadVariableOplayer_normalization/beta*
_output_shapes
:j*
dtype0
?
layer_normalization/gammaVarHandleOp*
_output_shapes
: *
dtype0*
shape:j**
shared_namelayer_normalization/gamma
?
-layer_normalization/gamma/Read/ReadVariableOpReadVariableOplayer_normalization/gamma*
_output_shapes
:j*
dtype0
l

dense/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:j*
shared_name
dense/bias
e
dense/bias/Read/ReadVariableOpReadVariableOp
dense/bias*
_output_shapes
:j*
dtype0
t
dense/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:jj*
shared_namedense/kernel
m
 dense/kernel/Read/ReadVariableOpReadVariableOpdense/kernel*
_output_shapes

:jj*
dtype0
?
embedding_4/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?<@*'
shared_nameembedding_4/embeddings
?
*embedding_4/embeddings/Read/ReadVariableOpReadVariableOpembedding_4/embeddings*
_output_shapes
:	?<@*
dtype0
?
embedding_3/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*'
shared_nameembedding_3/embeddings
?
*embedding_3/embeddings/Read/ReadVariableOpReadVariableOpembedding_3/embeddings*
_output_shapes

:*
dtype0
?
embedding_2/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*'
shared_nameembedding_2/embeddings
?
*embedding_2/embeddings/Read/ReadVariableOpReadVariableOpembedding_2/embeddings*
_output_shapes

:*
dtype0
?
embedding_1/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:

*'
shared_nameembedding_1/embeddings
?
*embedding_1/embeddings/Read/ReadVariableOpReadVariableOpembedding_1/embeddings*
_output_shapes

:

*
dtype0
?
embedding/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*%
shared_nameembedding/embeddings
}
(embedding/embeddings/Read/ReadVariableOpReadVariableOpembedding/embeddings*
_output_shapes

:*
dtype0
v
serving_default_DRGPlaceholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
|
serving_default_age_groupPlaceholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
~
serving_default_bed_day_depPlaceholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
?
!serving_default_degree_of_urgencyPlaceholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
?
 serving_default_diagnosis_code_1Placeholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
y
serving_default_genderPlaceholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
?
$serving_default_mainDiagICD10ChapterPlaceholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
?
StatefulPartitionedCallStatefulPartitionedCallserving_default_DRGserving_default_age_groupserving_default_bed_day_dep!serving_default_degree_of_urgency serving_default_diagnosis_code_1serving_default_gender$serving_default_mainDiagICD10Chapterembedding/embeddingsembedding_1/embeddingsembedding_2/embeddingsembedding_3/embeddingsembedding_4/embeddingsdense/kernel
dense/biaslayer_normalization/gammalayer_normalization/betadense_1/kerneldense_1/biasdense_2/kerneldense_2/biasdense_3/kerneldense_3/biasdense_4/kerneldense_4/biasdense_5/kerneldense_5/biasdense_6/kerneldense_6/biasdense_7/kerneldense_7/biasdense_8/kerneldense_8/biasreadmission_prediction/kernelreadmission_prediction/bias*-
Tin&
$2"*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*=
_read_only_resource_inputs
	
 !*-
config_proto

CPU

GPU 2J 8? *-
f(R&
$__inference_signature_wrapper_741451

NoOpNoOp
??
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*??
value??B?? B??
?
layer-0
layer-1
layer-2
layer-3
layer-4
layer-5
layer-6
layer-7
	layer-8

layer_with_weights-0

layer-9
layer_with_weights-1
layer-10
layer_with_weights-2
layer-11
layer_with_weights-3
layer-12
layer_with_weights-4
layer-13
layer-14
layer-15
layer_with_weights-5
layer-16
layer_with_weights-6
layer-17
layer-18
layer_with_weights-7
layer-19
layer_with_weights-8
layer-20
layer-21
layer-22
layer_with_weights-9
layer-23
layer_with_weights-10
layer-24
layer-25
layer-26
layer_with_weights-11
layer-27
layer_with_weights-12
layer-28
layer-29
layer-30
 layer_with_weights-13
 layer-31
!layer_with_weights-14
!layer-32
"layer-33
#layer-34
$layer_with_weights-15
$layer-35
%	variables
&trainable_variables
'regularization_losses
(	keras_api
)__call__
**&call_and_return_all_conditional_losses
+_default_save_signature
,	optimizer
-
signatures*
* 
* 
* 
* 
* 
* 
* 

.	keras_api* 

/	keras_api* 
?
0	variables
1trainable_variables
2regularization_losses
3	keras_api
4__call__
*5&call_and_return_all_conditional_losses
6
embeddings*
?
7	variables
8trainable_variables
9regularization_losses
:	keras_api
;__call__
*<&call_and_return_all_conditional_losses
=
embeddings*
?
>	variables
?trainable_variables
@regularization_losses
A	keras_api
B__call__
*C&call_and_return_all_conditional_losses
D
embeddings*
?
E	variables
Ftrainable_variables
Gregularization_losses
H	keras_api
I__call__
*J&call_and_return_all_conditional_losses
K
embeddings*
?
L	variables
Mtrainable_variables
Nregularization_losses
O	keras_api
P__call__
*Q&call_and_return_all_conditional_losses
R
embeddings*
?
S	variables
Ttrainable_variables
Uregularization_losses
V	keras_api
W__call__
*X&call_and_return_all_conditional_losses* 
?
Y	variables
Ztrainable_variables
[regularization_losses
\	keras_api
]__call__
*^&call_and_return_all_conditional_losses* 
?
_	variables
`trainable_variables
aregularization_losses
b	keras_api
c__call__
*d&call_and_return_all_conditional_losses

ekernel
fbias*
?
g	variables
htrainable_variables
iregularization_losses
j	keras_api
k__call__
*l&call_and_return_all_conditional_losses
maxis
	ngamma
obeta*
?
p	variables
qtrainable_variables
rregularization_losses
s	keras_api
t__call__
*u&call_and_return_all_conditional_losses
v_random_generator* 
?
w	variables
xtrainable_variables
yregularization_losses
z	keras_api
{__call__
*|&call_and_return_all_conditional_losses
}
activation

~kernel
bias*
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias*

?	keras_api* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?
activation
?kernel
	?bias*
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias*

?	keras_api* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?
activation
?kernel
	?bias*
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias*

?	keras_api* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?
activation
?kernel
	?bias*
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias*

?	keras_api* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias*
?
60
=1
D2
K3
R4
e5
f6
n7
o8
~9
10
?11
?12
?13
?14
?15
?16
?17
?18
?19
?20
?21
?22
?23
?24
?25
?26*
?
60
=1
D2
K3
R4
e5
f6
n7
o8
~9
10
?11
?12
?13
?14
?15
?16
?17
?18
?19
?20
?21
?22
?23
?24
?25
?26*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
%	variables
&trainable_variables
'regularization_losses
)__call__
+_default_save_signature
**&call_and_return_all_conditional_losses
&*"call_and_return_conditional_losses*
:
?trace_0
?trace_1
?trace_2
?trace_3* 
:
?trace_0
?trace_1
?trace_2
?trace_3* 
* 
?
	?iter
?beta_1
?beta_2

?decay
?learning_rate6m?=m?Dm?Km?Rm?em?fm?nm?om?~m?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?6v?=v?Dv?Kv?Rv?ev?fv?nv?ov?~v?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?*

?serving_default* 
* 
* 

60*

60*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
0	variables
1trainable_variables
2regularization_losses
4__call__
*5&call_and_return_all_conditional_losses
&5"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
hb
VARIABLE_VALUEembedding/embeddings:layer_with_weights-0/embeddings/.ATTRIBUTES/VARIABLE_VALUE*

=0*

=0*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
7	variables
8trainable_variables
9regularization_losses
;__call__
*<&call_and_return_all_conditional_losses
&<"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
jd
VARIABLE_VALUEembedding_1/embeddings:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUE*

D0*

D0*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
>	variables
?trainable_variables
@regularization_losses
B__call__
*C&call_and_return_all_conditional_losses
&C"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
jd
VARIABLE_VALUEembedding_2/embeddings:layer_with_weights-2/embeddings/.ATTRIBUTES/VARIABLE_VALUE*

K0*

K0*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
E	variables
Ftrainable_variables
Gregularization_losses
I__call__
*J&call_and_return_all_conditional_losses
&J"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
jd
VARIABLE_VALUEembedding_3/embeddings:layer_with_weights-3/embeddings/.ATTRIBUTES/VARIABLE_VALUE*

R0*

R0*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
L	variables
Mtrainable_variables
Nregularization_losses
P__call__
*Q&call_and_return_all_conditional_losses
&Q"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
jd
VARIABLE_VALUEembedding_4/embeddings:layer_with_weights-4/embeddings/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
S	variables
Ttrainable_variables
Uregularization_losses
W__call__
*X&call_and_return_all_conditional_losses
&X"call_and_return_conditional_losses* 

?trace_0* 

?trace_0* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
Y	variables
Ztrainable_variables
[regularization_losses
]__call__
*^&call_and_return_all_conditional_losses
&^"call_and_return_conditional_losses* 

?trace_0* 

?trace_0* 

e0
f1*

e0
f1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
_	variables
`trainable_variables
aregularization_losses
c__call__
*d&call_and_return_all_conditional_losses
&d"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
\V
VARIABLE_VALUEdense/kernel6layer_with_weights-5/kernel/.ATTRIBUTES/VARIABLE_VALUE*
XR
VARIABLE_VALUE
dense/bias4layer_with_weights-5/bias/.ATTRIBUTES/VARIABLE_VALUE*

n0
o1*

n0
o1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
g	variables
htrainable_variables
iregularization_losses
k__call__
*l&call_and_return_all_conditional_losses
&l"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
* 
hb
VARIABLE_VALUElayer_normalization/gamma5layer_with_weights-6/gamma/.ATTRIBUTES/VARIABLE_VALUE*
f`
VARIABLE_VALUElayer_normalization/beta4layer_with_weights-6/beta/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
p	variables
qtrainable_variables
rregularization_losses
t__call__
*u&call_and_return_all_conditional_losses
&u"call_and_return_conditional_losses* 

?trace_0
?trace_1* 

?trace_0
?trace_1* 
* 

~0
1*

~0
1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
w	variables
xtrainable_variables
yregularization_losses
{__call__
*|&call_and_return_all_conditional_losses
&|"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses* 
^X
VARIABLE_VALUEdense_1/kernel6layer_with_weights-7/kernel/.ATTRIBUTES/VARIABLE_VALUE*
ZT
VARIABLE_VALUEdense_1/bias4layer_with_weights-7/bias/.ATTRIBUTES/VARIABLE_VALUE*

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
^X
VARIABLE_VALUEdense_2/kernel6layer_with_weights-8/kernel/.ATTRIBUTES/VARIABLE_VALUE*
ZT
VARIABLE_VALUEdense_2/bias4layer_with_weights-8/bias/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 

?trace_0
?trace_1* 

?trace_0
?trace_1* 
* 

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses* 
^X
VARIABLE_VALUEdense_3/kernel6layer_with_weights-9/kernel/.ATTRIBUTES/VARIABLE_VALUE*
ZT
VARIABLE_VALUEdense_3/bias4layer_with_weights-9/bias/.ATTRIBUTES/VARIABLE_VALUE*

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
_Y
VARIABLE_VALUEdense_4/kernel7layer_with_weights-10/kernel/.ATTRIBUTES/VARIABLE_VALUE*
[U
VARIABLE_VALUEdense_4/bias5layer_with_weights-10/bias/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 

?trace_0
?trace_1* 

?trace_0
?trace_1* 
* 

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses* 
_Y
VARIABLE_VALUEdense_5/kernel7layer_with_weights-11/kernel/.ATTRIBUTES/VARIABLE_VALUE*
[U
VARIABLE_VALUEdense_5/bias5layer_with_weights-11/bias/.ATTRIBUTES/VARIABLE_VALUE*

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
_Y
VARIABLE_VALUEdense_6/kernel7layer_with_weights-12/kernel/.ATTRIBUTES/VARIABLE_VALUE*
[U
VARIABLE_VALUEdense_6/bias5layer_with_weights-12/bias/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 

?trace_0
?trace_1* 

?trace_0
?trace_1* 
* 

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses* 
_Y
VARIABLE_VALUEdense_7/kernel7layer_with_weights-13/kernel/.ATTRIBUTES/VARIABLE_VALUE*
[U
VARIABLE_VALUEdense_7/bias5layer_with_weights-13/bias/.ATTRIBUTES/VARIABLE_VALUE*

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
_Y
VARIABLE_VALUEdense_8/kernel7layer_with_weights-14/kernel/.ATTRIBUTES/VARIABLE_VALUE*
[U
VARIABLE_VALUEdense_8/bias5layer_with_weights-14/bias/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 

?trace_0
?trace_1* 

?trace_0
?trace_1* 
* 

?0
?1*

?0
?1*
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses*

?trace_0* 

?trace_0* 
nh
VARIABLE_VALUEreadmission_prediction/kernel7layer_with_weights-15/kernel/.ATTRIBUTES/VARIABLE_VALUE*
jd
VARIABLE_VALUEreadmission_prediction/bias5layer_with_weights-15/bias/.ATTRIBUTES/VARIABLE_VALUE*
* 
?
0
1
2
3
4
5
6
7
	8

9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
 31
!32
"33
#34
$35*

?0
?1*
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
LF
VARIABLE_VALUE	Adam/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE*
PJ
VARIABLE_VALUEAdam/beta_1+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUE*
PJ
VARIABLE_VALUEAdam/beta_2+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUE*
NH
VARIABLE_VALUE
Adam/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE*
^X
VARIABLE_VALUEAdam/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE*
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
	
}0* 
* 
* 
* 
* 
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 


?0* 
* 
* 
* 
* 
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 


?0* 
* 
* 
* 
* 
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 


?0* 
* 
* 
* 
* 
* 
* 
* 
* 
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
<
?	variables
?	keras_api

?total

?count*
M
?	variables
?	keras_api

?total

?count
?
_fn_kwargs*
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 

?0
?1*

?	variables*
UO
VARIABLE_VALUEtotal_14keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE*
UO
VARIABLE_VALUEcount_14keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE*

?0
?1*

?	variables*
SM
VARIABLE_VALUEtotal4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE*
SM
VARIABLE_VALUEcount4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE*
* 
??
VARIABLE_VALUEAdam/embedding/embeddings/mVlayer_with_weights-0/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_1/embeddings/mVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_2/embeddings/mVlayer_with_weights-2/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_3/embeddings/mVlayer_with_weights-3/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_4/embeddings/mVlayer_with_weights-4/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
y
VARIABLE_VALUEAdam/dense/kernel/mRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
{u
VARIABLE_VALUEAdam/dense/bias/mPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUE Adam/layer_normalization/gamma/mQlayer_with_weights-6/gamma/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/layer_normalization/beta/mPlayer_with_weights-6/beta/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?{
VARIABLE_VALUEAdam/dense_1/kernel/mRlayer_with_weights-7/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_1/bias/mPlayer_with_weights-7/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?{
VARIABLE_VALUEAdam/dense_2/kernel/mRlayer_with_weights-8/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_2/bias/mPlayer_with_weights-8/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?{
VARIABLE_VALUEAdam/dense_3/kernel/mRlayer_with_weights-9/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_3/bias/mPlayer_with_weights-9/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_4/kernel/mSlayer_with_weights-10/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_4/bias/mQlayer_with_weights-10/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_5/kernel/mSlayer_with_weights-11/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_5/bias/mQlayer_with_weights-11/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_6/kernel/mSlayer_with_weights-12/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_6/bias/mQlayer_with_weights-12/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_7/kernel/mSlayer_with_weights-13/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_7/bias/mQlayer_with_weights-13/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_8/kernel/mSlayer_with_weights-14/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_8/bias/mQlayer_with_weights-14/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUE$Adam/readmission_prediction/kernel/mSlayer_with_weights-15/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUE"Adam/readmission_prediction/bias/mQlayer_with_weights-15/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding/embeddings/vVlayer_with_weights-0/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_1/embeddings/vVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_2/embeddings/vVlayer_with_weights-2/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_3/embeddings/vVlayer_with_weights-3/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/embedding_4/embeddings/vVlayer_with_weights-4/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
y
VARIABLE_VALUEAdam/dense/kernel/vRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
{u
VARIABLE_VALUEAdam/dense/bias/vPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUE Adam/layer_normalization/gamma/vQlayer_with_weights-6/gamma/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUEAdam/layer_normalization/beta/vPlayer_with_weights-6/beta/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?{
VARIABLE_VALUEAdam/dense_1/kernel/vRlayer_with_weights-7/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_1/bias/vPlayer_with_weights-7/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?{
VARIABLE_VALUEAdam/dense_2/kernel/vRlayer_with_weights-8/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_2/bias/vPlayer_with_weights-8/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?{
VARIABLE_VALUEAdam/dense_3/kernel/vRlayer_with_weights-9/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_3/bias/vPlayer_with_weights-9/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_4/kernel/vSlayer_with_weights-10/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_4/bias/vQlayer_with_weights-10/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_5/kernel/vSlayer_with_weights-11/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_5/bias/vQlayer_with_weights-11/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_6/kernel/vSlayer_with_weights-12/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_6/bias/vQlayer_with_weights-12/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_7/kernel/vSlayer_with_weights-13/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_7/bias/vQlayer_with_weights-13/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
?|
VARIABLE_VALUEAdam/dense_8/kernel/vSlayer_with_weights-14/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
~x
VARIABLE_VALUEAdam/dense_8/bias/vQlayer_with_weights-14/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUE$Adam/readmission_prediction/kernel/vSlayer_with_weights-15/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
??
VARIABLE_VALUE"Adam/readmission_prediction/bias/vQlayer_with_weights-15/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
? 
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename(embedding/embeddings/Read/ReadVariableOp*embedding_1/embeddings/Read/ReadVariableOp*embedding_2/embeddings/Read/ReadVariableOp*embedding_3/embeddings/Read/ReadVariableOp*embedding_4/embeddings/Read/ReadVariableOp dense/kernel/Read/ReadVariableOpdense/bias/Read/ReadVariableOp-layer_normalization/gamma/Read/ReadVariableOp,layer_normalization/beta/Read/ReadVariableOp"dense_1/kernel/Read/ReadVariableOp dense_1/bias/Read/ReadVariableOp"dense_2/kernel/Read/ReadVariableOp dense_2/bias/Read/ReadVariableOp"dense_3/kernel/Read/ReadVariableOp dense_3/bias/Read/ReadVariableOp"dense_4/kernel/Read/ReadVariableOp dense_4/bias/Read/ReadVariableOp"dense_5/kernel/Read/ReadVariableOp dense_5/bias/Read/ReadVariableOp"dense_6/kernel/Read/ReadVariableOp dense_6/bias/Read/ReadVariableOp"dense_7/kernel/Read/ReadVariableOp dense_7/bias/Read/ReadVariableOp"dense_8/kernel/Read/ReadVariableOp dense_8/bias/Read/ReadVariableOp1readmission_prediction/kernel/Read/ReadVariableOp/readmission_prediction/bias/Read/ReadVariableOpAdam/iter/Read/ReadVariableOpAdam/beta_1/Read/ReadVariableOpAdam/beta_2/Read/ReadVariableOpAdam/decay/Read/ReadVariableOp&Adam/learning_rate/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOp/Adam/embedding/embeddings/m/Read/ReadVariableOp1Adam/embedding_1/embeddings/m/Read/ReadVariableOp1Adam/embedding_2/embeddings/m/Read/ReadVariableOp1Adam/embedding_3/embeddings/m/Read/ReadVariableOp1Adam/embedding_4/embeddings/m/Read/ReadVariableOp'Adam/dense/kernel/m/Read/ReadVariableOp%Adam/dense/bias/m/Read/ReadVariableOp4Adam/layer_normalization/gamma/m/Read/ReadVariableOp3Adam/layer_normalization/beta/m/Read/ReadVariableOp)Adam/dense_1/kernel/m/Read/ReadVariableOp'Adam/dense_1/bias/m/Read/ReadVariableOp)Adam/dense_2/kernel/m/Read/ReadVariableOp'Adam/dense_2/bias/m/Read/ReadVariableOp)Adam/dense_3/kernel/m/Read/ReadVariableOp'Adam/dense_3/bias/m/Read/ReadVariableOp)Adam/dense_4/kernel/m/Read/ReadVariableOp'Adam/dense_4/bias/m/Read/ReadVariableOp)Adam/dense_5/kernel/m/Read/ReadVariableOp'Adam/dense_5/bias/m/Read/ReadVariableOp)Adam/dense_6/kernel/m/Read/ReadVariableOp'Adam/dense_6/bias/m/Read/ReadVariableOp)Adam/dense_7/kernel/m/Read/ReadVariableOp'Adam/dense_7/bias/m/Read/ReadVariableOp)Adam/dense_8/kernel/m/Read/ReadVariableOp'Adam/dense_8/bias/m/Read/ReadVariableOp8Adam/readmission_prediction/kernel/m/Read/ReadVariableOp6Adam/readmission_prediction/bias/m/Read/ReadVariableOp/Adam/embedding/embeddings/v/Read/ReadVariableOp1Adam/embedding_1/embeddings/v/Read/ReadVariableOp1Adam/embedding_2/embeddings/v/Read/ReadVariableOp1Adam/embedding_3/embeddings/v/Read/ReadVariableOp1Adam/embedding_4/embeddings/v/Read/ReadVariableOp'Adam/dense/kernel/v/Read/ReadVariableOp%Adam/dense/bias/v/Read/ReadVariableOp4Adam/layer_normalization/gamma/v/Read/ReadVariableOp3Adam/layer_normalization/beta/v/Read/ReadVariableOp)Adam/dense_1/kernel/v/Read/ReadVariableOp'Adam/dense_1/bias/v/Read/ReadVariableOp)Adam/dense_2/kernel/v/Read/ReadVariableOp'Adam/dense_2/bias/v/Read/ReadVariableOp)Adam/dense_3/kernel/v/Read/ReadVariableOp'Adam/dense_3/bias/v/Read/ReadVariableOp)Adam/dense_4/kernel/v/Read/ReadVariableOp'Adam/dense_4/bias/v/Read/ReadVariableOp)Adam/dense_5/kernel/v/Read/ReadVariableOp'Adam/dense_5/bias/v/Read/ReadVariableOp)Adam/dense_6/kernel/v/Read/ReadVariableOp'Adam/dense_6/bias/v/Read/ReadVariableOp)Adam/dense_7/kernel/v/Read/ReadVariableOp'Adam/dense_7/bias/v/Read/ReadVariableOp)Adam/dense_8/kernel/v/Read/ReadVariableOp'Adam/dense_8/bias/v/Read/ReadVariableOp8Adam/readmission_prediction/kernel/v/Read/ReadVariableOp6Adam/readmission_prediction/bias/v/Read/ReadVariableOpConst*g
Tin`
^2\	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *(
f#R!
__inference__traced_save_742716
?
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenameembedding/embeddingsembedding_1/embeddingsembedding_2/embeddingsembedding_3/embeddingsembedding_4/embeddingsdense/kernel
dense/biaslayer_normalization/gammalayer_normalization/betadense_1/kerneldense_1/biasdense_2/kerneldense_2/biasdense_3/kerneldense_3/biasdense_4/kerneldense_4/biasdense_5/kerneldense_5/biasdense_6/kerneldense_6/biasdense_7/kerneldense_7/biasdense_8/kerneldense_8/biasreadmission_prediction/kernelreadmission_prediction/bias	Adam/iterAdam/beta_1Adam/beta_2
Adam/decayAdam/learning_ratetotal_1count_1totalcountAdam/embedding/embeddings/mAdam/embedding_1/embeddings/mAdam/embedding_2/embeddings/mAdam/embedding_3/embeddings/mAdam/embedding_4/embeddings/mAdam/dense/kernel/mAdam/dense/bias/m Adam/layer_normalization/gamma/mAdam/layer_normalization/beta/mAdam/dense_1/kernel/mAdam/dense_1/bias/mAdam/dense_2/kernel/mAdam/dense_2/bias/mAdam/dense_3/kernel/mAdam/dense_3/bias/mAdam/dense_4/kernel/mAdam/dense_4/bias/mAdam/dense_5/kernel/mAdam/dense_5/bias/mAdam/dense_6/kernel/mAdam/dense_6/bias/mAdam/dense_7/kernel/mAdam/dense_7/bias/mAdam/dense_8/kernel/mAdam/dense_8/bias/m$Adam/readmission_prediction/kernel/m"Adam/readmission_prediction/bias/mAdam/embedding/embeddings/vAdam/embedding_1/embeddings/vAdam/embedding_2/embeddings/vAdam/embedding_3/embeddings/vAdam/embedding_4/embeddings/vAdam/dense/kernel/vAdam/dense/bias/v Adam/layer_normalization/gamma/vAdam/layer_normalization/beta/vAdam/dense_1/kernel/vAdam/dense_1/bias/vAdam/dense_2/kernel/vAdam/dense_2/bias/vAdam/dense_3/kernel/vAdam/dense_3/bias/vAdam/dense_4/kernel/vAdam/dense_4/bias/vAdam/dense_5/kernel/vAdam/dense_5/bias/vAdam/dense_6/kernel/vAdam/dense_6/bias/vAdam/dense_7/kernel/vAdam/dense_7/bias/vAdam/dense_8/kernel/vAdam/dense_8/bias/v$Adam/readmission_prediction/kernel/v"Adam/readmission_prediction/bias/v*f
Tin_
]2[*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *+
f&R$
"__inference__traced_restore_742996??
?
c
E__inference_dropout_4_layer_call_and_return_conditional_losses_742385

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
F
*__inference_dropout_3_layer_call_fn_742309

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_3_layer_call_and_return_conditional_losses_740503a
IdentityIdentityPartitionedCall:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
G__inference_embedding_3_layer_call_and_return_conditional_losses_741986

inputs)
embedding_lookup_741980:
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_741980inputs*
Tindices0**
_class 
loc:@embedding_lookup/741980*+
_output_shapes
:?????????*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/741980*+
_output_shapes
:??????????
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
??
?&
__inference__traced_save_742716
file_prefix3
/savev2_embedding_embeddings_read_readvariableop5
1savev2_embedding_1_embeddings_read_readvariableop5
1savev2_embedding_2_embeddings_read_readvariableop5
1savev2_embedding_3_embeddings_read_readvariableop5
1savev2_embedding_4_embeddings_read_readvariableop+
'savev2_dense_kernel_read_readvariableop)
%savev2_dense_bias_read_readvariableop8
4savev2_layer_normalization_gamma_read_readvariableop7
3savev2_layer_normalization_beta_read_readvariableop-
)savev2_dense_1_kernel_read_readvariableop+
'savev2_dense_1_bias_read_readvariableop-
)savev2_dense_2_kernel_read_readvariableop+
'savev2_dense_2_bias_read_readvariableop-
)savev2_dense_3_kernel_read_readvariableop+
'savev2_dense_3_bias_read_readvariableop-
)savev2_dense_4_kernel_read_readvariableop+
'savev2_dense_4_bias_read_readvariableop-
)savev2_dense_5_kernel_read_readvariableop+
'savev2_dense_5_bias_read_readvariableop-
)savev2_dense_6_kernel_read_readvariableop+
'savev2_dense_6_bias_read_readvariableop-
)savev2_dense_7_kernel_read_readvariableop+
'savev2_dense_7_bias_read_readvariableop-
)savev2_dense_8_kernel_read_readvariableop+
'savev2_dense_8_bias_read_readvariableop<
8savev2_readmission_prediction_kernel_read_readvariableop:
6savev2_readmission_prediction_bias_read_readvariableop(
$savev2_adam_iter_read_readvariableop	*
&savev2_adam_beta_1_read_readvariableop*
&savev2_adam_beta_2_read_readvariableop)
%savev2_adam_decay_read_readvariableop1
-savev2_adam_learning_rate_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop:
6savev2_adam_embedding_embeddings_m_read_readvariableop<
8savev2_adam_embedding_1_embeddings_m_read_readvariableop<
8savev2_adam_embedding_2_embeddings_m_read_readvariableop<
8savev2_adam_embedding_3_embeddings_m_read_readvariableop<
8savev2_adam_embedding_4_embeddings_m_read_readvariableop2
.savev2_adam_dense_kernel_m_read_readvariableop0
,savev2_adam_dense_bias_m_read_readvariableop?
;savev2_adam_layer_normalization_gamma_m_read_readvariableop>
:savev2_adam_layer_normalization_beta_m_read_readvariableop4
0savev2_adam_dense_1_kernel_m_read_readvariableop2
.savev2_adam_dense_1_bias_m_read_readvariableop4
0savev2_adam_dense_2_kernel_m_read_readvariableop2
.savev2_adam_dense_2_bias_m_read_readvariableop4
0savev2_adam_dense_3_kernel_m_read_readvariableop2
.savev2_adam_dense_3_bias_m_read_readvariableop4
0savev2_adam_dense_4_kernel_m_read_readvariableop2
.savev2_adam_dense_4_bias_m_read_readvariableop4
0savev2_adam_dense_5_kernel_m_read_readvariableop2
.savev2_adam_dense_5_bias_m_read_readvariableop4
0savev2_adam_dense_6_kernel_m_read_readvariableop2
.savev2_adam_dense_6_bias_m_read_readvariableop4
0savev2_adam_dense_7_kernel_m_read_readvariableop2
.savev2_adam_dense_7_bias_m_read_readvariableop4
0savev2_adam_dense_8_kernel_m_read_readvariableop2
.savev2_adam_dense_8_bias_m_read_readvariableopC
?savev2_adam_readmission_prediction_kernel_m_read_readvariableopA
=savev2_adam_readmission_prediction_bias_m_read_readvariableop:
6savev2_adam_embedding_embeddings_v_read_readvariableop<
8savev2_adam_embedding_1_embeddings_v_read_readvariableop<
8savev2_adam_embedding_2_embeddings_v_read_readvariableop<
8savev2_adam_embedding_3_embeddings_v_read_readvariableop<
8savev2_adam_embedding_4_embeddings_v_read_readvariableop2
.savev2_adam_dense_kernel_v_read_readvariableop0
,savev2_adam_dense_bias_v_read_readvariableop?
;savev2_adam_layer_normalization_gamma_v_read_readvariableop>
:savev2_adam_layer_normalization_beta_v_read_readvariableop4
0savev2_adam_dense_1_kernel_v_read_readvariableop2
.savev2_adam_dense_1_bias_v_read_readvariableop4
0savev2_adam_dense_2_kernel_v_read_readvariableop2
.savev2_adam_dense_2_bias_v_read_readvariableop4
0savev2_adam_dense_3_kernel_v_read_readvariableop2
.savev2_adam_dense_3_bias_v_read_readvariableop4
0savev2_adam_dense_4_kernel_v_read_readvariableop2
.savev2_adam_dense_4_bias_v_read_readvariableop4
0savev2_adam_dense_5_kernel_v_read_readvariableop2
.savev2_adam_dense_5_bias_v_read_readvariableop4
0savev2_adam_dense_6_kernel_v_read_readvariableop2
.savev2_adam_dense_6_bias_v_read_readvariableop4
0savev2_adam_dense_7_kernel_v_read_readvariableop2
.savev2_adam_dense_7_bias_v_read_readvariableop4
0savev2_adam_dense_8_kernel_v_read_readvariableop2
.savev2_adam_dense_8_bias_v_read_readvariableopC
?savev2_adam_readmission_prediction_kernel_v_read_readvariableopA
=savev2_adam_readmission_prediction_bias_v_read_readvariableop
savev2_const

identity_1??MergeV2Checkpointsw
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*Z
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.parta
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: f

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: L

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :f
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : ?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: ?4
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:[*
dtype0*?3
value?3B?3[B:layer_with_weights-0/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-2/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-3/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-4/embeddings/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-5/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-5/bias/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-6/gamma/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-6/beta/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-7/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-7/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-8/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-8/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-9/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-9/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-10/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-10/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-11/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-11/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-12/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-12/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-13/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-13/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-14/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-14/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-15/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-15/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-0/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-2/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-3/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-4/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-6/gamma/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/beta/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-7/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-7/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-8/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-8/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-9/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-9/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-10/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-10/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-11/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-11/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-12/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-12/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-13/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-13/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-14/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-14/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-15/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-15/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-0/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-2/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-3/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-4/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-6/gamma/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/beta/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-7/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-7/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-8/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-8/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-9/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-9/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-10/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-10/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-11/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-11/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-12/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-12/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-13/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-13/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-14/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-14/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-15/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-15/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:[*
dtype0*?
value?B?[B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B ?$
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0/savev2_embedding_embeddings_read_readvariableop1savev2_embedding_1_embeddings_read_readvariableop1savev2_embedding_2_embeddings_read_readvariableop1savev2_embedding_3_embeddings_read_readvariableop1savev2_embedding_4_embeddings_read_readvariableop'savev2_dense_kernel_read_readvariableop%savev2_dense_bias_read_readvariableop4savev2_layer_normalization_gamma_read_readvariableop3savev2_layer_normalization_beta_read_readvariableop)savev2_dense_1_kernel_read_readvariableop'savev2_dense_1_bias_read_readvariableop)savev2_dense_2_kernel_read_readvariableop'savev2_dense_2_bias_read_readvariableop)savev2_dense_3_kernel_read_readvariableop'savev2_dense_3_bias_read_readvariableop)savev2_dense_4_kernel_read_readvariableop'savev2_dense_4_bias_read_readvariableop)savev2_dense_5_kernel_read_readvariableop'savev2_dense_5_bias_read_readvariableop)savev2_dense_6_kernel_read_readvariableop'savev2_dense_6_bias_read_readvariableop)savev2_dense_7_kernel_read_readvariableop'savev2_dense_7_bias_read_readvariableop)savev2_dense_8_kernel_read_readvariableop'savev2_dense_8_bias_read_readvariableop8savev2_readmission_prediction_kernel_read_readvariableop6savev2_readmission_prediction_bias_read_readvariableop$savev2_adam_iter_read_readvariableop&savev2_adam_beta_1_read_readvariableop&savev2_adam_beta_2_read_readvariableop%savev2_adam_decay_read_readvariableop-savev2_adam_learning_rate_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop6savev2_adam_embedding_embeddings_m_read_readvariableop8savev2_adam_embedding_1_embeddings_m_read_readvariableop8savev2_adam_embedding_2_embeddings_m_read_readvariableop8savev2_adam_embedding_3_embeddings_m_read_readvariableop8savev2_adam_embedding_4_embeddings_m_read_readvariableop.savev2_adam_dense_kernel_m_read_readvariableop,savev2_adam_dense_bias_m_read_readvariableop;savev2_adam_layer_normalization_gamma_m_read_readvariableop:savev2_adam_layer_normalization_beta_m_read_readvariableop0savev2_adam_dense_1_kernel_m_read_readvariableop.savev2_adam_dense_1_bias_m_read_readvariableop0savev2_adam_dense_2_kernel_m_read_readvariableop.savev2_adam_dense_2_bias_m_read_readvariableop0savev2_adam_dense_3_kernel_m_read_readvariableop.savev2_adam_dense_3_bias_m_read_readvariableop0savev2_adam_dense_4_kernel_m_read_readvariableop.savev2_adam_dense_4_bias_m_read_readvariableop0savev2_adam_dense_5_kernel_m_read_readvariableop.savev2_adam_dense_5_bias_m_read_readvariableop0savev2_adam_dense_6_kernel_m_read_readvariableop.savev2_adam_dense_6_bias_m_read_readvariableop0savev2_adam_dense_7_kernel_m_read_readvariableop.savev2_adam_dense_7_bias_m_read_readvariableop0savev2_adam_dense_8_kernel_m_read_readvariableop.savev2_adam_dense_8_bias_m_read_readvariableop?savev2_adam_readmission_prediction_kernel_m_read_readvariableop=savev2_adam_readmission_prediction_bias_m_read_readvariableop6savev2_adam_embedding_embeddings_v_read_readvariableop8savev2_adam_embedding_1_embeddings_v_read_readvariableop8savev2_adam_embedding_2_embeddings_v_read_readvariableop8savev2_adam_embedding_3_embeddings_v_read_readvariableop8savev2_adam_embedding_4_embeddings_v_read_readvariableop.savev2_adam_dense_kernel_v_read_readvariableop,savev2_adam_dense_bias_v_read_readvariableop;savev2_adam_layer_normalization_gamma_v_read_readvariableop:savev2_adam_layer_normalization_beta_v_read_readvariableop0savev2_adam_dense_1_kernel_v_read_readvariableop.savev2_adam_dense_1_bias_v_read_readvariableop0savev2_adam_dense_2_kernel_v_read_readvariableop.savev2_adam_dense_2_bias_v_read_readvariableop0savev2_adam_dense_3_kernel_v_read_readvariableop.savev2_adam_dense_3_bias_v_read_readvariableop0savev2_adam_dense_4_kernel_v_read_readvariableop.savev2_adam_dense_4_bias_v_read_readvariableop0savev2_adam_dense_5_kernel_v_read_readvariableop.savev2_adam_dense_5_bias_v_read_readvariableop0savev2_adam_dense_6_kernel_v_read_readvariableop.savev2_adam_dense_6_bias_v_read_readvariableop0savev2_adam_dense_7_kernel_v_read_readvariableop.savev2_adam_dense_7_bias_v_read_readvariableop0savev2_adam_dense_8_kernel_v_read_readvariableop.savev2_adam_dense_8_bias_v_read_readvariableop?savev2_adam_readmission_prediction_kernel_v_read_readvariableop=savev2_adam_readmission_prediction_bias_v_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *i
dtypes_
]2[	?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 f
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: Q

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: [
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 "!

identity_1Identity_1:output:0*?
_input_shapes?
?: ::

:::	?<@:jj:j:j:j:	j?:?:	j?:?:
??:?:	j?:?:
??:?:	j?:?:
??:?:	j?:?:	?:: : : : : : : : : ::

:::	?<@:jj:j:j:j:	j?:?:	j?:?:
??:?:	j?:?:
??:?:	j?:?:
??:?:	j?:?:	?:::

:::	?<@:jj:j:j:j:	j?:?:	j?:?:
??:?:	j?:?:
??:?:	j?:?:
??:?:	j?:?:	?:: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

::$ 

_output_shapes

:

:$ 

_output_shapes

::$ 

_output_shapes

::%!

_output_shapes
:	?<@:$ 

_output_shapes

:jj: 

_output_shapes
:j: 

_output_shapes
:j: 	

_output_shapes
:j:%
!

_output_shapes
:	j?:!

_output_shapes	
:?:%!

_output_shapes
:	j?:!

_output_shapes	
:?:&"
 
_output_shapes
:
??:!

_output_shapes	
:?:%!

_output_shapes
:	j?:!

_output_shapes	
:?:&"
 
_output_shapes
:
??:!

_output_shapes	
:?:%!

_output_shapes
:	j?:!

_output_shapes	
:?:&"
 
_output_shapes
:
??:!

_output_shapes	
:?:%!

_output_shapes
:	j?:!

_output_shapes	
:?:%!

_output_shapes
:	?: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: : 

_output_shapes
: :!

_output_shapes
: :"

_output_shapes
: :#

_output_shapes
: :$

_output_shapes
: :$% 

_output_shapes

::$& 

_output_shapes

:

:$' 

_output_shapes

::$( 

_output_shapes

::%)!

_output_shapes
:	?<@:$* 

_output_shapes

:jj: +

_output_shapes
:j: ,

_output_shapes
:j: -

_output_shapes
:j:%.!

_output_shapes
:	j?:!/

_output_shapes	
:?:%0!

_output_shapes
:	j?:!1

_output_shapes	
:?:&2"
 
_output_shapes
:
??:!3

_output_shapes	
:?:%4!

_output_shapes
:	j?:!5

_output_shapes	
:?:&6"
 
_output_shapes
:
??:!7

_output_shapes	
:?:%8!

_output_shapes
:	j?:!9

_output_shapes	
:?:&:"
 
_output_shapes
:
??:!;

_output_shapes	
:?:%<!

_output_shapes
:	j?:!=

_output_shapes	
:?:%>!

_output_shapes
:	?: ?

_output_shapes
::$@ 

_output_shapes

::$A 

_output_shapes

:

:$B 

_output_shapes

::$C 

_output_shapes

::%D!

_output_shapes
:	?<@:$E 

_output_shapes

:jj: F

_output_shapes
:j: G

_output_shapes
:j: H

_output_shapes
:j:%I!

_output_shapes
:	j?:!J

_output_shapes	
:?:%K!

_output_shapes
:	j?:!L

_output_shapes	
:?:&M"
 
_output_shapes
:
??:!N

_output_shapes	
:?:%O!

_output_shapes
:	j?:!P

_output_shapes	
:?:&Q"
 
_output_shapes
:
??:!R

_output_shapes	
:?:%S!

_output_shapes
:	j?:!T

_output_shapes	
:?:&U"
 
_output_shapes
:
??:!V

_output_shapes	
:?:%W!

_output_shapes
:	j?:!X

_output_shapes	
:?:%Y!

_output_shapes
:	?: Z

_output_shapes
::[

_output_shapes
: 
?
?
4__inference_layer_normalization_layer_call_fn_742064

inputs
unknown:j
	unknown_0:j
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *X
fSRQ
O__inference_layer_normalization_layer_call_and_return_conditional_losses_740369o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????j`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
_
C__inference_flatten_layer_call_and_return_conditional_losses_742036

inputs
identityV
ConstConst*
_output_shapes
:*
dtype0*
valueB"????j   \
ReshapeReshapeinputsConst:output:0*
T0*'
_output_shapes
:?????????jX
IdentityIdentityReshape:output:0*
T0*'
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j:S O
+
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
d
E__inference_dropout_1_layer_call_and_return_conditional_losses_740780

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
F
*__inference_dropout_4_layer_call_fn_742375

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_4_layer_call_and_return_conditional_losses_740544a
IdentityIdentityPartitionedCall:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
?
C__inference_dense_2_layer_call_and_return_conditional_losses_740409

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
?
C__inference_dense_4_layer_call_and_return_conditional_losses_740450

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?

?
C__inference_dense_3_layer_call_and_return_conditional_losses_742219

inputs2
matmul_readvariableop_resource:
??.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????p
leaky_re_lu_1/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>u
IdentityIdentity%leaky_re_lu_1/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
(__inference_dense_8_layer_call_fn_742360

inputs
unknown:	j?
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_8_layer_call_and_return_conditional_losses_740532p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
G__inference_embedding_1_layer_call_and_return_conditional_losses_740246

inputs)
embedding_lookup_740240:


identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_740240inputs*
Tindices0**
_class 
loc:@embedding_lookup/740240*+
_output_shapes
:?????????
*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/740240*+
_output_shapes
:?????????
?
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????
w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????
Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
a
C__inference_dropout_layer_call_and_return_conditional_losses_740380

inputs

identity_1N
IdentityIdentityinputs*
T0*'
_output_shapes
:?????????j[

Identity_1IdentityIdentity:output:0*
T0*'
_output_shapes
:?????????j"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????j:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
,__inference_concatenate_layer_call_fn_742013
inputs_0
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
inputs_6
identity?
PartitionedCallPartitionedCallinputs_0inputs_1inputs_2inputs_3inputs_4inputs_5inputs_6*
Tin
	2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_concatenate_layer_call_and_return_conditional_losses_740301d
IdentityIdentityPartitionedCall:output:0*
T0*+
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????
:?????????:?????????:?????????@:U Q
+
_output_shapes
:?????????
"
_user_specified_name
inputs/0:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/1:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/2:UQ
+
_output_shapes
:?????????

"
_user_specified_name
inputs/3:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/4:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/5:UQ
+
_output_shapes
:?????????@
"
_user_specified_name
inputs/6
??
?
!__inference__wrapped_model_740203
drg
	age_group
bed_day_dep
degree_of_urgency
diagnosis_code_1

gender
maindiagicd10chapter:
(i_fall_embedding_embedding_lookup_740061:<
*i_fall_embedding_1_embedding_lookup_740066:

<
*i_fall_embedding_2_embedding_lookup_740071:<
*i_fall_embedding_3_embedding_lookup_740076:=
*i_fall_embedding_4_embedding_lookup_740081:	?<@=
+i_fall_dense_matmul_readvariableop_resource:jj:
,i_fall_dense_biasadd_readvariableop_resource:jF
8i_fall_layer_normalization_mul_2_readvariableop_resource:jD
6i_fall_layer_normalization_add_readvariableop_resource:j@
-i_fall_dense_1_matmul_readvariableop_resource:	j?=
.i_fall_dense_1_biasadd_readvariableop_resource:	?@
-i_fall_dense_2_matmul_readvariableop_resource:	j?=
.i_fall_dense_2_biasadd_readvariableop_resource:	?A
-i_fall_dense_3_matmul_readvariableop_resource:
??=
.i_fall_dense_3_biasadd_readvariableop_resource:	?@
-i_fall_dense_4_matmul_readvariableop_resource:	j?=
.i_fall_dense_4_biasadd_readvariableop_resource:	?A
-i_fall_dense_5_matmul_readvariableop_resource:
??=
.i_fall_dense_5_biasadd_readvariableop_resource:	?@
-i_fall_dense_6_matmul_readvariableop_resource:	j?=
.i_fall_dense_6_biasadd_readvariableop_resource:	?A
-i_fall_dense_7_matmul_readvariableop_resource:
??=
.i_fall_dense_7_biasadd_readvariableop_resource:	?@
-i_fall_dense_8_matmul_readvariableop_resource:	j?=
.i_fall_dense_8_biasadd_readvariableop_resource:	?O
<i_fall_readmission_prediction_matmul_readvariableop_resource:	?K
=i_fall_readmission_prediction_biasadd_readvariableop_resource:
identity??#i_fall/dense/BiasAdd/ReadVariableOp?"i_fall/dense/MatMul/ReadVariableOp?%i_fall/dense_1/BiasAdd/ReadVariableOp?$i_fall/dense_1/MatMul/ReadVariableOp?%i_fall/dense_2/BiasAdd/ReadVariableOp?$i_fall/dense_2/MatMul/ReadVariableOp?%i_fall/dense_3/BiasAdd/ReadVariableOp?$i_fall/dense_3/MatMul/ReadVariableOp?%i_fall/dense_4/BiasAdd/ReadVariableOp?$i_fall/dense_4/MatMul/ReadVariableOp?%i_fall/dense_5/BiasAdd/ReadVariableOp?$i_fall/dense_5/MatMul/ReadVariableOp?%i_fall/dense_6/BiasAdd/ReadVariableOp?$i_fall/dense_6/MatMul/ReadVariableOp?%i_fall/dense_7/BiasAdd/ReadVariableOp?$i_fall/dense_7/MatMul/ReadVariableOp?%i_fall/dense_8/BiasAdd/ReadVariableOp?$i_fall/dense_8/MatMul/ReadVariableOp?!i_fall/embedding/embedding_lookup?#i_fall/embedding_1/embedding_lookup?#i_fall/embedding_2/embedding_lookup?#i_fall/embedding_3/embedding_lookup?#i_fall/embedding_4/embedding_lookup?-i_fall/layer_normalization/add/ReadVariableOp?/i_fall/layer_normalization/mul_2/ReadVariableOp?4i_fall/readmission_prediction/BiasAdd/ReadVariableOp?3i_fall/readmission_prediction/MatMul/ReadVariableOpu
i_fall/tf.stack/stackPackbed_day_dep*
N*
T0*+
_output_shapes
:?????????*

axiso
i_fall/tf.stack_1/stackPackdrg*
N*
T0*+
_output_shapes
:?????????*

axis?
!i_fall/embedding/embedding_lookupResourceGather(i_fall_embedding_embedding_lookup_740061gender*
Tindices0*;
_class1
/-loc:@i_fall/embedding/embedding_lookup/740061*+
_output_shapes
:?????????*
dtype0?
*i_fall/embedding/embedding_lookup/IdentityIdentity*i_fall/embedding/embedding_lookup:output:0*
T0*;
_class1
/-loc:@i_fall/embedding/embedding_lookup/740061*+
_output_shapes
:??????????
,i_fall/embedding/embedding_lookup/Identity_1Identity3i_fall/embedding/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
#i_fall/embedding_1/embedding_lookupResourceGather*i_fall_embedding_1_embedding_lookup_740066	age_group*
Tindices0*=
_class3
1/loc:@i_fall/embedding_1/embedding_lookup/740066*+
_output_shapes
:?????????
*
dtype0?
,i_fall/embedding_1/embedding_lookup/IdentityIdentity,i_fall/embedding_1/embedding_lookup:output:0*
T0*=
_class3
1/loc:@i_fall/embedding_1/embedding_lookup/740066*+
_output_shapes
:?????????
?
.i_fall/embedding_1/embedding_lookup/Identity_1Identity5i_fall/embedding_1/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????
?
#i_fall/embedding_2/embedding_lookupResourceGather*i_fall_embedding_2_embedding_lookup_740071degree_of_urgency*
Tindices0*=
_class3
1/loc:@i_fall/embedding_2/embedding_lookup/740071*+
_output_shapes
:?????????*
dtype0?
,i_fall/embedding_2/embedding_lookup/IdentityIdentity,i_fall/embedding_2/embedding_lookup:output:0*
T0*=
_class3
1/loc:@i_fall/embedding_2/embedding_lookup/740071*+
_output_shapes
:??????????
.i_fall/embedding_2/embedding_lookup/Identity_1Identity5i_fall/embedding_2/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
#i_fall/embedding_3/embedding_lookupResourceGather*i_fall_embedding_3_embedding_lookup_740076maindiagicd10chapter*
Tindices0*=
_class3
1/loc:@i_fall/embedding_3/embedding_lookup/740076*+
_output_shapes
:?????????*
dtype0?
,i_fall/embedding_3/embedding_lookup/IdentityIdentity,i_fall/embedding_3/embedding_lookup:output:0*
T0*=
_class3
1/loc:@i_fall/embedding_3/embedding_lookup/740076*+
_output_shapes
:??????????
.i_fall/embedding_3/embedding_lookup/Identity_1Identity5i_fall/embedding_3/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
#i_fall/embedding_4/embedding_lookupResourceGather*i_fall_embedding_4_embedding_lookup_740081diagnosis_code_1*
Tindices0*=
_class3
1/loc:@i_fall/embedding_4/embedding_lookup/740081*+
_output_shapes
:?????????@*
dtype0?
,i_fall/embedding_4/embedding_lookup/IdentityIdentity,i_fall/embedding_4/embedding_lookup:output:0*
T0*=
_class3
1/loc:@i_fall/embedding_4/embedding_lookup/740081*+
_output_shapes
:?????????@?
.i_fall/embedding_4/embedding_lookup/Identity_1Identity5i_fall/embedding_4/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????@`
i_fall/concatenate/concat/axisConst*
_output_shapes
: *
dtype0*
value	B :?
i_fall/concatenate/concatConcatV2i_fall/tf.stack/stack:output:0 i_fall/tf.stack_1/stack:output:05i_fall/embedding/embedding_lookup/Identity_1:output:07i_fall/embedding_1/embedding_lookup/Identity_1:output:07i_fall/embedding_2/embedding_lookup/Identity_1:output:07i_fall/embedding_3/embedding_lookup/Identity_1:output:07i_fall/embedding_4/embedding_lookup/Identity_1:output:0'i_fall/concatenate/concat/axis:output:0*
N*
T0*+
_output_shapes
:?????????je
i_fall/flatten/ConstConst*
_output_shapes
:*
dtype0*
valueB"????j   ?
i_fall/flatten/ReshapeReshape"i_fall/concatenate/concat:output:0i_fall/flatten/Const:output:0*
T0*'
_output_shapes
:?????????j?
"i_fall/dense/MatMul/ReadVariableOpReadVariableOp+i_fall_dense_matmul_readvariableop_resource*
_output_shapes

:jj*
dtype0?
i_fall/dense/MatMulMatMuli_fall/flatten/Reshape:output:0*i_fall/dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j?
#i_fall/dense/BiasAdd/ReadVariableOpReadVariableOp,i_fall_dense_biasadd_readvariableop_resource*
_output_shapes
:j*
dtype0?
i_fall/dense/BiasAddBiasAddi_fall/dense/MatMul:product:0+i_fall/dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jm
 i_fall/layer_normalization/ShapeShapei_fall/dense/BiasAdd:output:0*
T0*
_output_shapes
:x
.i_fall/layer_normalization/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: z
0i_fall/layer_normalization/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:z
0i_fall/layer_normalization/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
(i_fall/layer_normalization/strided_sliceStridedSlice)i_fall/layer_normalization/Shape:output:07i_fall/layer_normalization/strided_slice/stack:output:09i_fall/layer_normalization/strided_slice/stack_1:output:09i_fall/layer_normalization/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskb
 i_fall/layer_normalization/mul/xConst*
_output_shapes
: *
dtype0*
value	B :?
i_fall/layer_normalization/mulMul)i_fall/layer_normalization/mul/x:output:01i_fall/layer_normalization/strided_slice:output:0*
T0*
_output_shapes
: z
0i_fall/layer_normalization/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:|
2i_fall/layer_normalization/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:|
2i_fall/layer_normalization/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
*i_fall/layer_normalization/strided_slice_1StridedSlice)i_fall/layer_normalization/Shape:output:09i_fall/layer_normalization/strided_slice_1/stack:output:0;i_fall/layer_normalization/strided_slice_1/stack_1:output:0;i_fall/layer_normalization/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskd
"i_fall/layer_normalization/mul_1/xConst*
_output_shapes
: *
dtype0*
value	B :?
 i_fall/layer_normalization/mul_1Mul+i_fall/layer_normalization/mul_1/x:output:03i_fall/layer_normalization/strided_slice_1:output:0*
T0*
_output_shapes
: l
*i_fall/layer_normalization/Reshape/shape/0Const*
_output_shapes
: *
dtype0*
value	B :l
*i_fall/layer_normalization/Reshape/shape/3Const*
_output_shapes
: *
dtype0*
value	B :?
(i_fall/layer_normalization/Reshape/shapePack3i_fall/layer_normalization/Reshape/shape/0:output:0"i_fall/layer_normalization/mul:z:0$i_fall/layer_normalization/mul_1:z:03i_fall/layer_normalization/Reshape/shape/3:output:0*
N*
T0*
_output_shapes
:?
"i_fall/layer_normalization/ReshapeReshapei_fall/dense/BiasAdd:output:01i_fall/layer_normalization/Reshape/shape:output:0*
T0*/
_output_shapes
:?????????j?
&i_fall/layer_normalization/ones/packedPack"i_fall/layer_normalization/mul:z:0*
N*
T0*
_output_shapes
:j
%i_fall/layer_normalization/ones/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ???
i_fall/layer_normalization/onesFill/i_fall/layer_normalization/ones/packed:output:0.i_fall/layer_normalization/ones/Const:output:0*
T0*#
_output_shapes
:??????????
'i_fall/layer_normalization/zeros/packedPack"i_fall/layer_normalization/mul:z:0*
N*
T0*
_output_shapes
:k
&i_fall/layer_normalization/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    ?
 i_fall/layer_normalization/zerosFill0i_fall/layer_normalization/zeros/packed:output:0/i_fall/layer_normalization/zeros/Const:output:0*
T0*#
_output_shapes
:?????????c
 i_fall/layer_normalization/ConstConst*
_output_shapes
: *
dtype0*
valueB e
"i_fall/layer_normalization/Const_1Const*
_output_shapes
: *
dtype0*
valueB ?
+i_fall/layer_normalization/FusedBatchNormV3FusedBatchNormV3+i_fall/layer_normalization/Reshape:output:0(i_fall/layer_normalization/ones:output:0)i_fall/layer_normalization/zeros:output:0)i_fall/layer_normalization/Const:output:0+i_fall/layer_normalization/Const_1:output:0*
T0*
U0*o
_output_shapes]
[:?????????j:?????????:?????????:?????????:?????????:*
data_formatNCHW*
epsilon%o?:?
$i_fall/layer_normalization/Reshape_1Reshape/i_fall/layer_normalization/FusedBatchNormV3:y:0)i_fall/layer_normalization/Shape:output:0*
T0*'
_output_shapes
:?????????j?
/i_fall/layer_normalization/mul_2/ReadVariableOpReadVariableOp8i_fall_layer_normalization_mul_2_readvariableop_resource*
_output_shapes
:j*
dtype0?
 i_fall/layer_normalization/mul_2Mul-i_fall/layer_normalization/Reshape_1:output:07i_fall/layer_normalization/mul_2/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j?
-i_fall/layer_normalization/add/ReadVariableOpReadVariableOp6i_fall_layer_normalization_add_readvariableop_resource*
_output_shapes
:j*
dtype0?
i_fall/layer_normalization/addAddV2$i_fall/layer_normalization/mul_2:z:05i_fall/layer_normalization/add/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jy
i_fall/dropout/IdentityIdentity"i_fall/layer_normalization/add:z:0*
T0*'
_output_shapes
:?????????j?
$i_fall/dense_1/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_1_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
i_fall/dense_1/MatMulMatMul i_fall/dropout/Identity:output:0,i_fall/dense_1/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_1/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_1_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_1/BiasAddBiasAddi_fall/dense_1/MatMul:product:0-i_fall/dense_1/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
$i_fall/dense_1/leaky_re_lu/LeakyRelu	LeakyRelui_fall/dense_1/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
$i_fall/dense_2/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_2_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
i_fall/dense_2/MatMulMatMul i_fall/dropout/Identity:output:0,i_fall/dense_2/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_2/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_2_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_2/BiasAddBiasAddi_fall/dense_2/MatMul:product:0-i_fall/dense_2/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
!i_fall/tf.__operators__.add/AddV2AddV22i_fall/dense_1/leaky_re_lu/LeakyRelu:activations:0i_fall/dense_2/BiasAdd:output:0*
T0*(
_output_shapes
:??????????
i_fall/dropout_1/IdentityIdentity%i_fall/tf.__operators__.add/AddV2:z:0*
T0*(
_output_shapes
:???????????
$i_fall/dense_3/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_3_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
i_fall/dense_3/MatMulMatMul"i_fall/dropout_1/Identity:output:0,i_fall/dense_3/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_3/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_3_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_3/BiasAddBiasAddi_fall/dense_3/MatMul:product:0-i_fall/dense_3/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
&i_fall/dense_3/leaky_re_lu_1/LeakyRelu	LeakyRelui_fall/dense_3/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
$i_fall/dense_4/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_4_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
i_fall/dense_4/MatMulMatMul i_fall/dropout/Identity:output:0,i_fall/dense_4/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_4/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_4_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_4/BiasAddBiasAddi_fall/dense_4/MatMul:product:0-i_fall/dense_4/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
#i_fall/tf.__operators__.add_1/AddV2AddV24i_fall/dense_3/leaky_re_lu_1/LeakyRelu:activations:0i_fall/dense_4/BiasAdd:output:0*
T0*(
_output_shapes
:???????????
i_fall/dropout_2/IdentityIdentity'i_fall/tf.__operators__.add_1/AddV2:z:0*
T0*(
_output_shapes
:???????????
$i_fall/dense_5/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_5_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
i_fall/dense_5/MatMulMatMul"i_fall/dropout_2/Identity:output:0,i_fall/dense_5/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_5/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_5_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_5/BiasAddBiasAddi_fall/dense_5/MatMul:product:0-i_fall/dense_5/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
&i_fall/dense_5/leaky_re_lu_2/LeakyRelu	LeakyRelui_fall/dense_5/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
$i_fall/dense_6/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_6_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
i_fall/dense_6/MatMulMatMul i_fall/dropout/Identity:output:0,i_fall/dense_6/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_6/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_6/BiasAddBiasAddi_fall/dense_6/MatMul:product:0-i_fall/dense_6/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
#i_fall/tf.__operators__.add_2/AddV2AddV24i_fall/dense_5/leaky_re_lu_2/LeakyRelu:activations:0i_fall/dense_6/BiasAdd:output:0*
T0*(
_output_shapes
:???????????
i_fall/dropout_3/IdentityIdentity'i_fall/tf.__operators__.add_2/AddV2:z:0*
T0*(
_output_shapes
:???????????
$i_fall/dense_7/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_7_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
i_fall/dense_7/MatMulMatMul"i_fall/dropout_3/Identity:output:0,i_fall/dense_7/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_7/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_7/BiasAddBiasAddi_fall/dense_7/MatMul:product:0-i_fall/dense_7/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
&i_fall/dense_7/leaky_re_lu_3/LeakyRelu	LeakyRelui_fall/dense_7/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
$i_fall/dense_8/MatMul/ReadVariableOpReadVariableOp-i_fall_dense_8_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
i_fall/dense_8/MatMulMatMul i_fall/dropout/Identity:output:0,i_fall/dense_8/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
%i_fall/dense_8/BiasAdd/ReadVariableOpReadVariableOp.i_fall_dense_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
i_fall/dense_8/BiasAddBiasAddi_fall/dense_8/MatMul:product:0-i_fall/dense_8/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
#i_fall/tf.__operators__.add_3/AddV2AddV24i_fall/dense_7/leaky_re_lu_3/LeakyRelu:activations:0i_fall/dense_8/BiasAdd:output:0*
T0*(
_output_shapes
:???????????
i_fall/dropout_4/IdentityIdentity'i_fall/tf.__operators__.add_3/AddV2:z:0*
T0*(
_output_shapes
:???????????
3i_fall/readmission_prediction/MatMul/ReadVariableOpReadVariableOp<i_fall_readmission_prediction_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype0?
$i_fall/readmission_prediction/MatMulMatMul"i_fall/dropout_4/Identity:output:0;i_fall/readmission_prediction/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
4i_fall/readmission_prediction/BiasAdd/ReadVariableOpReadVariableOp=i_fall_readmission_prediction_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
%i_fall/readmission_prediction/BiasAddBiasAdd.i_fall/readmission_prediction/MatMul:product:0<i_fall/readmission_prediction/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
%i_fall/readmission_prediction/SigmoidSigmoid.i_fall/readmission_prediction/BiasAdd:output:0*
T0*'
_output_shapes
:?????????x
IdentityIdentity)i_fall/readmission_prediction/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:??????????	
NoOpNoOp$^i_fall/dense/BiasAdd/ReadVariableOp#^i_fall/dense/MatMul/ReadVariableOp&^i_fall/dense_1/BiasAdd/ReadVariableOp%^i_fall/dense_1/MatMul/ReadVariableOp&^i_fall/dense_2/BiasAdd/ReadVariableOp%^i_fall/dense_2/MatMul/ReadVariableOp&^i_fall/dense_3/BiasAdd/ReadVariableOp%^i_fall/dense_3/MatMul/ReadVariableOp&^i_fall/dense_4/BiasAdd/ReadVariableOp%^i_fall/dense_4/MatMul/ReadVariableOp&^i_fall/dense_5/BiasAdd/ReadVariableOp%^i_fall/dense_5/MatMul/ReadVariableOp&^i_fall/dense_6/BiasAdd/ReadVariableOp%^i_fall/dense_6/MatMul/ReadVariableOp&^i_fall/dense_7/BiasAdd/ReadVariableOp%^i_fall/dense_7/MatMul/ReadVariableOp&^i_fall/dense_8/BiasAdd/ReadVariableOp%^i_fall/dense_8/MatMul/ReadVariableOp"^i_fall/embedding/embedding_lookup$^i_fall/embedding_1/embedding_lookup$^i_fall/embedding_2/embedding_lookup$^i_fall/embedding_3/embedding_lookup$^i_fall/embedding_4/embedding_lookup.^i_fall/layer_normalization/add/ReadVariableOp0^i_fall/layer_normalization/mul_2/ReadVariableOp5^i_fall/readmission_prediction/BiasAdd/ReadVariableOp4^i_fall/readmission_prediction/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2J
#i_fall/dense/BiasAdd/ReadVariableOp#i_fall/dense/BiasAdd/ReadVariableOp2H
"i_fall/dense/MatMul/ReadVariableOp"i_fall/dense/MatMul/ReadVariableOp2N
%i_fall/dense_1/BiasAdd/ReadVariableOp%i_fall/dense_1/BiasAdd/ReadVariableOp2L
$i_fall/dense_1/MatMul/ReadVariableOp$i_fall/dense_1/MatMul/ReadVariableOp2N
%i_fall/dense_2/BiasAdd/ReadVariableOp%i_fall/dense_2/BiasAdd/ReadVariableOp2L
$i_fall/dense_2/MatMul/ReadVariableOp$i_fall/dense_2/MatMul/ReadVariableOp2N
%i_fall/dense_3/BiasAdd/ReadVariableOp%i_fall/dense_3/BiasAdd/ReadVariableOp2L
$i_fall/dense_3/MatMul/ReadVariableOp$i_fall/dense_3/MatMul/ReadVariableOp2N
%i_fall/dense_4/BiasAdd/ReadVariableOp%i_fall/dense_4/BiasAdd/ReadVariableOp2L
$i_fall/dense_4/MatMul/ReadVariableOp$i_fall/dense_4/MatMul/ReadVariableOp2N
%i_fall/dense_5/BiasAdd/ReadVariableOp%i_fall/dense_5/BiasAdd/ReadVariableOp2L
$i_fall/dense_5/MatMul/ReadVariableOp$i_fall/dense_5/MatMul/ReadVariableOp2N
%i_fall/dense_6/BiasAdd/ReadVariableOp%i_fall/dense_6/BiasAdd/ReadVariableOp2L
$i_fall/dense_6/MatMul/ReadVariableOp$i_fall/dense_6/MatMul/ReadVariableOp2N
%i_fall/dense_7/BiasAdd/ReadVariableOp%i_fall/dense_7/BiasAdd/ReadVariableOp2L
$i_fall/dense_7/MatMul/ReadVariableOp$i_fall/dense_7/MatMul/ReadVariableOp2N
%i_fall/dense_8/BiasAdd/ReadVariableOp%i_fall/dense_8/BiasAdd/ReadVariableOp2L
$i_fall/dense_8/MatMul/ReadVariableOp$i_fall/dense_8/MatMul/ReadVariableOp2F
!i_fall/embedding/embedding_lookup!i_fall/embedding/embedding_lookup2J
#i_fall/embedding_1/embedding_lookup#i_fall/embedding_1/embedding_lookup2J
#i_fall/embedding_2/embedding_lookup#i_fall/embedding_2/embedding_lookup2J
#i_fall/embedding_3/embedding_lookup#i_fall/embedding_3/embedding_lookup2J
#i_fall/embedding_4/embedding_lookup#i_fall/embedding_4/embedding_lookup2^
-i_fall/layer_normalization/add/ReadVariableOp-i_fall/layer_normalization/add/ReadVariableOp2b
/i_fall/layer_normalization/mul_2/ReadVariableOp/i_fall/layer_normalization/mul_2/ReadVariableOp2l
4i_fall/readmission_prediction/BiasAdd/ReadVariableOp4i_fall/readmission_prediction/BiasAdd/ReadVariableOp2j
3i_fall/readmission_prediction/MatMul/ReadVariableOp3i_fall/readmission_prediction/MatMul/ReadVariableOp:L H
'
_output_shapes
:?????????

_user_specified_nameDRG:RN
'
_output_shapes
:?????????
#
_user_specified_name	age_group:TP
'
_output_shapes
:?????????
%
_user_specified_namebed_day_dep:ZV
'
_output_shapes
:?????????
+
_user_specified_namedegree_of_urgency:YU
'
_output_shapes
:?????????
*
_user_specified_namediagnosis_code_1:OK
'
_output_shapes
:?????????
 
_user_specified_namegender:]Y
'
_output_shapes
:?????????
.
_user_specified_namemainDiagICD10Chapter
?

?
C__inference_dense_7_layer_call_and_return_conditional_losses_742351

inputs2
matmul_readvariableop_resource:
??.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????p
leaky_re_lu_3/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>u
IdentityIdentity%leaky_re_lu_3/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?

?
C__inference_dense_5_layer_call_and_return_conditional_losses_740475

inputs2
matmul_readvariableop_resource:
??.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????p
leaky_re_lu_2/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>u
IdentityIdentity%leaky_re_lu_2/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
E__inference_dropout_3_layer_call_and_return_conditional_losses_740503

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
a
C__inference_dropout_layer_call_and_return_conditional_losses_742121

inputs

identity_1N
IdentityIdentityinputs*
T0*'
_output_shapes
:?????????j[

Identity_1IdentityIdentity:output:0*
T0*'
_output_shapes
:?????????j"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????j:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?p
?
B__inference_i_fall_layer_call_and_return_conditional_losses_740564

inputs
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
inputs_6"
embedding_740234:$
embedding_1_740247:

$
embedding_2_740260:$
embedding_3_740273:%
embedding_4_740286:	?<@
dense_740322:jj
dense_740324:j(
layer_normalization_740370:j(
layer_normalization_740372:j!
dense_1_740394:	j?
dense_1_740396:	?!
dense_2_740410:	j?
dense_2_740412:	?"
dense_3_740435:
??
dense_3_740437:	?!
dense_4_740451:	j?
dense_4_740453:	?"
dense_5_740476:
??
dense_5_740478:	?!
dense_6_740492:	j?
dense_6_740494:	?"
dense_7_740517:
??
dense_7_740519:	?!
dense_8_740533:	j?
dense_8_740535:	?0
readmission_prediction_740558:	?+
readmission_prediction_740560:
identity??dense/StatefulPartitionedCall?dense_1/StatefulPartitionedCall?dense_2/StatefulPartitionedCall?dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?dense_6/StatefulPartitionedCall?dense_7/StatefulPartitionedCall?dense_8/StatefulPartitionedCall?!embedding/StatefulPartitionedCall?#embedding_1/StatefulPartitionedCall?#embedding_2/StatefulPartitionedCall?#embedding_3/StatefulPartitionedCall?#embedding_4/StatefulPartitionedCall?+layer_normalization/StatefulPartitionedCall?.readmission_prediction/StatefulPartitionedCallk
tf.stack/stackPackinputs_2*
N*
T0*+
_output_shapes
:?????????*

axisk
tf.stack_1/stackPackinputs*
N*
T0*+
_output_shapes
:?????????*

axis?
!embedding/StatefulPartitionedCallStatefulPartitionedCallinputs_5embedding_740234*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_embedding_layer_call_and_return_conditional_losses_740233?
#embedding_1/StatefulPartitionedCallStatefulPartitionedCallinputs_1embedding_1_740247*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????
*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_1_layer_call_and_return_conditional_losses_740246?
#embedding_2/StatefulPartitionedCallStatefulPartitionedCallinputs_3embedding_2_740260*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_2_layer_call_and_return_conditional_losses_740259?
#embedding_3/StatefulPartitionedCallStatefulPartitionedCallinputs_6embedding_3_740273*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_3_layer_call_and_return_conditional_losses_740272?
#embedding_4/StatefulPartitionedCallStatefulPartitionedCallinputs_4embedding_4_740286*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????@*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_4_layer_call_and_return_conditional_losses_740285?
concatenate/PartitionedCallPartitionedCalltf.stack/stack:output:0tf.stack_1/stack:output:0*embedding/StatefulPartitionedCall:output:0,embedding_1/StatefulPartitionedCall:output:0,embedding_2/StatefulPartitionedCall:output:0,embedding_3/StatefulPartitionedCall:output:0,embedding_4/StatefulPartitionedCall:output:0*
Tin
	2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_concatenate_layer_call_and_return_conditional_losses_740301?
flatten/PartitionedCallPartitionedCall$concatenate/PartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_flatten_layer_call_and_return_conditional_losses_740309?
dense/StatefulPartitionedCallStatefulPartitionedCall flatten/PartitionedCall:output:0dense_740322dense_740324*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_dense_layer_call_and_return_conditional_losses_740321?
+layer_normalization/StatefulPartitionedCallStatefulPartitionedCall&dense/StatefulPartitionedCall:output:0layer_normalization_740370layer_normalization_740372*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *X
fSRQ
O__inference_layer_normalization_layer_call_and_return_conditional_losses_740369?
dropout/PartitionedCallPartitionedCall4layer_normalization/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dropout_layer_call_and_return_conditional_losses_740380?
dense_1/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_1_740394dense_1_740396*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_1_layer_call_and_return_conditional_losses_740393?
dense_2/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_2_740410dense_2_740412*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_2_layer_call_and_return_conditional_losses_740409?
tf.__operators__.add/AddV2AddV2(dense_1/StatefulPartitionedCall:output:0(dense_2/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_1/PartitionedCallPartitionedCalltf.__operators__.add/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_1_layer_call_and_return_conditional_losses_740421?
dense_3/StatefulPartitionedCallStatefulPartitionedCall"dropout_1/PartitionedCall:output:0dense_3_740435dense_3_740437*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_3_layer_call_and_return_conditional_losses_740434?
dense_4/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_4_740451dense_4_740453*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_4_layer_call_and_return_conditional_losses_740450?
tf.__operators__.add_1/AddV2AddV2(dense_3/StatefulPartitionedCall:output:0(dense_4/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_2/PartitionedCallPartitionedCall tf.__operators__.add_1/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_2_layer_call_and_return_conditional_losses_740462?
dense_5/StatefulPartitionedCallStatefulPartitionedCall"dropout_2/PartitionedCall:output:0dense_5_740476dense_5_740478*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_5_layer_call_and_return_conditional_losses_740475?
dense_6/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_6_740492dense_6_740494*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_6_layer_call_and_return_conditional_losses_740491?
tf.__operators__.add_2/AddV2AddV2(dense_5/StatefulPartitionedCall:output:0(dense_6/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_3/PartitionedCallPartitionedCall tf.__operators__.add_2/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_3_layer_call_and_return_conditional_losses_740503?
dense_7/StatefulPartitionedCallStatefulPartitionedCall"dropout_3/PartitionedCall:output:0dense_7_740517dense_7_740519*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_7_layer_call_and_return_conditional_losses_740516?
dense_8/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_8_740533dense_8_740535*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_8_layer_call_and_return_conditional_losses_740532?
tf.__operators__.add_3/AddV2AddV2(dense_7/StatefulPartitionedCall:output:0(dense_8/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_4/PartitionedCallPartitionedCall tf.__operators__.add_3/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_4_layer_call_and_return_conditional_losses_740544?
.readmission_prediction/StatefulPartitionedCallStatefulPartitionedCall"dropout_4/PartitionedCall:output:0readmission_prediction_740558readmission_prediction_740560*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *[
fVRT
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_740557?
IdentityIdentity7readmission_prediction/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp^dense/StatefulPartitionedCall ^dense_1/StatefulPartitionedCall ^dense_2/StatefulPartitionedCall ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall ^dense_6/StatefulPartitionedCall ^dense_7/StatefulPartitionedCall ^dense_8/StatefulPartitionedCall"^embedding/StatefulPartitionedCall$^embedding_1/StatefulPartitionedCall$^embedding_2/StatefulPartitionedCall$^embedding_3/StatefulPartitionedCall$^embedding_4/StatefulPartitionedCall,^layer_normalization/StatefulPartitionedCall/^readmission_prediction/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2B
dense_2/StatefulPartitionedCalldense_2/StatefulPartitionedCall2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall2B
dense_6/StatefulPartitionedCalldense_6/StatefulPartitionedCall2B
dense_7/StatefulPartitionedCalldense_7/StatefulPartitionedCall2B
dense_8/StatefulPartitionedCalldense_8/StatefulPartitionedCall2F
!embedding/StatefulPartitionedCall!embedding/StatefulPartitionedCall2J
#embedding_1/StatefulPartitionedCall#embedding_1/StatefulPartitionedCall2J
#embedding_2/StatefulPartitionedCall#embedding_2/StatefulPartitionedCall2J
#embedding_3/StatefulPartitionedCall#embedding_3/StatefulPartitionedCall2J
#embedding_4/StatefulPartitionedCall#embedding_4/StatefulPartitionedCall2Z
+layer_normalization/StatefulPartitionedCall+layer_normalization/StatefulPartitionedCall2`
.readmission_prediction/StatefulPartitionedCall.readmission_prediction/StatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_742417

inputs1
matmul_readvariableop_resource:	?-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????V
SigmoidSigmoidBiasAdd:output:0*
T0*'
_output_shapes
:?????????Z
IdentityIdentitySigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
?
C__inference_dense_8_layer_call_and_return_conditional_losses_740532

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
$__inference_signature_wrapper_741451
drg
	age_group
bed_day_dep
degree_of_urgency
diagnosis_code_1

gender
maindiagicd10chapter
unknown:
	unknown_0:


	unknown_1:
	unknown_2:
	unknown_3:	?<@
	unknown_4:jj
	unknown_5:j
	unknown_6:j
	unknown_7:j
	unknown_8:	j?
	unknown_9:	?

unknown_10:	j?

unknown_11:	?

unknown_12:
??

unknown_13:	?

unknown_14:	j?

unknown_15:	?

unknown_16:
??

unknown_17:	?

unknown_18:	j?

unknown_19:	?

unknown_20:
??

unknown_21:	?

unknown_22:	j?

unknown_23:	?

unknown_24:	?

unknown_25:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalldrg	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermaindiagicd10chapterunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12
unknown_13
unknown_14
unknown_15
unknown_16
unknown_17
unknown_18
unknown_19
unknown_20
unknown_21
unknown_22
unknown_23
unknown_24
unknown_25*-
Tin&
$2"*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*=
_read_only_resource_inputs
	
 !*-
config_proto

CPU

GPU 2J 8? **
f%R#
!__inference__wrapped_model_740203o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:L H
'
_output_shapes
:?????????

_user_specified_nameDRG:RN
'
_output_shapes
:?????????
#
_user_specified_name	age_group:TP
'
_output_shapes
:?????????
%
_user_specified_namebed_day_dep:ZV
'
_output_shapes
:?????????
+
_user_specified_namedegree_of_urgency:YU
'
_output_shapes
:?????????
*
_user_specified_namediagnosis_code_1:OK
'
_output_shapes
:?????????
 
_user_specified_namegender:]Y
'
_output_shapes
:?????????
.
_user_specified_namemainDiagICD10Chapter
?
c
E__inference_dropout_2_layer_call_and_return_conditional_losses_740462

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?x
?
B__inference_i_fall_layer_call_and_return_conditional_losses_741070

inputs
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
inputs_6"
embedding_740988:$
embedding_1_740991:

$
embedding_2_740994:$
embedding_3_740997:%
embedding_4_741000:	?<@
dense_741005:jj
dense_741007:j(
layer_normalization_741010:j(
layer_normalization_741012:j!
dense_1_741016:	j?
dense_1_741018:	?!
dense_2_741021:	j?
dense_2_741023:	?"
dense_3_741028:
??
dense_3_741030:	?!
dense_4_741033:	j?
dense_4_741035:	?"
dense_5_741040:
??
dense_5_741042:	?!
dense_6_741045:	j?
dense_6_741047:	?"
dense_7_741052:
??
dense_7_741054:	?!
dense_8_741057:	j?
dense_8_741059:	?0
readmission_prediction_741064:	?+
readmission_prediction_741066:
identity??dense/StatefulPartitionedCall?dense_1/StatefulPartitionedCall?dense_2/StatefulPartitionedCall?dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?dense_6/StatefulPartitionedCall?dense_7/StatefulPartitionedCall?dense_8/StatefulPartitionedCall?dropout/StatefulPartitionedCall?!dropout_1/StatefulPartitionedCall?!dropout_2/StatefulPartitionedCall?!dropout_3/StatefulPartitionedCall?!dropout_4/StatefulPartitionedCall?!embedding/StatefulPartitionedCall?#embedding_1/StatefulPartitionedCall?#embedding_2/StatefulPartitionedCall?#embedding_3/StatefulPartitionedCall?#embedding_4/StatefulPartitionedCall?+layer_normalization/StatefulPartitionedCall?.readmission_prediction/StatefulPartitionedCallk
tf.stack/stackPackinputs_2*
N*
T0*+
_output_shapes
:?????????*

axisk
tf.stack_1/stackPackinputs*
N*
T0*+
_output_shapes
:?????????*

axis?
!embedding/StatefulPartitionedCallStatefulPartitionedCallinputs_5embedding_740988*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_embedding_layer_call_and_return_conditional_losses_740233?
#embedding_1/StatefulPartitionedCallStatefulPartitionedCallinputs_1embedding_1_740991*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????
*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_1_layer_call_and_return_conditional_losses_740246?
#embedding_2/StatefulPartitionedCallStatefulPartitionedCallinputs_3embedding_2_740994*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_2_layer_call_and_return_conditional_losses_740259?
#embedding_3/StatefulPartitionedCallStatefulPartitionedCallinputs_6embedding_3_740997*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_3_layer_call_and_return_conditional_losses_740272?
#embedding_4/StatefulPartitionedCallStatefulPartitionedCallinputs_4embedding_4_741000*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????@*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_4_layer_call_and_return_conditional_losses_740285?
concatenate/PartitionedCallPartitionedCalltf.stack/stack:output:0tf.stack_1/stack:output:0*embedding/StatefulPartitionedCall:output:0,embedding_1/StatefulPartitionedCall:output:0,embedding_2/StatefulPartitionedCall:output:0,embedding_3/StatefulPartitionedCall:output:0,embedding_4/StatefulPartitionedCall:output:0*
Tin
	2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_concatenate_layer_call_and_return_conditional_losses_740301?
flatten/PartitionedCallPartitionedCall$concatenate/PartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_flatten_layer_call_and_return_conditional_losses_740309?
dense/StatefulPartitionedCallStatefulPartitionedCall flatten/PartitionedCall:output:0dense_741005dense_741007*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_dense_layer_call_and_return_conditional_losses_740321?
+layer_normalization/StatefulPartitionedCallStatefulPartitionedCall&dense/StatefulPartitionedCall:output:0layer_normalization_741010layer_normalization_741012*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *X
fSRQ
O__inference_layer_normalization_layer_call_and_return_conditional_losses_740369?
dropout/StatefulPartitionedCallStatefulPartitionedCall4layer_normalization/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dropout_layer_call_and_return_conditional_losses_740823?
dense_1/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_1_741016dense_1_741018*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_1_layer_call_and_return_conditional_losses_740393?
dense_2/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_2_741021dense_2_741023*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_2_layer_call_and_return_conditional_losses_740409?
tf.__operators__.add/AddV2AddV2(dense_1/StatefulPartitionedCall:output:0(dense_2/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_1/StatefulPartitionedCallStatefulPartitionedCalltf.__operators__.add/AddV2:z:0 ^dropout/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_1_layer_call_and_return_conditional_losses_740780?
dense_3/StatefulPartitionedCallStatefulPartitionedCall*dropout_1/StatefulPartitionedCall:output:0dense_3_741028dense_3_741030*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_3_layer_call_and_return_conditional_losses_740434?
dense_4/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_4_741033dense_4_741035*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_4_layer_call_and_return_conditional_losses_740450?
tf.__operators__.add_1/AddV2AddV2(dense_3/StatefulPartitionedCall:output:0(dense_4/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_2/StatefulPartitionedCallStatefulPartitionedCall tf.__operators__.add_1/AddV2:z:0"^dropout_1/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_2_layer_call_and_return_conditional_losses_740737?
dense_5/StatefulPartitionedCallStatefulPartitionedCall*dropout_2/StatefulPartitionedCall:output:0dense_5_741040dense_5_741042*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_5_layer_call_and_return_conditional_losses_740475?
dense_6/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_6_741045dense_6_741047*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_6_layer_call_and_return_conditional_losses_740491?
tf.__operators__.add_2/AddV2AddV2(dense_5/StatefulPartitionedCall:output:0(dense_6/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_3/StatefulPartitionedCallStatefulPartitionedCall tf.__operators__.add_2/AddV2:z:0"^dropout_2/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_3_layer_call_and_return_conditional_losses_740694?
dense_7/StatefulPartitionedCallStatefulPartitionedCall*dropout_3/StatefulPartitionedCall:output:0dense_7_741052dense_7_741054*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_7_layer_call_and_return_conditional_losses_740516?
dense_8/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_8_741057dense_8_741059*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_8_layer_call_and_return_conditional_losses_740532?
tf.__operators__.add_3/AddV2AddV2(dense_7/StatefulPartitionedCall:output:0(dense_8/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_4/StatefulPartitionedCallStatefulPartitionedCall tf.__operators__.add_3/AddV2:z:0"^dropout_3/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_4_layer_call_and_return_conditional_losses_740651?
.readmission_prediction/StatefulPartitionedCallStatefulPartitionedCall*dropout_4/StatefulPartitionedCall:output:0readmission_prediction_741064readmission_prediction_741066*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *[
fVRT
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_740557?
IdentityIdentity7readmission_prediction/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp^dense/StatefulPartitionedCall ^dense_1/StatefulPartitionedCall ^dense_2/StatefulPartitionedCall ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall ^dense_6/StatefulPartitionedCall ^dense_7/StatefulPartitionedCall ^dense_8/StatefulPartitionedCall ^dropout/StatefulPartitionedCall"^dropout_1/StatefulPartitionedCall"^dropout_2/StatefulPartitionedCall"^dropout_3/StatefulPartitionedCall"^dropout_4/StatefulPartitionedCall"^embedding/StatefulPartitionedCall$^embedding_1/StatefulPartitionedCall$^embedding_2/StatefulPartitionedCall$^embedding_3/StatefulPartitionedCall$^embedding_4/StatefulPartitionedCall,^layer_normalization/StatefulPartitionedCall/^readmission_prediction/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2B
dense_2/StatefulPartitionedCalldense_2/StatefulPartitionedCall2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall2B
dense_6/StatefulPartitionedCalldense_6/StatefulPartitionedCall2B
dense_7/StatefulPartitionedCalldense_7/StatefulPartitionedCall2B
dense_8/StatefulPartitionedCalldense_8/StatefulPartitionedCall2B
dropout/StatefulPartitionedCalldropout/StatefulPartitionedCall2F
!dropout_1/StatefulPartitionedCall!dropout_1/StatefulPartitionedCall2F
!dropout_2/StatefulPartitionedCall!dropout_2/StatefulPartitionedCall2F
!dropout_3/StatefulPartitionedCall!dropout_3/StatefulPartitionedCall2F
!dropout_4/StatefulPartitionedCall!dropout_4/StatefulPartitionedCall2F
!embedding/StatefulPartitionedCall!embedding/StatefulPartitionedCall2J
#embedding_1/StatefulPartitionedCall#embedding_1/StatefulPartitionedCall2J
#embedding_2/StatefulPartitionedCall#embedding_2/StatefulPartitionedCall2J
#embedding_3/StatefulPartitionedCall#embedding_3/StatefulPartitionedCall2J
#embedding_4/StatefulPartitionedCall#embedding_4/StatefulPartitionedCall2Z
+layer_normalization/StatefulPartitionedCall+layer_normalization/StatefulPartitionedCall2`
.readmission_prediction/StatefulPartitionedCall.readmission_prediction/StatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
?
C__inference_dense_6_layer_call_and_return_conditional_losses_742304

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?

?
C__inference_dense_3_layer_call_and_return_conditional_losses_740434

inputs2
matmul_readvariableop_resource:
??.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????p
leaky_re_lu_1/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>u
IdentityIdentity%leaky_re_lu_1/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
'__inference_i_fall_layer_call_fn_741516

inputs_drg
inputs_age_group
inputs_bed_day_dep
inputs_degree_of_urgency
inputs_diagnosis_code_1
inputs_gender
inputs_maindiagicd10chapter
unknown:
	unknown_0:


	unknown_1:
	unknown_2:
	unknown_3:	?<@
	unknown_4:jj
	unknown_5:j
	unknown_6:j
	unknown_7:j
	unknown_8:	j?
	unknown_9:	?

unknown_10:	j?

unknown_11:	?

unknown_12:
??

unknown_13:	?

unknown_14:	j?

unknown_15:	?

unknown_16:
??

unknown_17:	?

unknown_18:	j?

unknown_19:	?

unknown_20:
??

unknown_21:	?

unknown_22:	j?

unknown_23:	?

unknown_24:	?

unknown_25:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall
inputs_drginputs_age_groupinputs_bed_day_depinputs_degree_of_urgencyinputs_diagnosis_code_1inputs_genderinputs_maindiagicd10chapterunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12
unknown_13
unknown_14
unknown_15
unknown_16
unknown_17
unknown_18
unknown_19
unknown_20
unknown_21
unknown_22
unknown_23
unknown_24
unknown_25*-
Tin&
$2"*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*=
_read_only_resource_inputs
	
 !*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_i_fall_layer_call_and_return_conditional_losses_740564o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:S O
'
_output_shapes
:?????????
$
_user_specified_name
inputs/DRG:YU
'
_output_shapes
:?????????
*
_user_specified_nameinputs/age_group:[W
'
_output_shapes
:?????????
,
_user_specified_nameinputs/bed_day_dep:a]
'
_output_shapes
:?????????
2
_user_specified_nameinputs/degree_of_urgency:`\
'
_output_shapes
:?????????
1
_user_specified_nameinputs/diagnosis_code_1:VR
'
_output_shapes
:?????????
'
_user_specified_nameinputs/gender:d`
'
_output_shapes
:?????????
5
_user_specified_nameinputs/mainDiagICD10Chapter
?	
?
C__inference_dense_6_layer_call_and_return_conditional_losses_740491

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
??
?8
"__inference__traced_restore_742996
file_prefix7
%assignvariableop_embedding_embeddings:;
)assignvariableop_1_embedding_1_embeddings:

;
)assignvariableop_2_embedding_2_embeddings:;
)assignvariableop_3_embedding_3_embeddings:<
)assignvariableop_4_embedding_4_embeddings:	?<@1
assignvariableop_5_dense_kernel:jj+
assignvariableop_6_dense_bias:j:
,assignvariableop_7_layer_normalization_gamma:j9
+assignvariableop_8_layer_normalization_beta:j4
!assignvariableop_9_dense_1_kernel:	j?/
 assignvariableop_10_dense_1_bias:	?5
"assignvariableop_11_dense_2_kernel:	j?/
 assignvariableop_12_dense_2_bias:	?6
"assignvariableop_13_dense_3_kernel:
??/
 assignvariableop_14_dense_3_bias:	?5
"assignvariableop_15_dense_4_kernel:	j?/
 assignvariableop_16_dense_4_bias:	?6
"assignvariableop_17_dense_5_kernel:
??/
 assignvariableop_18_dense_5_bias:	?5
"assignvariableop_19_dense_6_kernel:	j?/
 assignvariableop_20_dense_6_bias:	?6
"assignvariableop_21_dense_7_kernel:
??/
 assignvariableop_22_dense_7_bias:	?5
"assignvariableop_23_dense_8_kernel:	j?/
 assignvariableop_24_dense_8_bias:	?D
1assignvariableop_25_readmission_prediction_kernel:	?=
/assignvariableop_26_readmission_prediction_bias:'
assignvariableop_27_adam_iter:	 )
assignvariableop_28_adam_beta_1: )
assignvariableop_29_adam_beta_2: (
assignvariableop_30_adam_decay: 0
&assignvariableop_31_adam_learning_rate: %
assignvariableop_32_total_1: %
assignvariableop_33_count_1: #
assignvariableop_34_total: #
assignvariableop_35_count: A
/assignvariableop_36_adam_embedding_embeddings_m:C
1assignvariableop_37_adam_embedding_1_embeddings_m:

C
1assignvariableop_38_adam_embedding_2_embeddings_m:C
1assignvariableop_39_adam_embedding_3_embeddings_m:D
1assignvariableop_40_adam_embedding_4_embeddings_m:	?<@9
'assignvariableop_41_adam_dense_kernel_m:jj3
%assignvariableop_42_adam_dense_bias_m:jB
4assignvariableop_43_adam_layer_normalization_gamma_m:jA
3assignvariableop_44_adam_layer_normalization_beta_m:j<
)assignvariableop_45_adam_dense_1_kernel_m:	j?6
'assignvariableop_46_adam_dense_1_bias_m:	?<
)assignvariableop_47_adam_dense_2_kernel_m:	j?6
'assignvariableop_48_adam_dense_2_bias_m:	?=
)assignvariableop_49_adam_dense_3_kernel_m:
??6
'assignvariableop_50_adam_dense_3_bias_m:	?<
)assignvariableop_51_adam_dense_4_kernel_m:	j?6
'assignvariableop_52_adam_dense_4_bias_m:	?=
)assignvariableop_53_adam_dense_5_kernel_m:
??6
'assignvariableop_54_adam_dense_5_bias_m:	?<
)assignvariableop_55_adam_dense_6_kernel_m:	j?6
'assignvariableop_56_adam_dense_6_bias_m:	?=
)assignvariableop_57_adam_dense_7_kernel_m:
??6
'assignvariableop_58_adam_dense_7_bias_m:	?<
)assignvariableop_59_adam_dense_8_kernel_m:	j?6
'assignvariableop_60_adam_dense_8_bias_m:	?K
8assignvariableop_61_adam_readmission_prediction_kernel_m:	?D
6assignvariableop_62_adam_readmission_prediction_bias_m:A
/assignvariableop_63_adam_embedding_embeddings_v:C
1assignvariableop_64_adam_embedding_1_embeddings_v:

C
1assignvariableop_65_adam_embedding_2_embeddings_v:C
1assignvariableop_66_adam_embedding_3_embeddings_v:D
1assignvariableop_67_adam_embedding_4_embeddings_v:	?<@9
'assignvariableop_68_adam_dense_kernel_v:jj3
%assignvariableop_69_adam_dense_bias_v:jB
4assignvariableop_70_adam_layer_normalization_gamma_v:jA
3assignvariableop_71_adam_layer_normalization_beta_v:j<
)assignvariableop_72_adam_dense_1_kernel_v:	j?6
'assignvariableop_73_adam_dense_1_bias_v:	?<
)assignvariableop_74_adam_dense_2_kernel_v:	j?6
'assignvariableop_75_adam_dense_2_bias_v:	?=
)assignvariableop_76_adam_dense_3_kernel_v:
??6
'assignvariableop_77_adam_dense_3_bias_v:	?<
)assignvariableop_78_adam_dense_4_kernel_v:	j?6
'assignvariableop_79_adam_dense_4_bias_v:	?=
)assignvariableop_80_adam_dense_5_kernel_v:
??6
'assignvariableop_81_adam_dense_5_bias_v:	?<
)assignvariableop_82_adam_dense_6_kernel_v:	j?6
'assignvariableop_83_adam_dense_6_bias_v:	?=
)assignvariableop_84_adam_dense_7_kernel_v:
??6
'assignvariableop_85_adam_dense_7_bias_v:	?<
)assignvariableop_86_adam_dense_8_kernel_v:	j?6
'assignvariableop_87_adam_dense_8_bias_v:	?K
8assignvariableop_88_adam_readmission_prediction_kernel_v:	?D
6assignvariableop_89_adam_readmission_prediction_bias_v:
identity_91??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_16?AssignVariableOp_17?AssignVariableOp_18?AssignVariableOp_19?AssignVariableOp_2?AssignVariableOp_20?AssignVariableOp_21?AssignVariableOp_22?AssignVariableOp_23?AssignVariableOp_24?AssignVariableOp_25?AssignVariableOp_26?AssignVariableOp_27?AssignVariableOp_28?AssignVariableOp_29?AssignVariableOp_3?AssignVariableOp_30?AssignVariableOp_31?AssignVariableOp_32?AssignVariableOp_33?AssignVariableOp_34?AssignVariableOp_35?AssignVariableOp_36?AssignVariableOp_37?AssignVariableOp_38?AssignVariableOp_39?AssignVariableOp_4?AssignVariableOp_40?AssignVariableOp_41?AssignVariableOp_42?AssignVariableOp_43?AssignVariableOp_44?AssignVariableOp_45?AssignVariableOp_46?AssignVariableOp_47?AssignVariableOp_48?AssignVariableOp_49?AssignVariableOp_5?AssignVariableOp_50?AssignVariableOp_51?AssignVariableOp_52?AssignVariableOp_53?AssignVariableOp_54?AssignVariableOp_55?AssignVariableOp_56?AssignVariableOp_57?AssignVariableOp_58?AssignVariableOp_59?AssignVariableOp_6?AssignVariableOp_60?AssignVariableOp_61?AssignVariableOp_62?AssignVariableOp_63?AssignVariableOp_64?AssignVariableOp_65?AssignVariableOp_66?AssignVariableOp_67?AssignVariableOp_68?AssignVariableOp_69?AssignVariableOp_7?AssignVariableOp_70?AssignVariableOp_71?AssignVariableOp_72?AssignVariableOp_73?AssignVariableOp_74?AssignVariableOp_75?AssignVariableOp_76?AssignVariableOp_77?AssignVariableOp_78?AssignVariableOp_79?AssignVariableOp_8?AssignVariableOp_80?AssignVariableOp_81?AssignVariableOp_82?AssignVariableOp_83?AssignVariableOp_84?AssignVariableOp_85?AssignVariableOp_86?AssignVariableOp_87?AssignVariableOp_88?AssignVariableOp_89?AssignVariableOp_9?4
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:[*
dtype0*?3
value?3B?3[B:layer_with_weights-0/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-2/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-3/embeddings/.ATTRIBUTES/VARIABLE_VALUEB:layer_with_weights-4/embeddings/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-5/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-5/bias/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-6/gamma/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-6/beta/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-7/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-7/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-8/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-8/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-9/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-9/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-10/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-10/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-11/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-11/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-12/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-12/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-13/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-13/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-14/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-14/bias/.ATTRIBUTES/VARIABLE_VALUEB7layer_with_weights-15/kernel/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-15/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-0/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-2/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-3/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-4/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-6/gamma/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/beta/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-7/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-7/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-8/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-8/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-9/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-9/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-10/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-10/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-11/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-11/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-12/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-12/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-13/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-13/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-14/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-14/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-15/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-15/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-0/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-2/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-3/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-4/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-6/gamma/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/beta/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-7/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-7/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-8/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-8/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-9/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-9/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-10/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-10/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-11/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-11/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-12/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-12/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-13/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-13/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-14/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-14/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBSlayer_with_weights-15/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBQlayer_with_weights-15/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:[*
dtype0*?
value?B?[B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B ?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*?
_output_shapes?
?:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*i
dtypes_
]2[	[
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOpAssignVariableOp%assignvariableop_embedding_embeddingsIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_1AssignVariableOp)assignvariableop_1_embedding_1_embeddingsIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_2AssignVariableOp)assignvariableop_2_embedding_2_embeddingsIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_3AssignVariableOp)assignvariableop_3_embedding_3_embeddingsIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_4AssignVariableOp)assignvariableop_4_embedding_4_embeddingsIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_5AssignVariableOpassignvariableop_5_dense_kernelIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_6AssignVariableOpassignvariableop_6_dense_biasIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_7AssignVariableOp,assignvariableop_7_layer_normalization_gammaIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_8AssignVariableOp+assignvariableop_8_layer_normalization_betaIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_9AssignVariableOp!assignvariableop_9_dense_1_kernelIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_10AssignVariableOp assignvariableop_10_dense_1_biasIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_11AssignVariableOp"assignvariableop_11_dense_2_kernelIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_12AssignVariableOp assignvariableop_12_dense_2_biasIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_13AssignVariableOp"assignvariableop_13_dense_3_kernelIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_14AssignVariableOp assignvariableop_14_dense_3_biasIdentity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_15AssignVariableOp"assignvariableop_15_dense_4_kernelIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_16AssignVariableOp assignvariableop_16_dense_4_biasIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_17AssignVariableOp"assignvariableop_17_dense_5_kernelIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_18AssignVariableOp assignvariableop_18_dense_5_biasIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_19AssignVariableOp"assignvariableop_19_dense_6_kernelIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_20AssignVariableOp assignvariableop_20_dense_6_biasIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_21AssignVariableOp"assignvariableop_21_dense_7_kernelIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_22AssignVariableOp assignvariableop_22_dense_7_biasIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_23IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_23AssignVariableOp"assignvariableop_23_dense_8_kernelIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_24IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_24AssignVariableOp assignvariableop_24_dense_8_biasIdentity_24:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_25IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_25AssignVariableOp1assignvariableop_25_readmission_prediction_kernelIdentity_25:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_26IdentityRestoreV2:tensors:26"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_26AssignVariableOp/assignvariableop_26_readmission_prediction_biasIdentity_26:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_27IdentityRestoreV2:tensors:27"/device:CPU:0*
T0	*
_output_shapes
:?
AssignVariableOp_27AssignVariableOpassignvariableop_27_adam_iterIdentity_27:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	_
Identity_28IdentityRestoreV2:tensors:28"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_28AssignVariableOpassignvariableop_28_adam_beta_1Identity_28:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_29IdentityRestoreV2:tensors:29"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_29AssignVariableOpassignvariableop_29_adam_beta_2Identity_29:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_30IdentityRestoreV2:tensors:30"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_30AssignVariableOpassignvariableop_30_adam_decayIdentity_30:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_31IdentityRestoreV2:tensors:31"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_31AssignVariableOp&assignvariableop_31_adam_learning_rateIdentity_31:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_32IdentityRestoreV2:tensors:32"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_32AssignVariableOpassignvariableop_32_total_1Identity_32:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_33IdentityRestoreV2:tensors:33"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_33AssignVariableOpassignvariableop_33_count_1Identity_33:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_34IdentityRestoreV2:tensors:34"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_34AssignVariableOpassignvariableop_34_totalIdentity_34:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_35IdentityRestoreV2:tensors:35"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_35AssignVariableOpassignvariableop_35_countIdentity_35:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_36IdentityRestoreV2:tensors:36"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_36AssignVariableOp/assignvariableop_36_adam_embedding_embeddings_mIdentity_36:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_37IdentityRestoreV2:tensors:37"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_37AssignVariableOp1assignvariableop_37_adam_embedding_1_embeddings_mIdentity_37:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_38IdentityRestoreV2:tensors:38"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_38AssignVariableOp1assignvariableop_38_adam_embedding_2_embeddings_mIdentity_38:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_39IdentityRestoreV2:tensors:39"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_39AssignVariableOp1assignvariableop_39_adam_embedding_3_embeddings_mIdentity_39:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_40IdentityRestoreV2:tensors:40"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_40AssignVariableOp1assignvariableop_40_adam_embedding_4_embeddings_mIdentity_40:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_41IdentityRestoreV2:tensors:41"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_41AssignVariableOp'assignvariableop_41_adam_dense_kernel_mIdentity_41:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_42IdentityRestoreV2:tensors:42"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_42AssignVariableOp%assignvariableop_42_adam_dense_bias_mIdentity_42:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_43IdentityRestoreV2:tensors:43"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_43AssignVariableOp4assignvariableop_43_adam_layer_normalization_gamma_mIdentity_43:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_44IdentityRestoreV2:tensors:44"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_44AssignVariableOp3assignvariableop_44_adam_layer_normalization_beta_mIdentity_44:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_45IdentityRestoreV2:tensors:45"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_45AssignVariableOp)assignvariableop_45_adam_dense_1_kernel_mIdentity_45:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_46IdentityRestoreV2:tensors:46"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_46AssignVariableOp'assignvariableop_46_adam_dense_1_bias_mIdentity_46:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_47IdentityRestoreV2:tensors:47"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_47AssignVariableOp)assignvariableop_47_adam_dense_2_kernel_mIdentity_47:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_48IdentityRestoreV2:tensors:48"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_48AssignVariableOp'assignvariableop_48_adam_dense_2_bias_mIdentity_48:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_49IdentityRestoreV2:tensors:49"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_49AssignVariableOp)assignvariableop_49_adam_dense_3_kernel_mIdentity_49:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_50IdentityRestoreV2:tensors:50"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_50AssignVariableOp'assignvariableop_50_adam_dense_3_bias_mIdentity_50:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_51IdentityRestoreV2:tensors:51"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_51AssignVariableOp)assignvariableop_51_adam_dense_4_kernel_mIdentity_51:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_52IdentityRestoreV2:tensors:52"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_52AssignVariableOp'assignvariableop_52_adam_dense_4_bias_mIdentity_52:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_53IdentityRestoreV2:tensors:53"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_53AssignVariableOp)assignvariableop_53_adam_dense_5_kernel_mIdentity_53:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_54IdentityRestoreV2:tensors:54"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_54AssignVariableOp'assignvariableop_54_adam_dense_5_bias_mIdentity_54:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_55IdentityRestoreV2:tensors:55"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_55AssignVariableOp)assignvariableop_55_adam_dense_6_kernel_mIdentity_55:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_56IdentityRestoreV2:tensors:56"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_56AssignVariableOp'assignvariableop_56_adam_dense_6_bias_mIdentity_56:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_57IdentityRestoreV2:tensors:57"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_57AssignVariableOp)assignvariableop_57_adam_dense_7_kernel_mIdentity_57:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_58IdentityRestoreV2:tensors:58"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_58AssignVariableOp'assignvariableop_58_adam_dense_7_bias_mIdentity_58:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_59IdentityRestoreV2:tensors:59"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_59AssignVariableOp)assignvariableop_59_adam_dense_8_kernel_mIdentity_59:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_60IdentityRestoreV2:tensors:60"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_60AssignVariableOp'assignvariableop_60_adam_dense_8_bias_mIdentity_60:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_61IdentityRestoreV2:tensors:61"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_61AssignVariableOp8assignvariableop_61_adam_readmission_prediction_kernel_mIdentity_61:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_62IdentityRestoreV2:tensors:62"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_62AssignVariableOp6assignvariableop_62_adam_readmission_prediction_bias_mIdentity_62:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_63IdentityRestoreV2:tensors:63"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_63AssignVariableOp/assignvariableop_63_adam_embedding_embeddings_vIdentity_63:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_64IdentityRestoreV2:tensors:64"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_64AssignVariableOp1assignvariableop_64_adam_embedding_1_embeddings_vIdentity_64:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_65IdentityRestoreV2:tensors:65"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_65AssignVariableOp1assignvariableop_65_adam_embedding_2_embeddings_vIdentity_65:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_66IdentityRestoreV2:tensors:66"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_66AssignVariableOp1assignvariableop_66_adam_embedding_3_embeddings_vIdentity_66:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_67IdentityRestoreV2:tensors:67"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_67AssignVariableOp1assignvariableop_67_adam_embedding_4_embeddings_vIdentity_67:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_68IdentityRestoreV2:tensors:68"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_68AssignVariableOp'assignvariableop_68_adam_dense_kernel_vIdentity_68:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_69IdentityRestoreV2:tensors:69"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_69AssignVariableOp%assignvariableop_69_adam_dense_bias_vIdentity_69:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_70IdentityRestoreV2:tensors:70"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_70AssignVariableOp4assignvariableop_70_adam_layer_normalization_gamma_vIdentity_70:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_71IdentityRestoreV2:tensors:71"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_71AssignVariableOp3assignvariableop_71_adam_layer_normalization_beta_vIdentity_71:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_72IdentityRestoreV2:tensors:72"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_72AssignVariableOp)assignvariableop_72_adam_dense_1_kernel_vIdentity_72:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_73IdentityRestoreV2:tensors:73"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_73AssignVariableOp'assignvariableop_73_adam_dense_1_bias_vIdentity_73:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_74IdentityRestoreV2:tensors:74"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_74AssignVariableOp)assignvariableop_74_adam_dense_2_kernel_vIdentity_74:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_75IdentityRestoreV2:tensors:75"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_75AssignVariableOp'assignvariableop_75_adam_dense_2_bias_vIdentity_75:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_76IdentityRestoreV2:tensors:76"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_76AssignVariableOp)assignvariableop_76_adam_dense_3_kernel_vIdentity_76:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_77IdentityRestoreV2:tensors:77"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_77AssignVariableOp'assignvariableop_77_adam_dense_3_bias_vIdentity_77:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_78IdentityRestoreV2:tensors:78"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_78AssignVariableOp)assignvariableop_78_adam_dense_4_kernel_vIdentity_78:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_79IdentityRestoreV2:tensors:79"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_79AssignVariableOp'assignvariableop_79_adam_dense_4_bias_vIdentity_79:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_80IdentityRestoreV2:tensors:80"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_80AssignVariableOp)assignvariableop_80_adam_dense_5_kernel_vIdentity_80:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_81IdentityRestoreV2:tensors:81"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_81AssignVariableOp'assignvariableop_81_adam_dense_5_bias_vIdentity_81:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_82IdentityRestoreV2:tensors:82"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_82AssignVariableOp)assignvariableop_82_adam_dense_6_kernel_vIdentity_82:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_83IdentityRestoreV2:tensors:83"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_83AssignVariableOp'assignvariableop_83_adam_dense_6_bias_vIdentity_83:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_84IdentityRestoreV2:tensors:84"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_84AssignVariableOp)assignvariableop_84_adam_dense_7_kernel_vIdentity_84:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_85IdentityRestoreV2:tensors:85"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_85AssignVariableOp'assignvariableop_85_adam_dense_7_bias_vIdentity_85:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_86IdentityRestoreV2:tensors:86"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_86AssignVariableOp)assignvariableop_86_adam_dense_8_kernel_vIdentity_86:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_87IdentityRestoreV2:tensors:87"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_87AssignVariableOp'assignvariableop_87_adam_dense_8_bias_vIdentity_87:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_88IdentityRestoreV2:tensors:88"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_88AssignVariableOp8assignvariableop_88_adam_readmission_prediction_kernel_vIdentity_88:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_89IdentityRestoreV2:tensors:89"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_89AssignVariableOp6assignvariableop_89_adam_readmission_prediction_bias_vIdentity_89:output:0"/device:CPU:0*
_output_shapes
 *
dtype01
NoOpNoOp"/device:CPU:0*
_output_shapes
 ?
Identity_90Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_37^AssignVariableOp_38^AssignVariableOp_39^AssignVariableOp_4^AssignVariableOp_40^AssignVariableOp_41^AssignVariableOp_42^AssignVariableOp_43^AssignVariableOp_44^AssignVariableOp_45^AssignVariableOp_46^AssignVariableOp_47^AssignVariableOp_48^AssignVariableOp_49^AssignVariableOp_5^AssignVariableOp_50^AssignVariableOp_51^AssignVariableOp_52^AssignVariableOp_53^AssignVariableOp_54^AssignVariableOp_55^AssignVariableOp_56^AssignVariableOp_57^AssignVariableOp_58^AssignVariableOp_59^AssignVariableOp_6^AssignVariableOp_60^AssignVariableOp_61^AssignVariableOp_62^AssignVariableOp_63^AssignVariableOp_64^AssignVariableOp_65^AssignVariableOp_66^AssignVariableOp_67^AssignVariableOp_68^AssignVariableOp_69^AssignVariableOp_7^AssignVariableOp_70^AssignVariableOp_71^AssignVariableOp_72^AssignVariableOp_73^AssignVariableOp_74^AssignVariableOp_75^AssignVariableOp_76^AssignVariableOp_77^AssignVariableOp_78^AssignVariableOp_79^AssignVariableOp_8^AssignVariableOp_80^AssignVariableOp_81^AssignVariableOp_82^AssignVariableOp_83^AssignVariableOp_84^AssignVariableOp_85^AssignVariableOp_86^AssignVariableOp_87^AssignVariableOp_88^AssignVariableOp_89^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: W
Identity_91IdentityIdentity_90:output:0^NoOp_1*
T0*
_output_shapes
: ?
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_37^AssignVariableOp_38^AssignVariableOp_39^AssignVariableOp_4^AssignVariableOp_40^AssignVariableOp_41^AssignVariableOp_42^AssignVariableOp_43^AssignVariableOp_44^AssignVariableOp_45^AssignVariableOp_46^AssignVariableOp_47^AssignVariableOp_48^AssignVariableOp_49^AssignVariableOp_5^AssignVariableOp_50^AssignVariableOp_51^AssignVariableOp_52^AssignVariableOp_53^AssignVariableOp_54^AssignVariableOp_55^AssignVariableOp_56^AssignVariableOp_57^AssignVariableOp_58^AssignVariableOp_59^AssignVariableOp_6^AssignVariableOp_60^AssignVariableOp_61^AssignVariableOp_62^AssignVariableOp_63^AssignVariableOp_64^AssignVariableOp_65^AssignVariableOp_66^AssignVariableOp_67^AssignVariableOp_68^AssignVariableOp_69^AssignVariableOp_7^AssignVariableOp_70^AssignVariableOp_71^AssignVariableOp_72^AssignVariableOp_73^AssignVariableOp_74^AssignVariableOp_75^AssignVariableOp_76^AssignVariableOp_77^AssignVariableOp_78^AssignVariableOp_79^AssignVariableOp_8^AssignVariableOp_80^AssignVariableOp_81^AssignVariableOp_82^AssignVariableOp_83^AssignVariableOp_84^AssignVariableOp_85^AssignVariableOp_86^AssignVariableOp_87^AssignVariableOp_88^AssignVariableOp_89^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 "#
identity_91Identity_91:output:0*?
_input_shapes?
?: : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222*
AssignVariableOp_23AssignVariableOp_232*
AssignVariableOp_24AssignVariableOp_242*
AssignVariableOp_25AssignVariableOp_252*
AssignVariableOp_26AssignVariableOp_262*
AssignVariableOp_27AssignVariableOp_272*
AssignVariableOp_28AssignVariableOp_282*
AssignVariableOp_29AssignVariableOp_292(
AssignVariableOp_3AssignVariableOp_32*
AssignVariableOp_30AssignVariableOp_302*
AssignVariableOp_31AssignVariableOp_312*
AssignVariableOp_32AssignVariableOp_322*
AssignVariableOp_33AssignVariableOp_332*
AssignVariableOp_34AssignVariableOp_342*
AssignVariableOp_35AssignVariableOp_352*
AssignVariableOp_36AssignVariableOp_362*
AssignVariableOp_37AssignVariableOp_372*
AssignVariableOp_38AssignVariableOp_382*
AssignVariableOp_39AssignVariableOp_392(
AssignVariableOp_4AssignVariableOp_42*
AssignVariableOp_40AssignVariableOp_402*
AssignVariableOp_41AssignVariableOp_412*
AssignVariableOp_42AssignVariableOp_422*
AssignVariableOp_43AssignVariableOp_432*
AssignVariableOp_44AssignVariableOp_442*
AssignVariableOp_45AssignVariableOp_452*
AssignVariableOp_46AssignVariableOp_462*
AssignVariableOp_47AssignVariableOp_472*
AssignVariableOp_48AssignVariableOp_482*
AssignVariableOp_49AssignVariableOp_492(
AssignVariableOp_5AssignVariableOp_52*
AssignVariableOp_50AssignVariableOp_502*
AssignVariableOp_51AssignVariableOp_512*
AssignVariableOp_52AssignVariableOp_522*
AssignVariableOp_53AssignVariableOp_532*
AssignVariableOp_54AssignVariableOp_542*
AssignVariableOp_55AssignVariableOp_552*
AssignVariableOp_56AssignVariableOp_562*
AssignVariableOp_57AssignVariableOp_572*
AssignVariableOp_58AssignVariableOp_582*
AssignVariableOp_59AssignVariableOp_592(
AssignVariableOp_6AssignVariableOp_62*
AssignVariableOp_60AssignVariableOp_602*
AssignVariableOp_61AssignVariableOp_612*
AssignVariableOp_62AssignVariableOp_622*
AssignVariableOp_63AssignVariableOp_632*
AssignVariableOp_64AssignVariableOp_642*
AssignVariableOp_65AssignVariableOp_652*
AssignVariableOp_66AssignVariableOp_662*
AssignVariableOp_67AssignVariableOp_672*
AssignVariableOp_68AssignVariableOp_682*
AssignVariableOp_69AssignVariableOp_692(
AssignVariableOp_7AssignVariableOp_72*
AssignVariableOp_70AssignVariableOp_702*
AssignVariableOp_71AssignVariableOp_712*
AssignVariableOp_72AssignVariableOp_722*
AssignVariableOp_73AssignVariableOp_732*
AssignVariableOp_74AssignVariableOp_742*
AssignVariableOp_75AssignVariableOp_752*
AssignVariableOp_76AssignVariableOp_762*
AssignVariableOp_77AssignVariableOp_772*
AssignVariableOp_78AssignVariableOp_782*
AssignVariableOp_79AssignVariableOp_792(
AssignVariableOp_8AssignVariableOp_82*
AssignVariableOp_80AssignVariableOp_802*
AssignVariableOp_81AssignVariableOp_812*
AssignVariableOp_82AssignVariableOp_822*
AssignVariableOp_83AssignVariableOp_832*
AssignVariableOp_84AssignVariableOp_842*
AssignVariableOp_85AssignVariableOp_852*
AssignVariableOp_86AssignVariableOp_862*
AssignVariableOp_87AssignVariableOp_872*
AssignVariableOp_88AssignVariableOp_882*
AssignVariableOp_89AssignVariableOp_892(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
?
?
,__inference_embedding_2_layer_call_fn_741961

inputs
unknown:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_2_layer_call_and_return_conditional_losses_740259s
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
C__inference_dense_5_layer_call_and_return_conditional_losses_742285

inputs2
matmul_readvariableop_resource:
??.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????p
leaky_re_lu_2/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>u
IdentityIdentity%leaky_re_lu_2/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
(__inference_dense_5_layer_call_fn_742274

inputs
unknown:
??
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_5_layer_call_and_return_conditional_losses_740475p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
G__inference_embedding_1_layer_call_and_return_conditional_losses_741954

inputs)
embedding_lookup_741948:


identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_741948inputs*
Tindices0**
_class 
loc:@embedding_lookup/741948*+
_output_shapes
:?????????
*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/741948*+
_output_shapes
:?????????
?
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????
w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????
Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
,__inference_embedding_4_layer_call_fn_741993

inputs
unknown:	?<@
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????@*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_4_layer_call_and_return_conditional_losses_740285s
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????@`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
C__inference_dense_1_layer_call_and_return_conditional_losses_742153

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????n
leaky_re_lu/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>s
IdentityIdentity#leaky_re_lu/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
d
E__inference_dropout_4_layer_call_and_return_conditional_losses_740651

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
*__inference_dropout_3_layer_call_fn_742314

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_3_layer_call_and_return_conditional_losses_740694p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
,__inference_embedding_1_layer_call_fn_741945

inputs
unknown:


identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????
*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_1_layer_call_and_return_conditional_losses_740246s
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????
`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
O__inference_layer_normalization_layer_call_and_return_conditional_losses_742106

inputs+
mul_2_readvariableop_resource:j)
add_readvariableop_resource:j
identity??add/ReadVariableOp?mul_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskG
mul/xConst*
_output_shapes
: *
dtype0*
value	B :S
mulMulmul/x:output:0strided_slice:output:0*
T0*
_output_shapes
: _
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
strided_slice_1StridedSliceShape:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskI
mul_1/xConst*
_output_shapes
: *
dtype0*
value	B :Y
mul_1Mulmul_1/x:output:0strided_slice_1:output:0*
T0*
_output_shapes
: Q
Reshape/shape/0Const*
_output_shapes
: *
dtype0*
value	B :Q
Reshape/shape/3Const*
_output_shapes
: *
dtype0*
value	B :?
Reshape/shapePackReshape/shape/0:output:0mul:z:0	mul_1:z:0Reshape/shape/3:output:0*
N*
T0*
_output_shapes
:l
ReshapeReshapeinputsReshape/shape:output:0*
T0*/
_output_shapes
:?????????jJ
ones/packedPackmul:z:0*
N*
T0*
_output_shapes
:O

ones/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??e
onesFillones/packed:output:0ones/Const:output:0*
T0*#
_output_shapes
:?????????K
zeros/packedPackmul:z:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    h
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*#
_output_shapes
:?????????H
ConstConst*
_output_shapes
: *
dtype0*
valueB J
Const_1Const*
_output_shapes
: *
dtype0*
valueB ?
FusedBatchNormV3FusedBatchNormV3Reshape:output:0ones:output:0zeros:output:0Const:output:0Const_1:output:0*
T0*
U0*o
_output_shapes]
[:?????????j:?????????:?????????:?????????:?????????:*
data_formatNCHW*
epsilon%o?:l
	Reshape_1ReshapeFusedBatchNormV3:y:0Shape:output:0*
T0*'
_output_shapes
:?????????jn
mul_2/ReadVariableOpReadVariableOpmul_2_readvariableop_resource*
_output_shapes
:j*
dtype0p
mul_2MulReshape_1:output:0mul_2/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jj
add/ReadVariableOpReadVariableOpadd_readvariableop_resource*
_output_shapes
:j*
dtype0e
addAddV2	mul_2:z:0add/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jV
IdentityIdentityadd:z:0^NoOp*
T0*'
_output_shapes
:?????????jr
NoOpNoOp^add/ReadVariableOp^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 2(
add/ReadVariableOpadd/ReadVariableOp2,
mul_2/ReadVariableOpmul_2/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
'__inference_i_fall_layer_call_fn_741581

inputs_drg
inputs_age_group
inputs_bed_day_dep
inputs_degree_of_urgency
inputs_diagnosis_code_1
inputs_gender
inputs_maindiagicd10chapter
unknown:
	unknown_0:


	unknown_1:
	unknown_2:
	unknown_3:	?<@
	unknown_4:jj
	unknown_5:j
	unknown_6:j
	unknown_7:j
	unknown_8:	j?
	unknown_9:	?

unknown_10:	j?

unknown_11:	?

unknown_12:
??

unknown_13:	?

unknown_14:	j?

unknown_15:	?

unknown_16:
??

unknown_17:	?

unknown_18:	j?

unknown_19:	?

unknown_20:
??

unknown_21:	?

unknown_22:	j?

unknown_23:	?

unknown_24:	?

unknown_25:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall
inputs_drginputs_age_groupinputs_bed_day_depinputs_degree_of_urgencyinputs_diagnosis_code_1inputs_genderinputs_maindiagicd10chapterunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12
unknown_13
unknown_14
unknown_15
unknown_16
unknown_17
unknown_18
unknown_19
unknown_20
unknown_21
unknown_22
unknown_23
unknown_24
unknown_25*-
Tin&
$2"*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*=
_read_only_resource_inputs
	
 !*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_i_fall_layer_call_and_return_conditional_losses_741070o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:S O
'
_output_shapes
:?????????
$
_user_specified_name
inputs/DRG:YU
'
_output_shapes
:?????????
*
_user_specified_nameinputs/age_group:[W
'
_output_shapes
:?????????
,
_user_specified_nameinputs/bed_day_dep:a]
'
_output_shapes
:?????????
2
_user_specified_nameinputs/degree_of_urgency:`\
'
_output_shapes
:?????????
1
_user_specified_nameinputs/diagnosis_code_1:VR
'
_output_shapes
:?????????
'
_user_specified_nameinputs/gender:d`
'
_output_shapes
:?????????
5
_user_specified_nameinputs/mainDiagICD10Chapter
?
?
G__inference_embedding_2_layer_call_and_return_conditional_losses_741970

inputs)
embedding_lookup_741964:
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_741964inputs*
Tindices0**
_class 
loc:@embedding_lookup/741964*+
_output_shapes
:?????????*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/741964*+
_output_shapes
:??????????
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
d
E__inference_dropout_3_layer_call_and_return_conditional_losses_740694

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
d
E__inference_dropout_3_layer_call_and_return_conditional_losses_742331

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
(__inference_dense_4_layer_call_fn_742228

inputs
unknown:	j?
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_4_layer_call_and_return_conditional_losses_740450p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?y
?
B__inference_i_fall_layer_call_and_return_conditional_losses_741378
drg
	age_group
bed_day_dep
degree_of_urgency
diagnosis_code_1

gender
maindiagicd10chapter"
embedding_741296:$
embedding_1_741299:

$
embedding_2_741302:$
embedding_3_741305:%
embedding_4_741308:	?<@
dense_741313:jj
dense_741315:j(
layer_normalization_741318:j(
layer_normalization_741320:j!
dense_1_741324:	j?
dense_1_741326:	?!
dense_2_741329:	j?
dense_2_741331:	?"
dense_3_741336:
??
dense_3_741338:	?!
dense_4_741341:	j?
dense_4_741343:	?"
dense_5_741348:
??
dense_5_741350:	?!
dense_6_741353:	j?
dense_6_741355:	?"
dense_7_741360:
??
dense_7_741362:	?!
dense_8_741365:	j?
dense_8_741367:	?0
readmission_prediction_741372:	?+
readmission_prediction_741374:
identity??dense/StatefulPartitionedCall?dense_1/StatefulPartitionedCall?dense_2/StatefulPartitionedCall?dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?dense_6/StatefulPartitionedCall?dense_7/StatefulPartitionedCall?dense_8/StatefulPartitionedCall?dropout/StatefulPartitionedCall?!dropout_1/StatefulPartitionedCall?!dropout_2/StatefulPartitionedCall?!dropout_3/StatefulPartitionedCall?!dropout_4/StatefulPartitionedCall?!embedding/StatefulPartitionedCall?#embedding_1/StatefulPartitionedCall?#embedding_2/StatefulPartitionedCall?#embedding_3/StatefulPartitionedCall?#embedding_4/StatefulPartitionedCall?+layer_normalization/StatefulPartitionedCall?.readmission_prediction/StatefulPartitionedCalln
tf.stack/stackPackbed_day_dep*
N*
T0*+
_output_shapes
:?????????*

axish
tf.stack_1/stackPackdrg*
N*
T0*+
_output_shapes
:?????????*

axis?
!embedding/StatefulPartitionedCallStatefulPartitionedCallgenderembedding_741296*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_embedding_layer_call_and_return_conditional_losses_740233?
#embedding_1/StatefulPartitionedCallStatefulPartitionedCall	age_groupembedding_1_741299*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????
*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_1_layer_call_and_return_conditional_losses_740246?
#embedding_2/StatefulPartitionedCallStatefulPartitionedCalldegree_of_urgencyembedding_2_741302*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_2_layer_call_and_return_conditional_losses_740259?
#embedding_3/StatefulPartitionedCallStatefulPartitionedCallmaindiagicd10chapterembedding_3_741305*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_3_layer_call_and_return_conditional_losses_740272?
#embedding_4/StatefulPartitionedCallStatefulPartitionedCalldiagnosis_code_1embedding_4_741308*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????@*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_4_layer_call_and_return_conditional_losses_740285?
concatenate/PartitionedCallPartitionedCalltf.stack/stack:output:0tf.stack_1/stack:output:0*embedding/StatefulPartitionedCall:output:0,embedding_1/StatefulPartitionedCall:output:0,embedding_2/StatefulPartitionedCall:output:0,embedding_3/StatefulPartitionedCall:output:0,embedding_4/StatefulPartitionedCall:output:0*
Tin
	2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_concatenate_layer_call_and_return_conditional_losses_740301?
flatten/PartitionedCallPartitionedCall$concatenate/PartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_flatten_layer_call_and_return_conditional_losses_740309?
dense/StatefulPartitionedCallStatefulPartitionedCall flatten/PartitionedCall:output:0dense_741313dense_741315*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_dense_layer_call_and_return_conditional_losses_740321?
+layer_normalization/StatefulPartitionedCallStatefulPartitionedCall&dense/StatefulPartitionedCall:output:0layer_normalization_741318layer_normalization_741320*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *X
fSRQ
O__inference_layer_normalization_layer_call_and_return_conditional_losses_740369?
dropout/StatefulPartitionedCallStatefulPartitionedCall4layer_normalization/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dropout_layer_call_and_return_conditional_losses_740823?
dense_1/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_1_741324dense_1_741326*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_1_layer_call_and_return_conditional_losses_740393?
dense_2/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_2_741329dense_2_741331*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_2_layer_call_and_return_conditional_losses_740409?
tf.__operators__.add/AddV2AddV2(dense_1/StatefulPartitionedCall:output:0(dense_2/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_1/StatefulPartitionedCallStatefulPartitionedCalltf.__operators__.add/AddV2:z:0 ^dropout/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_1_layer_call_and_return_conditional_losses_740780?
dense_3/StatefulPartitionedCallStatefulPartitionedCall*dropout_1/StatefulPartitionedCall:output:0dense_3_741336dense_3_741338*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_3_layer_call_and_return_conditional_losses_740434?
dense_4/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_4_741341dense_4_741343*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_4_layer_call_and_return_conditional_losses_740450?
tf.__operators__.add_1/AddV2AddV2(dense_3/StatefulPartitionedCall:output:0(dense_4/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_2/StatefulPartitionedCallStatefulPartitionedCall tf.__operators__.add_1/AddV2:z:0"^dropout_1/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_2_layer_call_and_return_conditional_losses_740737?
dense_5/StatefulPartitionedCallStatefulPartitionedCall*dropout_2/StatefulPartitionedCall:output:0dense_5_741348dense_5_741350*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_5_layer_call_and_return_conditional_losses_740475?
dense_6/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_6_741353dense_6_741355*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_6_layer_call_and_return_conditional_losses_740491?
tf.__operators__.add_2/AddV2AddV2(dense_5/StatefulPartitionedCall:output:0(dense_6/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_3/StatefulPartitionedCallStatefulPartitionedCall tf.__operators__.add_2/AddV2:z:0"^dropout_2/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_3_layer_call_and_return_conditional_losses_740694?
dense_7/StatefulPartitionedCallStatefulPartitionedCall*dropout_3/StatefulPartitionedCall:output:0dense_7_741360dense_7_741362*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_7_layer_call_and_return_conditional_losses_740516?
dense_8/StatefulPartitionedCallStatefulPartitionedCall(dropout/StatefulPartitionedCall:output:0dense_8_741365dense_8_741367*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_8_layer_call_and_return_conditional_losses_740532?
tf.__operators__.add_3/AddV2AddV2(dense_7/StatefulPartitionedCall:output:0(dense_8/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
!dropout_4/StatefulPartitionedCallStatefulPartitionedCall tf.__operators__.add_3/AddV2:z:0"^dropout_3/StatefulPartitionedCall*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_4_layer_call_and_return_conditional_losses_740651?
.readmission_prediction/StatefulPartitionedCallStatefulPartitionedCall*dropout_4/StatefulPartitionedCall:output:0readmission_prediction_741372readmission_prediction_741374*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *[
fVRT
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_740557?
IdentityIdentity7readmission_prediction/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp^dense/StatefulPartitionedCall ^dense_1/StatefulPartitionedCall ^dense_2/StatefulPartitionedCall ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall ^dense_6/StatefulPartitionedCall ^dense_7/StatefulPartitionedCall ^dense_8/StatefulPartitionedCall ^dropout/StatefulPartitionedCall"^dropout_1/StatefulPartitionedCall"^dropout_2/StatefulPartitionedCall"^dropout_3/StatefulPartitionedCall"^dropout_4/StatefulPartitionedCall"^embedding/StatefulPartitionedCall$^embedding_1/StatefulPartitionedCall$^embedding_2/StatefulPartitionedCall$^embedding_3/StatefulPartitionedCall$^embedding_4/StatefulPartitionedCall,^layer_normalization/StatefulPartitionedCall/^readmission_prediction/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2B
dense_2/StatefulPartitionedCalldense_2/StatefulPartitionedCall2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall2B
dense_6/StatefulPartitionedCalldense_6/StatefulPartitionedCall2B
dense_7/StatefulPartitionedCalldense_7/StatefulPartitionedCall2B
dense_8/StatefulPartitionedCalldense_8/StatefulPartitionedCall2B
dropout/StatefulPartitionedCalldropout/StatefulPartitionedCall2F
!dropout_1/StatefulPartitionedCall!dropout_1/StatefulPartitionedCall2F
!dropout_2/StatefulPartitionedCall!dropout_2/StatefulPartitionedCall2F
!dropout_3/StatefulPartitionedCall!dropout_3/StatefulPartitionedCall2F
!dropout_4/StatefulPartitionedCall!dropout_4/StatefulPartitionedCall2F
!embedding/StatefulPartitionedCall!embedding/StatefulPartitionedCall2J
#embedding_1/StatefulPartitionedCall#embedding_1/StatefulPartitionedCall2J
#embedding_2/StatefulPartitionedCall#embedding_2/StatefulPartitionedCall2J
#embedding_3/StatefulPartitionedCall#embedding_3/StatefulPartitionedCall2J
#embedding_4/StatefulPartitionedCall#embedding_4/StatefulPartitionedCall2Z
+layer_normalization/StatefulPartitionedCall+layer_normalization/StatefulPartitionedCall2`
.readmission_prediction/StatefulPartitionedCall.readmission_prediction/StatefulPartitionedCall:L H
'
_output_shapes
:?????????

_user_specified_nameDRG:RN
'
_output_shapes
:?????????
#
_user_specified_name	age_group:TP
'
_output_shapes
:?????????
%
_user_specified_namebed_day_dep:ZV
'
_output_shapes
:?????????
+
_user_specified_namedegree_of_urgency:YU
'
_output_shapes
:?????????
*
_user_specified_namediagnosis_code_1:OK
'
_output_shapes
:?????????
 
_user_specified_namegender:]Y
'
_output_shapes
:?????????
.
_user_specified_namemainDiagICD10Chapter
?
c
E__inference_dropout_1_layer_call_and_return_conditional_losses_742187

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
D
(__inference_flatten_layer_call_fn_742030

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_flatten_layer_call_and_return_conditional_losses_740309`
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j:S O
+
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
?
C__inference_dense_4_layer_call_and_return_conditional_losses_742238

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
&__inference_dense_layer_call_fn_742045

inputs
unknown:jj
	unknown_0:j
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_dense_layer_call_and_return_conditional_losses_740321o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????j`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?

?
G__inference_concatenate_layer_call_and_return_conditional_losses_740301

inputs
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
inputs_6
identityM
concat/axisConst*
_output_shapes
: *
dtype0*
value	B :?
concatConcatV2inputsinputs_1inputs_2inputs_3inputs_4inputs_5inputs_6concat/axis:output:0*
N*
T0*+
_output_shapes
:?????????j[
IdentityIdentityconcat:output:0*
T0*+
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????
:?????????:?????????:?????????@:S O
+
_output_shapes
:?????????
 
_user_specified_nameinputs:SO
+
_output_shapes
:?????????
 
_user_specified_nameinputs:SO
+
_output_shapes
:?????????
 
_user_specified_nameinputs:SO
+
_output_shapes
:?????????

 
_user_specified_nameinputs:SO
+
_output_shapes
:?????????
 
_user_specified_nameinputs:SO
+
_output_shapes
:?????????
 
_user_specified_nameinputs:SO
+
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
?
(__inference_dense_7_layer_call_fn_742340

inputs
unknown:
??
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_7_layer_call_and_return_conditional_losses_740516p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
E__inference_dropout_3_layer_call_and_return_conditional_losses_742319

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
E__inference_dropout_2_layer_call_and_return_conditional_losses_742253

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?q
?
B__inference_i_fall_layer_call_and_return_conditional_losses_741285
drg
	age_group
bed_day_dep
degree_of_urgency
diagnosis_code_1

gender
maindiagicd10chapter"
embedding_741203:$
embedding_1_741206:

$
embedding_2_741209:$
embedding_3_741212:%
embedding_4_741215:	?<@
dense_741220:jj
dense_741222:j(
layer_normalization_741225:j(
layer_normalization_741227:j!
dense_1_741231:	j?
dense_1_741233:	?!
dense_2_741236:	j?
dense_2_741238:	?"
dense_3_741243:
??
dense_3_741245:	?!
dense_4_741248:	j?
dense_4_741250:	?"
dense_5_741255:
??
dense_5_741257:	?!
dense_6_741260:	j?
dense_6_741262:	?"
dense_7_741267:
??
dense_7_741269:	?!
dense_8_741272:	j?
dense_8_741274:	?0
readmission_prediction_741279:	?+
readmission_prediction_741281:
identity??dense/StatefulPartitionedCall?dense_1/StatefulPartitionedCall?dense_2/StatefulPartitionedCall?dense_3/StatefulPartitionedCall?dense_4/StatefulPartitionedCall?dense_5/StatefulPartitionedCall?dense_6/StatefulPartitionedCall?dense_7/StatefulPartitionedCall?dense_8/StatefulPartitionedCall?!embedding/StatefulPartitionedCall?#embedding_1/StatefulPartitionedCall?#embedding_2/StatefulPartitionedCall?#embedding_3/StatefulPartitionedCall?#embedding_4/StatefulPartitionedCall?+layer_normalization/StatefulPartitionedCall?.readmission_prediction/StatefulPartitionedCalln
tf.stack/stackPackbed_day_dep*
N*
T0*+
_output_shapes
:?????????*

axish
tf.stack_1/stackPackdrg*
N*
T0*+
_output_shapes
:?????????*

axis?
!embedding/StatefulPartitionedCallStatefulPartitionedCallgenderembedding_741203*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_embedding_layer_call_and_return_conditional_losses_740233?
#embedding_1/StatefulPartitionedCallStatefulPartitionedCall	age_groupembedding_1_741206*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????
*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_1_layer_call_and_return_conditional_losses_740246?
#embedding_2/StatefulPartitionedCallStatefulPartitionedCalldegree_of_urgencyembedding_2_741209*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_2_layer_call_and_return_conditional_losses_740259?
#embedding_3/StatefulPartitionedCallStatefulPartitionedCallmaindiagicd10chapterembedding_3_741212*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_3_layer_call_and_return_conditional_losses_740272?
#embedding_4/StatefulPartitionedCallStatefulPartitionedCalldiagnosis_code_1embedding_4_741215*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????@*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_4_layer_call_and_return_conditional_losses_740285?
concatenate/PartitionedCallPartitionedCalltf.stack/stack:output:0tf.stack_1/stack:output:0*embedding/StatefulPartitionedCall:output:0,embedding_1/StatefulPartitionedCall:output:0,embedding_2/StatefulPartitionedCall:output:0,embedding_3/StatefulPartitionedCall:output:0,embedding_4/StatefulPartitionedCall:output:0*
Tin
	2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_concatenate_layer_call_and_return_conditional_losses_740301?
flatten/PartitionedCallPartitionedCall$concatenate/PartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_flatten_layer_call_and_return_conditional_losses_740309?
dense/StatefulPartitionedCallStatefulPartitionedCall flatten/PartitionedCall:output:0dense_741220dense_741222*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_dense_layer_call_and_return_conditional_losses_740321?
+layer_normalization/StatefulPartitionedCallStatefulPartitionedCall&dense/StatefulPartitionedCall:output:0layer_normalization_741225layer_normalization_741227*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *X
fSRQ
O__inference_layer_normalization_layer_call_and_return_conditional_losses_740369?
dropout/PartitionedCallPartitionedCall4layer_normalization/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dropout_layer_call_and_return_conditional_losses_740380?
dense_1/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_1_741231dense_1_741233*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_1_layer_call_and_return_conditional_losses_740393?
dense_2/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_2_741236dense_2_741238*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_2_layer_call_and_return_conditional_losses_740409?
tf.__operators__.add/AddV2AddV2(dense_1/StatefulPartitionedCall:output:0(dense_2/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_1/PartitionedCallPartitionedCalltf.__operators__.add/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_1_layer_call_and_return_conditional_losses_740421?
dense_3/StatefulPartitionedCallStatefulPartitionedCall"dropout_1/PartitionedCall:output:0dense_3_741243dense_3_741245*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_3_layer_call_and_return_conditional_losses_740434?
dense_4/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_4_741248dense_4_741250*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_4_layer_call_and_return_conditional_losses_740450?
tf.__operators__.add_1/AddV2AddV2(dense_3/StatefulPartitionedCall:output:0(dense_4/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_2/PartitionedCallPartitionedCall tf.__operators__.add_1/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_2_layer_call_and_return_conditional_losses_740462?
dense_5/StatefulPartitionedCallStatefulPartitionedCall"dropout_2/PartitionedCall:output:0dense_5_741255dense_5_741257*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_5_layer_call_and_return_conditional_losses_740475?
dense_6/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_6_741260dense_6_741262*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_6_layer_call_and_return_conditional_losses_740491?
tf.__operators__.add_2/AddV2AddV2(dense_5/StatefulPartitionedCall:output:0(dense_6/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_3/PartitionedCallPartitionedCall tf.__operators__.add_2/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_3_layer_call_and_return_conditional_losses_740503?
dense_7/StatefulPartitionedCallStatefulPartitionedCall"dropout_3/PartitionedCall:output:0dense_7_741267dense_7_741269*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_7_layer_call_and_return_conditional_losses_740516?
dense_8/StatefulPartitionedCallStatefulPartitionedCall dropout/PartitionedCall:output:0dense_8_741272dense_8_741274*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_8_layer_call_and_return_conditional_losses_740532?
tf.__operators__.add_3/AddV2AddV2(dense_7/StatefulPartitionedCall:output:0(dense_8/StatefulPartitionedCall:output:0*
T0*(
_output_shapes
:???????????
dropout_4/PartitionedCallPartitionedCall tf.__operators__.add_3/AddV2:z:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_4_layer_call_and_return_conditional_losses_740544?
.readmission_prediction/StatefulPartitionedCallStatefulPartitionedCall"dropout_4/PartitionedCall:output:0readmission_prediction_741279readmission_prediction_741281*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *[
fVRT
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_740557?
IdentityIdentity7readmission_prediction/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp^dense/StatefulPartitionedCall ^dense_1/StatefulPartitionedCall ^dense_2/StatefulPartitionedCall ^dense_3/StatefulPartitionedCall ^dense_4/StatefulPartitionedCall ^dense_5/StatefulPartitionedCall ^dense_6/StatefulPartitionedCall ^dense_7/StatefulPartitionedCall ^dense_8/StatefulPartitionedCall"^embedding/StatefulPartitionedCall$^embedding_1/StatefulPartitionedCall$^embedding_2/StatefulPartitionedCall$^embedding_3/StatefulPartitionedCall$^embedding_4/StatefulPartitionedCall,^layer_normalization/StatefulPartitionedCall/^readmission_prediction/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2B
dense_2/StatefulPartitionedCalldense_2/StatefulPartitionedCall2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2B
dense_4/StatefulPartitionedCalldense_4/StatefulPartitionedCall2B
dense_5/StatefulPartitionedCalldense_5/StatefulPartitionedCall2B
dense_6/StatefulPartitionedCalldense_6/StatefulPartitionedCall2B
dense_7/StatefulPartitionedCalldense_7/StatefulPartitionedCall2B
dense_8/StatefulPartitionedCalldense_8/StatefulPartitionedCall2F
!embedding/StatefulPartitionedCall!embedding/StatefulPartitionedCall2J
#embedding_1/StatefulPartitionedCall#embedding_1/StatefulPartitionedCall2J
#embedding_2/StatefulPartitionedCall#embedding_2/StatefulPartitionedCall2J
#embedding_3/StatefulPartitionedCall#embedding_3/StatefulPartitionedCall2J
#embedding_4/StatefulPartitionedCall#embedding_4/StatefulPartitionedCall2Z
+layer_normalization/StatefulPartitionedCall+layer_normalization/StatefulPartitionedCall2`
.readmission_prediction/StatefulPartitionedCall.readmission_prediction/StatefulPartitionedCall:L H
'
_output_shapes
:?????????

_user_specified_nameDRG:RN
'
_output_shapes
:?????????
#
_user_specified_name	age_group:TP
'
_output_shapes
:?????????
%
_user_specified_namebed_day_dep:ZV
'
_output_shapes
:?????????
+
_user_specified_namedegree_of_urgency:YU
'
_output_shapes
:?????????
*
_user_specified_namediagnosis_code_1:OK
'
_output_shapes
:?????????
 
_user_specified_namegender:]Y
'
_output_shapes
:?????????
.
_user_specified_namemainDiagICD10Chapter
?
?
,__inference_embedding_3_layer_call_fn_741977

inputs
unknown:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_3_layer_call_and_return_conditional_losses_740272s
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
G__inference_embedding_2_layer_call_and_return_conditional_losses_740259

inputs)
embedding_lookup_740253:
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_740253inputs*
Tindices0**
_class 
loc:@embedding_lookup/740253*+
_output_shapes
:?????????*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/740253*+
_output_shapes
:??????????
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
'__inference_i_fall_layer_call_fn_741192
drg
	age_group
bed_day_dep
degree_of_urgency
diagnosis_code_1

gender
maindiagicd10chapter
unknown:
	unknown_0:


	unknown_1:
	unknown_2:
	unknown_3:	?<@
	unknown_4:jj
	unknown_5:j
	unknown_6:j
	unknown_7:j
	unknown_8:	j?
	unknown_9:	?

unknown_10:	j?

unknown_11:	?

unknown_12:
??

unknown_13:	?

unknown_14:	j?

unknown_15:	?

unknown_16:
??

unknown_17:	?

unknown_18:	j?

unknown_19:	?

unknown_20:
??

unknown_21:	?

unknown_22:	j?

unknown_23:	?

unknown_24:	?

unknown_25:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalldrg	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermaindiagicd10chapterunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12
unknown_13
unknown_14
unknown_15
unknown_16
unknown_17
unknown_18
unknown_19
unknown_20
unknown_21
unknown_22
unknown_23
unknown_24
unknown_25*-
Tin&
$2"*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*=
_read_only_resource_inputs
	
 !*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_i_fall_layer_call_and_return_conditional_losses_741070o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:L H
'
_output_shapes
:?????????

_user_specified_nameDRG:RN
'
_output_shapes
:?????????
#
_user_specified_name	age_group:TP
'
_output_shapes
:?????????
%
_user_specified_namebed_day_dep:ZV
'
_output_shapes
:?????????
+
_user_specified_namedegree_of_urgency:YU
'
_output_shapes
:?????????
*
_user_specified_namediagnosis_code_1:OK
'
_output_shapes
:?????????
 
_user_specified_namegender:]Y
'
_output_shapes
:?????????
.
_user_specified_namemainDiagICD10Chapter
?	
?
C__inference_dense_2_layer_call_and_return_conditional_losses_742172

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
?
C__inference_dense_8_layer_call_and_return_conditional_losses_742370

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????`
IdentityIdentityBiasAdd:output:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
(__inference_dense_6_layer_call_fn_742294

inputs
unknown:	j?
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_6_layer_call_and_return_conditional_losses_740491p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
?
A__inference_dense_layer_call_and_return_conditional_losses_740321

inputs0
matmul_readvariableop_resource:jj-
biasadd_readvariableop_resource:j
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:jj*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:j*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:?????????jw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
G__inference_embedding_4_layer_call_and_return_conditional_losses_742002

inputs*
embedding_lookup_741996:	?<@
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_741996inputs*
Tindices0**
_class 
loc:@embedding_lookup/741996*+
_output_shapes
:?????????@*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/741996*+
_output_shapes
:?????????@?
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????@w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????@Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
~
*__inference_embedding_layer_call_fn_741929

inputs
unknown:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_embedding_layer_call_and_return_conditional_losses_740233s
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
(__inference_dense_2_layer_call_fn_742162

inputs
unknown:	j?
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_2_layer_call_and_return_conditional_losses_740409p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
(__inference_dense_1_layer_call_fn_742142

inputs
unknown:	j?
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_1_layer_call_and_return_conditional_losses_740393p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?	
?
A__inference_dense_layer_call_and_return_conditional_losses_742055

inputs0
matmul_readvariableop_resource:jj-
biasadd_readvariableop_resource:j
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:jj*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:j*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:?????????jw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
??
?
B__inference_i_fall_layer_call_and_return_conditional_losses_741734

inputs_drg
inputs_age_group
inputs_bed_day_dep
inputs_degree_of_urgency
inputs_diagnosis_code_1
inputs_gender
inputs_maindiagicd10chapter3
!embedding_embedding_lookup_741592:5
#embedding_1_embedding_lookup_741597:

5
#embedding_2_embedding_lookup_741602:5
#embedding_3_embedding_lookup_741607:6
#embedding_4_embedding_lookup_741612:	?<@6
$dense_matmul_readvariableop_resource:jj3
%dense_biasadd_readvariableop_resource:j?
1layer_normalization_mul_2_readvariableop_resource:j=
/layer_normalization_add_readvariableop_resource:j9
&dense_1_matmul_readvariableop_resource:	j?6
'dense_1_biasadd_readvariableop_resource:	?9
&dense_2_matmul_readvariableop_resource:	j?6
'dense_2_biasadd_readvariableop_resource:	?:
&dense_3_matmul_readvariableop_resource:
??6
'dense_3_biasadd_readvariableop_resource:	?9
&dense_4_matmul_readvariableop_resource:	j?6
'dense_4_biasadd_readvariableop_resource:	?:
&dense_5_matmul_readvariableop_resource:
??6
'dense_5_biasadd_readvariableop_resource:	?9
&dense_6_matmul_readvariableop_resource:	j?6
'dense_6_biasadd_readvariableop_resource:	?:
&dense_7_matmul_readvariableop_resource:
??6
'dense_7_biasadd_readvariableop_resource:	?9
&dense_8_matmul_readvariableop_resource:	j?6
'dense_8_biasadd_readvariableop_resource:	?H
5readmission_prediction_matmul_readvariableop_resource:	?D
6readmission_prediction_biasadd_readvariableop_resource:
identity??dense/BiasAdd/ReadVariableOp?dense/MatMul/ReadVariableOp?dense_1/BiasAdd/ReadVariableOp?dense_1/MatMul/ReadVariableOp?dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOp?dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOp?dense_4/BiasAdd/ReadVariableOp?dense_4/MatMul/ReadVariableOp?dense_5/BiasAdd/ReadVariableOp?dense_5/MatMul/ReadVariableOp?dense_6/BiasAdd/ReadVariableOp?dense_6/MatMul/ReadVariableOp?dense_7/BiasAdd/ReadVariableOp?dense_7/MatMul/ReadVariableOp?dense_8/BiasAdd/ReadVariableOp?dense_8/MatMul/ReadVariableOp?embedding/embedding_lookup?embedding_1/embedding_lookup?embedding_2/embedding_lookup?embedding_3/embedding_lookup?embedding_4/embedding_lookup?&layer_normalization/add/ReadVariableOp?(layer_normalization/mul_2/ReadVariableOp?-readmission_prediction/BiasAdd/ReadVariableOp?,readmission_prediction/MatMul/ReadVariableOpu
tf.stack/stackPackinputs_bed_day_dep*
N*
T0*+
_output_shapes
:?????????*

axiso
tf.stack_1/stackPack
inputs_drg*
N*
T0*+
_output_shapes
:?????????*

axis?
embedding/embedding_lookupResourceGather!embedding_embedding_lookup_741592inputs_gender*
Tindices0*4
_class*
(&loc:@embedding/embedding_lookup/741592*+
_output_shapes
:?????????*
dtype0?
#embedding/embedding_lookup/IdentityIdentity#embedding/embedding_lookup:output:0*
T0*4
_class*
(&loc:@embedding/embedding_lookup/741592*+
_output_shapes
:??????????
%embedding/embedding_lookup/Identity_1Identity,embedding/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
embedding_1/embedding_lookupResourceGather#embedding_1_embedding_lookup_741597inputs_age_group*
Tindices0*6
_class,
*(loc:@embedding_1/embedding_lookup/741597*+
_output_shapes
:?????????
*
dtype0?
%embedding_1/embedding_lookup/IdentityIdentity%embedding_1/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_1/embedding_lookup/741597*+
_output_shapes
:?????????
?
'embedding_1/embedding_lookup/Identity_1Identity.embedding_1/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????
?
embedding_2/embedding_lookupResourceGather#embedding_2_embedding_lookup_741602inputs_degree_of_urgency*
Tindices0*6
_class,
*(loc:@embedding_2/embedding_lookup/741602*+
_output_shapes
:?????????*
dtype0?
%embedding_2/embedding_lookup/IdentityIdentity%embedding_2/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_2/embedding_lookup/741602*+
_output_shapes
:??????????
'embedding_2/embedding_lookup/Identity_1Identity.embedding_2/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
embedding_3/embedding_lookupResourceGather#embedding_3_embedding_lookup_741607inputs_maindiagicd10chapter*
Tindices0*6
_class,
*(loc:@embedding_3/embedding_lookup/741607*+
_output_shapes
:?????????*
dtype0?
%embedding_3/embedding_lookup/IdentityIdentity%embedding_3/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_3/embedding_lookup/741607*+
_output_shapes
:??????????
'embedding_3/embedding_lookup/Identity_1Identity.embedding_3/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
embedding_4/embedding_lookupResourceGather#embedding_4_embedding_lookup_741612inputs_diagnosis_code_1*
Tindices0*6
_class,
*(loc:@embedding_4/embedding_lookup/741612*+
_output_shapes
:?????????@*
dtype0?
%embedding_4/embedding_lookup/IdentityIdentity%embedding_4/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_4/embedding_lookup/741612*+
_output_shapes
:?????????@?
'embedding_4/embedding_lookup/Identity_1Identity.embedding_4/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????@Y
concatenate/concat/axisConst*
_output_shapes
: *
dtype0*
value	B :?
concatenate/concatConcatV2tf.stack/stack:output:0tf.stack_1/stack:output:0.embedding/embedding_lookup/Identity_1:output:00embedding_1/embedding_lookup/Identity_1:output:00embedding_2/embedding_lookup/Identity_1:output:00embedding_3/embedding_lookup/Identity_1:output:00embedding_4/embedding_lookup/Identity_1:output:0 concatenate/concat/axis:output:0*
N*
T0*+
_output_shapes
:?????????j^
flatten/ConstConst*
_output_shapes
:*
dtype0*
valueB"????j   ?
flatten/ReshapeReshapeconcatenate/concat:output:0flatten/Const:output:0*
T0*'
_output_shapes
:?????????j?
dense/MatMul/ReadVariableOpReadVariableOp$dense_matmul_readvariableop_resource*
_output_shapes

:jj*
dtype0?
dense/MatMulMatMulflatten/Reshape:output:0#dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j~
dense/BiasAdd/ReadVariableOpReadVariableOp%dense_biasadd_readvariableop_resource*
_output_shapes
:j*
dtype0?
dense/BiasAddBiasAdddense/MatMul:product:0$dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j_
layer_normalization/ShapeShapedense/BiasAdd:output:0*
T0*
_output_shapes
:q
'layer_normalization/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: s
)layer_normalization/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:s
)layer_normalization/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
!layer_normalization/strided_sliceStridedSlice"layer_normalization/Shape:output:00layer_normalization/strided_slice/stack:output:02layer_normalization/strided_slice/stack_1:output:02layer_normalization/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask[
layer_normalization/mul/xConst*
_output_shapes
: *
dtype0*
value	B :?
layer_normalization/mulMul"layer_normalization/mul/x:output:0*layer_normalization/strided_slice:output:0*
T0*
_output_shapes
: s
)layer_normalization/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:u
+layer_normalization/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:u
+layer_normalization/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
#layer_normalization/strided_slice_1StridedSlice"layer_normalization/Shape:output:02layer_normalization/strided_slice_1/stack:output:04layer_normalization/strided_slice_1/stack_1:output:04layer_normalization/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask]
layer_normalization/mul_1/xConst*
_output_shapes
: *
dtype0*
value	B :?
layer_normalization/mul_1Mul$layer_normalization/mul_1/x:output:0,layer_normalization/strided_slice_1:output:0*
T0*
_output_shapes
: e
#layer_normalization/Reshape/shape/0Const*
_output_shapes
: *
dtype0*
value	B :e
#layer_normalization/Reshape/shape/3Const*
_output_shapes
: *
dtype0*
value	B :?
!layer_normalization/Reshape/shapePack,layer_normalization/Reshape/shape/0:output:0layer_normalization/mul:z:0layer_normalization/mul_1:z:0,layer_normalization/Reshape/shape/3:output:0*
N*
T0*
_output_shapes
:?
layer_normalization/ReshapeReshapedense/BiasAdd:output:0*layer_normalization/Reshape/shape:output:0*
T0*/
_output_shapes
:?????????jr
layer_normalization/ones/packedPacklayer_normalization/mul:z:0*
N*
T0*
_output_shapes
:c
layer_normalization/ones/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ???
layer_normalization/onesFill(layer_normalization/ones/packed:output:0'layer_normalization/ones/Const:output:0*
T0*#
_output_shapes
:?????????s
 layer_normalization/zeros/packedPacklayer_normalization/mul:z:0*
N*
T0*
_output_shapes
:d
layer_normalization/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    ?
layer_normalization/zerosFill)layer_normalization/zeros/packed:output:0(layer_normalization/zeros/Const:output:0*
T0*#
_output_shapes
:?????????\
layer_normalization/ConstConst*
_output_shapes
: *
dtype0*
valueB ^
layer_normalization/Const_1Const*
_output_shapes
: *
dtype0*
valueB ?
$layer_normalization/FusedBatchNormV3FusedBatchNormV3$layer_normalization/Reshape:output:0!layer_normalization/ones:output:0"layer_normalization/zeros:output:0"layer_normalization/Const:output:0$layer_normalization/Const_1:output:0*
T0*
U0*o
_output_shapes]
[:?????????j:?????????:?????????:?????????:?????????:*
data_formatNCHW*
epsilon%o?:?
layer_normalization/Reshape_1Reshape(layer_normalization/FusedBatchNormV3:y:0"layer_normalization/Shape:output:0*
T0*'
_output_shapes
:?????????j?
(layer_normalization/mul_2/ReadVariableOpReadVariableOp1layer_normalization_mul_2_readvariableop_resource*
_output_shapes
:j*
dtype0?
layer_normalization/mul_2Mul&layer_normalization/Reshape_1:output:00layer_normalization/mul_2/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j?
&layer_normalization/add/ReadVariableOpReadVariableOp/layer_normalization_add_readvariableop_resource*
_output_shapes
:j*
dtype0?
layer_normalization/addAddV2layer_normalization/mul_2:z:0.layer_normalization/add/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jk
dropout/IdentityIdentitylayer_normalization/add:z:0*
T0*'
_output_shapes
:?????????j?
dense_1/MatMul/ReadVariableOpReadVariableOp&dense_1_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_1/MatMulMatMuldropout/Identity:output:0%dense_1/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_1/BiasAdd/ReadVariableOpReadVariableOp'dense_1_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_1/BiasAddBiasAdddense_1/MatMul:product:0&dense_1/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????~
dense_1/leaky_re_lu/LeakyRelu	LeakyReludense_1/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_2/MatMulMatMuldropout/Identity:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add/AddV2AddV2+dense_1/leaky_re_lu/LeakyRelu:activations:0dense_2/BiasAdd:output:0*
T0*(
_output_shapes
:??????????q
dropout_1/IdentityIdentitytf.__operators__.add/AddV2:z:0*
T0*(
_output_shapes
:???????????
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
dense_3/MatMulMatMuldropout_1/Identity:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_3/leaky_re_lu_1/LeakyRelu	LeakyReludense_3/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_4/MatMul/ReadVariableOpReadVariableOp&dense_4_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_4/MatMulMatMuldropout/Identity:output:0%dense_4/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_4/BiasAdd/ReadVariableOpReadVariableOp'dense_4_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_4/BiasAddBiasAdddense_4/MatMul:product:0&dense_4/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add_1/AddV2AddV2-dense_3/leaky_re_lu_1/LeakyRelu:activations:0dense_4/BiasAdd:output:0*
T0*(
_output_shapes
:??????????s
dropout_2/IdentityIdentity tf.__operators__.add_1/AddV2:z:0*
T0*(
_output_shapes
:???????????
dense_5/MatMul/ReadVariableOpReadVariableOp&dense_5_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
dense_5/MatMulMatMuldropout_2/Identity:output:0%dense_5/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_5/BiasAdd/ReadVariableOpReadVariableOp'dense_5_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_5/BiasAddBiasAdddense_5/MatMul:product:0&dense_5/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_5/leaky_re_lu_2/LeakyRelu	LeakyReludense_5/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_6/MatMul/ReadVariableOpReadVariableOp&dense_6_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_6/MatMulMatMuldropout/Identity:output:0%dense_6/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_6/BiasAdd/ReadVariableOpReadVariableOp'dense_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_6/BiasAddBiasAdddense_6/MatMul:product:0&dense_6/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add_2/AddV2AddV2-dense_5/leaky_re_lu_2/LeakyRelu:activations:0dense_6/BiasAdd:output:0*
T0*(
_output_shapes
:??????????s
dropout_3/IdentityIdentity tf.__operators__.add_2/AddV2:z:0*
T0*(
_output_shapes
:???????????
dense_7/MatMul/ReadVariableOpReadVariableOp&dense_7_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
dense_7/MatMulMatMuldropout_3/Identity:output:0%dense_7/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_7/BiasAdd/ReadVariableOpReadVariableOp'dense_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_7/BiasAddBiasAdddense_7/MatMul:product:0&dense_7/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_7/leaky_re_lu_3/LeakyRelu	LeakyReludense_7/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_8/MatMulMatMuldropout/Identity:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add_3/AddV2AddV2-dense_7/leaky_re_lu_3/LeakyRelu:activations:0dense_8/BiasAdd:output:0*
T0*(
_output_shapes
:??????????s
dropout_4/IdentityIdentity tf.__operators__.add_3/AddV2:z:0*
T0*(
_output_shapes
:???????????
,readmission_prediction/MatMul/ReadVariableOpReadVariableOp5readmission_prediction_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype0?
readmission_prediction/MatMulMatMuldropout_4/Identity:output:04readmission_prediction/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
-readmission_prediction/BiasAdd/ReadVariableOpReadVariableOp6readmission_prediction_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
readmission_prediction/BiasAddBiasAdd'readmission_prediction/MatMul:product:05readmission_prediction/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
readmission_prediction/SigmoidSigmoid'readmission_prediction/BiasAdd:output:0*
T0*'
_output_shapes
:?????????q
IdentityIdentity"readmission_prediction/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp^dense/BiasAdd/ReadVariableOp^dense/MatMul/ReadVariableOp^dense_1/BiasAdd/ReadVariableOp^dense_1/MatMul/ReadVariableOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp^dense_4/BiasAdd/ReadVariableOp^dense_4/MatMul/ReadVariableOp^dense_5/BiasAdd/ReadVariableOp^dense_5/MatMul/ReadVariableOp^dense_6/BiasAdd/ReadVariableOp^dense_6/MatMul/ReadVariableOp^dense_7/BiasAdd/ReadVariableOp^dense_7/MatMul/ReadVariableOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp^embedding/embedding_lookup^embedding_1/embedding_lookup^embedding_2/embedding_lookup^embedding_3/embedding_lookup^embedding_4/embedding_lookup'^layer_normalization/add/ReadVariableOp)^layer_normalization/mul_2/ReadVariableOp.^readmission_prediction/BiasAdd/ReadVariableOp-^readmission_prediction/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2<
dense/BiasAdd/ReadVariableOpdense/BiasAdd/ReadVariableOp2:
dense/MatMul/ReadVariableOpdense/MatMul/ReadVariableOp2@
dense_1/BiasAdd/ReadVariableOpdense_1/BiasAdd/ReadVariableOp2>
dense_1/MatMul/ReadVariableOpdense_1/MatMul/ReadVariableOp2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp2@
dense_4/BiasAdd/ReadVariableOpdense_4/BiasAdd/ReadVariableOp2>
dense_4/MatMul/ReadVariableOpdense_4/MatMul/ReadVariableOp2@
dense_5/BiasAdd/ReadVariableOpdense_5/BiasAdd/ReadVariableOp2>
dense_5/MatMul/ReadVariableOpdense_5/MatMul/ReadVariableOp2@
dense_6/BiasAdd/ReadVariableOpdense_6/BiasAdd/ReadVariableOp2>
dense_6/MatMul/ReadVariableOpdense_6/MatMul/ReadVariableOp2@
dense_7/BiasAdd/ReadVariableOpdense_7/BiasAdd/ReadVariableOp2>
dense_7/MatMul/ReadVariableOpdense_7/MatMul/ReadVariableOp2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp28
embedding/embedding_lookupembedding/embedding_lookup2<
embedding_1/embedding_lookupembedding_1/embedding_lookup2<
embedding_2/embedding_lookupembedding_2/embedding_lookup2<
embedding_3/embedding_lookupembedding_3/embedding_lookup2<
embedding_4/embedding_lookupembedding_4/embedding_lookup2P
&layer_normalization/add/ReadVariableOp&layer_normalization/add/ReadVariableOp2T
(layer_normalization/mul_2/ReadVariableOp(layer_normalization/mul_2/ReadVariableOp2^
-readmission_prediction/BiasAdd/ReadVariableOp-readmission_prediction/BiasAdd/ReadVariableOp2\
,readmission_prediction/MatMul/ReadVariableOp,readmission_prediction/MatMul/ReadVariableOp:S O
'
_output_shapes
:?????????
$
_user_specified_name
inputs/DRG:YU
'
_output_shapes
:?????????
*
_user_specified_nameinputs/age_group:[W
'
_output_shapes
:?????????
,
_user_specified_nameinputs/bed_day_dep:a]
'
_output_shapes
:?????????
2
_user_specified_nameinputs/degree_of_urgency:`\
'
_output_shapes
:?????????
1
_user_specified_nameinputs/diagnosis_code_1:VR
'
_output_shapes
:?????????
'
_user_specified_nameinputs/gender:d`
'
_output_shapes
:?????????
5
_user_specified_nameinputs/mainDiagICD10Chapter
?
?
O__inference_layer_normalization_layer_call_and_return_conditional_losses_740369

inputs+
mul_2_readvariableop_resource:j)
add_readvariableop_resource:j
identity??add/ReadVariableOp?mul_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskG
mul/xConst*
_output_shapes
: *
dtype0*
value	B :S
mulMulmul/x:output:0strided_slice:output:0*
T0*
_output_shapes
: _
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
strided_slice_1StridedSliceShape:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskI
mul_1/xConst*
_output_shapes
: *
dtype0*
value	B :Y
mul_1Mulmul_1/x:output:0strided_slice_1:output:0*
T0*
_output_shapes
: Q
Reshape/shape/0Const*
_output_shapes
: *
dtype0*
value	B :Q
Reshape/shape/3Const*
_output_shapes
: *
dtype0*
value	B :?
Reshape/shapePackReshape/shape/0:output:0mul:z:0	mul_1:z:0Reshape/shape/3:output:0*
N*
T0*
_output_shapes
:l
ReshapeReshapeinputsReshape/shape:output:0*
T0*/
_output_shapes
:?????????jJ
ones/packedPackmul:z:0*
N*
T0*
_output_shapes
:O

ones/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??e
onesFillones/packed:output:0ones/Const:output:0*
T0*#
_output_shapes
:?????????K
zeros/packedPackmul:z:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    h
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*#
_output_shapes
:?????????H
ConstConst*
_output_shapes
: *
dtype0*
valueB J
Const_1Const*
_output_shapes
: *
dtype0*
valueB ?
FusedBatchNormV3FusedBatchNormV3Reshape:output:0ones:output:0zeros:output:0Const:output:0Const_1:output:0*
T0*
U0*o
_output_shapes]
[:?????????j:?????????:?????????:?????????:?????????:*
data_formatNCHW*
epsilon%o?:l
	Reshape_1ReshapeFusedBatchNormV3:y:0Shape:output:0*
T0*'
_output_shapes
:?????????jn
mul_2/ReadVariableOpReadVariableOpmul_2_readvariableop_resource*
_output_shapes
:j*
dtype0p
mul_2MulReshape_1:output:0mul_2/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jj
add/ReadVariableOpReadVariableOpadd_readvariableop_resource*
_output_shapes
:j*
dtype0e
addAddV2	mul_2:z:0add/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jV
IdentityIdentityadd:z:0^NoOp*
T0*'
_output_shapes
:?????????jr
NoOpNoOp^add/ReadVariableOp^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 2(
add/ReadVariableOpadd/ReadVariableOp2,
mul_2/ReadVariableOpmul_2/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?

?
C__inference_dense_1_layer_call_and_return_conditional_losses_740393

inputs1
matmul_readvariableop_resource:	j?.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????n
leaky_re_lu/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>s
IdentityIdentity#leaky_re_lu/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
a
(__inference_dropout_layer_call_fn_742116

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dropout_layer_call_and_return_conditional_losses_740823o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????j`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????j22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
E__inference_embedding_layer_call_and_return_conditional_losses_740233

inputs)
embedding_lookup_740227:
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_740227inputs*
Tindices0**
_class 
loc:@embedding_lookup/740227*+
_output_shapes
:?????????*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/740227*+
_output_shapes
:??????????
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_740557

inputs1
matmul_readvariableop_resource:	?-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????V
SigmoidSigmoidBiasAdd:output:0*
T0*'
_output_shapes
:?????????Z
IdentityIdentitySigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
F
*__inference_dropout_1_layer_call_fn_742177

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_1_layer_call_and_return_conditional_losses_740421a
IdentityIdentityPartitionedCall:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
E__inference_dropout_1_layer_call_and_return_conditional_losses_740421

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
b
C__inference_dropout_layer_call_and_return_conditional_losses_742133

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????d
dropout/MulMulinputsdropout/Const:output:0*
T0*'
_output_shapes
:?????????jC
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:?????????j*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????jo
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????ji
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:?????????jY
IdentityIdentitydropout/Mul_1:z:0*
T0*'
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????j:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
7__inference_readmission_prediction_layer_call_fn_742406

inputs
unknown:	?
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *[
fVRT
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_740557o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
E__inference_dropout_4_layer_call_and_return_conditional_losses_740544

inputs

identity_1O
IdentityIdentityinputs*
T0*(
_output_shapes
:??????????\

Identity_1IdentityIdentity:output:0*
T0*(
_output_shapes
:??????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
*__inference_dropout_4_layer_call_fn_742380

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_4_layer_call_and_return_conditional_losses_740651p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
*__inference_dropout_2_layer_call_fn_742248

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_2_layer_call_and_return_conditional_losses_740737p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?

?
G__inference_concatenate_layer_call_and_return_conditional_losses_742025
inputs_0
inputs_1
inputs_2
inputs_3
inputs_4
inputs_5
inputs_6
identityM
concat/axisConst*
_output_shapes
: *
dtype0*
value	B :?
concatConcatV2inputs_0inputs_1inputs_2inputs_3inputs_4inputs_5inputs_6concat/axis:output:0*
N*
T0*+
_output_shapes
:?????????j[
IdentityIdentityconcat:output:0*
T0*+
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????
:?????????:?????????:?????????@:U Q
+
_output_shapes
:?????????
"
_user_specified_name
inputs/0:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/1:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/2:UQ
+
_output_shapes
:?????????

"
_user_specified_name
inputs/3:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/4:UQ
+
_output_shapes
:?????????
"
_user_specified_name
inputs/5:UQ
+
_output_shapes
:?????????@
"
_user_specified_name
inputs/6
?	
d
E__inference_dropout_1_layer_call_and_return_conditional_losses_742199

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
c
*__inference_dropout_1_layer_call_fn_742182

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_1_layer_call_and_return_conditional_losses_740780p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?

?
C__inference_dense_7_layer_call_and_return_conditional_losses_740516

inputs2
matmul_readvariableop_resource:
??.
biasadd_readvariableop_resource:	?
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????s
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????p
leaky_re_lu_3/LeakyRelu	LeakyReluBiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>u
IdentityIdentity%leaky_re_lu_3/LeakyRelu:activations:0^NoOp*
T0*(
_output_shapes
:??????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
b
C__inference_dropout_layer_call_and_return_conditional_losses_740823

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????d
dropout/MulMulinputsdropout/Const:output:0*
T0*'
_output_shapes
:?????????jC
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:?????????j*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????jo
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????ji
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:?????????jY
IdentityIdentitydropout/Mul_1:z:0*
T0*'
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????j:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
D
(__inference_dropout_layer_call_fn_742111

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????j* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dropout_layer_call_and_return_conditional_losses_740380`
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????j:O K
'
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
_
C__inference_flatten_layer_call_and_return_conditional_losses_740309

inputs
identityV
ConstConst*
_output_shapes
:*
dtype0*
valueB"????j   \
ReshapeReshapeinputsConst:output:0*
T0*'
_output_shapes
:?????????jX
IdentityIdentityReshape:output:0*
T0*'
_output_shapes
:?????????j"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????j:S O
+
_output_shapes
:?????????j
 
_user_specified_nameinputs
?
?
G__inference_embedding_4_layer_call_and_return_conditional_losses_740285

inputs*
embedding_lookup_740279:	?<@
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_740279inputs*
Tindices0**
_class 
loc:@embedding_lookup/740279*+
_output_shapes
:?????????@*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/740279*+
_output_shapes
:?????????@?
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????@w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????@Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
??
?
B__inference_i_fall_layer_call_and_return_conditional_losses_741922

inputs_drg
inputs_age_group
inputs_bed_day_dep
inputs_degree_of_urgency
inputs_diagnosis_code_1
inputs_gender
inputs_maindiagicd10chapter3
!embedding_embedding_lookup_741745:5
#embedding_1_embedding_lookup_741750:

5
#embedding_2_embedding_lookup_741755:5
#embedding_3_embedding_lookup_741760:6
#embedding_4_embedding_lookup_741765:	?<@6
$dense_matmul_readvariableop_resource:jj3
%dense_biasadd_readvariableop_resource:j?
1layer_normalization_mul_2_readvariableop_resource:j=
/layer_normalization_add_readvariableop_resource:j9
&dense_1_matmul_readvariableop_resource:	j?6
'dense_1_biasadd_readvariableop_resource:	?9
&dense_2_matmul_readvariableop_resource:	j?6
'dense_2_biasadd_readvariableop_resource:	?:
&dense_3_matmul_readvariableop_resource:
??6
'dense_3_biasadd_readvariableop_resource:	?9
&dense_4_matmul_readvariableop_resource:	j?6
'dense_4_biasadd_readvariableop_resource:	?:
&dense_5_matmul_readvariableop_resource:
??6
'dense_5_biasadd_readvariableop_resource:	?9
&dense_6_matmul_readvariableop_resource:	j?6
'dense_6_biasadd_readvariableop_resource:	?:
&dense_7_matmul_readvariableop_resource:
??6
'dense_7_biasadd_readvariableop_resource:	?9
&dense_8_matmul_readvariableop_resource:	j?6
'dense_8_biasadd_readvariableop_resource:	?H
5readmission_prediction_matmul_readvariableop_resource:	?D
6readmission_prediction_biasadd_readvariableop_resource:
identity??dense/BiasAdd/ReadVariableOp?dense/MatMul/ReadVariableOp?dense_1/BiasAdd/ReadVariableOp?dense_1/MatMul/ReadVariableOp?dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOp?dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOp?dense_4/BiasAdd/ReadVariableOp?dense_4/MatMul/ReadVariableOp?dense_5/BiasAdd/ReadVariableOp?dense_5/MatMul/ReadVariableOp?dense_6/BiasAdd/ReadVariableOp?dense_6/MatMul/ReadVariableOp?dense_7/BiasAdd/ReadVariableOp?dense_7/MatMul/ReadVariableOp?dense_8/BiasAdd/ReadVariableOp?dense_8/MatMul/ReadVariableOp?embedding/embedding_lookup?embedding_1/embedding_lookup?embedding_2/embedding_lookup?embedding_3/embedding_lookup?embedding_4/embedding_lookup?&layer_normalization/add/ReadVariableOp?(layer_normalization/mul_2/ReadVariableOp?-readmission_prediction/BiasAdd/ReadVariableOp?,readmission_prediction/MatMul/ReadVariableOpu
tf.stack/stackPackinputs_bed_day_dep*
N*
T0*+
_output_shapes
:?????????*

axiso
tf.stack_1/stackPack
inputs_drg*
N*
T0*+
_output_shapes
:?????????*

axis?
embedding/embedding_lookupResourceGather!embedding_embedding_lookup_741745inputs_gender*
Tindices0*4
_class*
(&loc:@embedding/embedding_lookup/741745*+
_output_shapes
:?????????*
dtype0?
#embedding/embedding_lookup/IdentityIdentity#embedding/embedding_lookup:output:0*
T0*4
_class*
(&loc:@embedding/embedding_lookup/741745*+
_output_shapes
:??????????
%embedding/embedding_lookup/Identity_1Identity,embedding/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
embedding_1/embedding_lookupResourceGather#embedding_1_embedding_lookup_741750inputs_age_group*
Tindices0*6
_class,
*(loc:@embedding_1/embedding_lookup/741750*+
_output_shapes
:?????????
*
dtype0?
%embedding_1/embedding_lookup/IdentityIdentity%embedding_1/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_1/embedding_lookup/741750*+
_output_shapes
:?????????
?
'embedding_1/embedding_lookup/Identity_1Identity.embedding_1/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????
?
embedding_2/embedding_lookupResourceGather#embedding_2_embedding_lookup_741755inputs_degree_of_urgency*
Tindices0*6
_class,
*(loc:@embedding_2/embedding_lookup/741755*+
_output_shapes
:?????????*
dtype0?
%embedding_2/embedding_lookup/IdentityIdentity%embedding_2/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_2/embedding_lookup/741755*+
_output_shapes
:??????????
'embedding_2/embedding_lookup/Identity_1Identity.embedding_2/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
embedding_3/embedding_lookupResourceGather#embedding_3_embedding_lookup_741760inputs_maindiagicd10chapter*
Tindices0*6
_class,
*(loc:@embedding_3/embedding_lookup/741760*+
_output_shapes
:?????????*
dtype0?
%embedding_3/embedding_lookup/IdentityIdentity%embedding_3/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_3/embedding_lookup/741760*+
_output_shapes
:??????????
'embedding_3/embedding_lookup/Identity_1Identity.embedding_3/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:??????????
embedding_4/embedding_lookupResourceGather#embedding_4_embedding_lookup_741765inputs_diagnosis_code_1*
Tindices0*6
_class,
*(loc:@embedding_4/embedding_lookup/741765*+
_output_shapes
:?????????@*
dtype0?
%embedding_4/embedding_lookup/IdentityIdentity%embedding_4/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_4/embedding_lookup/741765*+
_output_shapes
:?????????@?
'embedding_4/embedding_lookup/Identity_1Identity.embedding_4/embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????@Y
concatenate/concat/axisConst*
_output_shapes
: *
dtype0*
value	B :?
concatenate/concatConcatV2tf.stack/stack:output:0tf.stack_1/stack:output:0.embedding/embedding_lookup/Identity_1:output:00embedding_1/embedding_lookup/Identity_1:output:00embedding_2/embedding_lookup/Identity_1:output:00embedding_3/embedding_lookup/Identity_1:output:00embedding_4/embedding_lookup/Identity_1:output:0 concatenate/concat/axis:output:0*
N*
T0*+
_output_shapes
:?????????j^
flatten/ConstConst*
_output_shapes
:*
dtype0*
valueB"????j   ?
flatten/ReshapeReshapeconcatenate/concat:output:0flatten/Const:output:0*
T0*'
_output_shapes
:?????????j?
dense/MatMul/ReadVariableOpReadVariableOp$dense_matmul_readvariableop_resource*
_output_shapes

:jj*
dtype0?
dense/MatMulMatMulflatten/Reshape:output:0#dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j~
dense/BiasAdd/ReadVariableOpReadVariableOp%dense_biasadd_readvariableop_resource*
_output_shapes
:j*
dtype0?
dense/BiasAddBiasAdddense/MatMul:product:0$dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j_
layer_normalization/ShapeShapedense/BiasAdd:output:0*
T0*
_output_shapes
:q
'layer_normalization/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: s
)layer_normalization/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:s
)layer_normalization/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
!layer_normalization/strided_sliceStridedSlice"layer_normalization/Shape:output:00layer_normalization/strided_slice/stack:output:02layer_normalization/strided_slice/stack_1:output:02layer_normalization/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask[
layer_normalization/mul/xConst*
_output_shapes
: *
dtype0*
value	B :?
layer_normalization/mulMul"layer_normalization/mul/x:output:0*layer_normalization/strided_slice:output:0*
T0*
_output_shapes
: s
)layer_normalization/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:u
+layer_normalization/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:u
+layer_normalization/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
#layer_normalization/strided_slice_1StridedSlice"layer_normalization/Shape:output:02layer_normalization/strided_slice_1/stack:output:04layer_normalization/strided_slice_1/stack_1:output:04layer_normalization/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask]
layer_normalization/mul_1/xConst*
_output_shapes
: *
dtype0*
value	B :?
layer_normalization/mul_1Mul$layer_normalization/mul_1/x:output:0,layer_normalization/strided_slice_1:output:0*
T0*
_output_shapes
: e
#layer_normalization/Reshape/shape/0Const*
_output_shapes
: *
dtype0*
value	B :e
#layer_normalization/Reshape/shape/3Const*
_output_shapes
: *
dtype0*
value	B :?
!layer_normalization/Reshape/shapePack,layer_normalization/Reshape/shape/0:output:0layer_normalization/mul:z:0layer_normalization/mul_1:z:0,layer_normalization/Reshape/shape/3:output:0*
N*
T0*
_output_shapes
:?
layer_normalization/ReshapeReshapedense/BiasAdd:output:0*layer_normalization/Reshape/shape:output:0*
T0*/
_output_shapes
:?????????jr
layer_normalization/ones/packedPacklayer_normalization/mul:z:0*
N*
T0*
_output_shapes
:c
layer_normalization/ones/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ???
layer_normalization/onesFill(layer_normalization/ones/packed:output:0'layer_normalization/ones/Const:output:0*
T0*#
_output_shapes
:?????????s
 layer_normalization/zeros/packedPacklayer_normalization/mul:z:0*
N*
T0*
_output_shapes
:d
layer_normalization/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    ?
layer_normalization/zerosFill)layer_normalization/zeros/packed:output:0(layer_normalization/zeros/Const:output:0*
T0*#
_output_shapes
:?????????\
layer_normalization/ConstConst*
_output_shapes
: *
dtype0*
valueB ^
layer_normalization/Const_1Const*
_output_shapes
: *
dtype0*
valueB ?
$layer_normalization/FusedBatchNormV3FusedBatchNormV3$layer_normalization/Reshape:output:0!layer_normalization/ones:output:0"layer_normalization/zeros:output:0"layer_normalization/Const:output:0$layer_normalization/Const_1:output:0*
T0*
U0*o
_output_shapes]
[:?????????j:?????????:?????????:?????????:?????????:*
data_formatNCHW*
epsilon%o?:?
layer_normalization/Reshape_1Reshape(layer_normalization/FusedBatchNormV3:y:0"layer_normalization/Shape:output:0*
T0*'
_output_shapes
:?????????j?
(layer_normalization/mul_2/ReadVariableOpReadVariableOp1layer_normalization_mul_2_readvariableop_resource*
_output_shapes
:j*
dtype0?
layer_normalization/mul_2Mul&layer_normalization/Reshape_1:output:00layer_normalization/mul_2/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????j?
&layer_normalization/add/ReadVariableOpReadVariableOp/layer_normalization_add_readvariableop_resource*
_output_shapes
:j*
dtype0?
layer_normalization/addAddV2layer_normalization/mul_2:z:0.layer_normalization/add/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????jZ
dropout/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *?????
dropout/dropout/MulMullayer_normalization/add:z:0dropout/dropout/Const:output:0*
T0*'
_output_shapes
:?????????j`
dropout/dropout/ShapeShapelayer_normalization/add:z:0*
T0*
_output_shapes
:?
,dropout/dropout/random_uniform/RandomUniformRandomUniformdropout/dropout/Shape:output:0*
T0*'
_output_shapes
:?????????j*
dtype0c
dropout/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/dropout/GreaterEqualGreaterEqual5dropout/dropout/random_uniform/RandomUniform:output:0'dropout/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????j
dropout/dropout/CastCast dropout/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????j?
dropout/dropout/Mul_1Muldropout/dropout/Mul:z:0dropout/dropout/Cast:y:0*
T0*'
_output_shapes
:?????????j?
dense_1/MatMul/ReadVariableOpReadVariableOp&dense_1_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_1/MatMulMatMuldropout/dropout/Mul_1:z:0%dense_1/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_1/BiasAdd/ReadVariableOpReadVariableOp'dense_1_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_1/BiasAddBiasAdddense_1/MatMul:product:0&dense_1/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:??????????~
dense_1/leaky_re_lu/LeakyRelu	LeakyReludense_1/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_2/MatMulMatMuldropout/dropout/Mul_1:z:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add/AddV2AddV2+dense_1/leaky_re_lu/LeakyRelu:activations:0dense_2/BiasAdd:output:0*
T0*(
_output_shapes
:??????????\
dropout_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *?????
dropout_1/dropout/MulMultf.__operators__.add/AddV2:z:0 dropout_1/dropout/Const:output:0*
T0*(
_output_shapes
:??????????e
dropout_1/dropout/ShapeShapetf.__operators__.add/AddV2:z:0*
T0*
_output_shapes
:?
.dropout_1/dropout/random_uniform/RandomUniformRandomUniform dropout_1/dropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0e
 dropout_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout_1/dropout/GreaterEqualGreaterEqual7dropout_1/dropout/random_uniform/RandomUniform:output:0)dropout_1/dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:???????????
dropout_1/dropout/CastCast"dropout_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:???????????
dropout_1/dropout/Mul_1Muldropout_1/dropout/Mul:z:0dropout_1/dropout/Cast:y:0*
T0*(
_output_shapes
:???????????
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
dense_3/MatMulMatMuldropout_1/dropout/Mul_1:z:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_3/leaky_re_lu_1/LeakyRelu	LeakyReludense_3/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_4/MatMul/ReadVariableOpReadVariableOp&dense_4_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_4/MatMulMatMuldropout/dropout/Mul_1:z:0%dense_4/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_4/BiasAdd/ReadVariableOpReadVariableOp'dense_4_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_4/BiasAddBiasAdddense_4/MatMul:product:0&dense_4/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add_1/AddV2AddV2-dense_3/leaky_re_lu_1/LeakyRelu:activations:0dense_4/BiasAdd:output:0*
T0*(
_output_shapes
:??????????\
dropout_2/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *?????
dropout_2/dropout/MulMul tf.__operators__.add_1/AddV2:z:0 dropout_2/dropout/Const:output:0*
T0*(
_output_shapes
:??????????g
dropout_2/dropout/ShapeShape tf.__operators__.add_1/AddV2:z:0*
T0*
_output_shapes
:?
.dropout_2/dropout/random_uniform/RandomUniformRandomUniform dropout_2/dropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0e
 dropout_2/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout_2/dropout/GreaterEqualGreaterEqual7dropout_2/dropout/random_uniform/RandomUniform:output:0)dropout_2/dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:???????????
dropout_2/dropout/CastCast"dropout_2/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:???????????
dropout_2/dropout/Mul_1Muldropout_2/dropout/Mul:z:0dropout_2/dropout/Cast:y:0*
T0*(
_output_shapes
:???????????
dense_5/MatMul/ReadVariableOpReadVariableOp&dense_5_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
dense_5/MatMulMatMuldropout_2/dropout/Mul_1:z:0%dense_5/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_5/BiasAdd/ReadVariableOpReadVariableOp'dense_5_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_5/BiasAddBiasAdddense_5/MatMul:product:0&dense_5/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_5/leaky_re_lu_2/LeakyRelu	LeakyReludense_5/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_6/MatMul/ReadVariableOpReadVariableOp&dense_6_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_6/MatMulMatMuldropout/dropout/Mul_1:z:0%dense_6/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_6/BiasAdd/ReadVariableOpReadVariableOp'dense_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_6/BiasAddBiasAdddense_6/MatMul:product:0&dense_6/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add_2/AddV2AddV2-dense_5/leaky_re_lu_2/LeakyRelu:activations:0dense_6/BiasAdd:output:0*
T0*(
_output_shapes
:??????????\
dropout_3/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *?????
dropout_3/dropout/MulMul tf.__operators__.add_2/AddV2:z:0 dropout_3/dropout/Const:output:0*
T0*(
_output_shapes
:??????????g
dropout_3/dropout/ShapeShape tf.__operators__.add_2/AddV2:z:0*
T0*
_output_shapes
:?
.dropout_3/dropout/random_uniform/RandomUniformRandomUniform dropout_3/dropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0e
 dropout_3/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout_3/dropout/GreaterEqualGreaterEqual7dropout_3/dropout/random_uniform/RandomUniform:output:0)dropout_3/dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:???????????
dropout_3/dropout/CastCast"dropout_3/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:???????????
dropout_3/dropout/Mul_1Muldropout_3/dropout/Mul:z:0dropout_3/dropout/Cast:y:0*
T0*(
_output_shapes
:???????????
dense_7/MatMul/ReadVariableOpReadVariableOp&dense_7_matmul_readvariableop_resource* 
_output_shapes
:
??*
dtype0?
dense_7/MatMulMatMuldropout_3/dropout/Mul_1:z:0%dense_7/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_7/BiasAdd/ReadVariableOpReadVariableOp'dense_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_7/BiasAddBiasAdddense_7/MatMul:product:0&dense_7/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_7/leaky_re_lu_3/LeakyRelu	LeakyReludense_7/BiasAdd:output:0*(
_output_shapes
:??????????*
alpha%???>?
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes
:	j?*
dtype0?
dense_8/MatMulMatMuldropout/dropout/Mul_1:z:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype0?
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:???????????
tf.__operators__.add_3/AddV2AddV2-dense_7/leaky_re_lu_3/LeakyRelu:activations:0dense_8/BiasAdd:output:0*
T0*(
_output_shapes
:??????????\
dropout_4/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *?????
dropout_4/dropout/MulMul tf.__operators__.add_3/AddV2:z:0 dropout_4/dropout/Const:output:0*
T0*(
_output_shapes
:??????????g
dropout_4/dropout/ShapeShape tf.__operators__.add_3/AddV2:z:0*
T0*
_output_shapes
:?
.dropout_4/dropout/random_uniform/RandomUniformRandomUniform dropout_4/dropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0e
 dropout_4/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout_4/dropout/GreaterEqualGreaterEqual7dropout_4/dropout/random_uniform/RandomUniform:output:0)dropout_4/dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:???????????
dropout_4/dropout/CastCast"dropout_4/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:???????????
dropout_4/dropout/Mul_1Muldropout_4/dropout/Mul:z:0dropout_4/dropout/Cast:y:0*
T0*(
_output_shapes
:???????????
,readmission_prediction/MatMul/ReadVariableOpReadVariableOp5readmission_prediction_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype0?
readmission_prediction/MatMulMatMuldropout_4/dropout/Mul_1:z:04readmission_prediction/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
-readmission_prediction/BiasAdd/ReadVariableOpReadVariableOp6readmission_prediction_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
readmission_prediction/BiasAddBiasAdd'readmission_prediction/MatMul:product:05readmission_prediction/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
readmission_prediction/SigmoidSigmoid'readmission_prediction/BiasAdd:output:0*
T0*'
_output_shapes
:?????????q
IdentityIdentity"readmission_prediction/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp^dense/BiasAdd/ReadVariableOp^dense/MatMul/ReadVariableOp^dense_1/BiasAdd/ReadVariableOp^dense_1/MatMul/ReadVariableOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp^dense_4/BiasAdd/ReadVariableOp^dense_4/MatMul/ReadVariableOp^dense_5/BiasAdd/ReadVariableOp^dense_5/MatMul/ReadVariableOp^dense_6/BiasAdd/ReadVariableOp^dense_6/MatMul/ReadVariableOp^dense_7/BiasAdd/ReadVariableOp^dense_7/MatMul/ReadVariableOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp^embedding/embedding_lookup^embedding_1/embedding_lookup^embedding_2/embedding_lookup^embedding_3/embedding_lookup^embedding_4/embedding_lookup'^layer_normalization/add/ReadVariableOp)^layer_normalization/mul_2/ReadVariableOp.^readmission_prediction/BiasAdd/ReadVariableOp-^readmission_prediction/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 2<
dense/BiasAdd/ReadVariableOpdense/BiasAdd/ReadVariableOp2:
dense/MatMul/ReadVariableOpdense/MatMul/ReadVariableOp2@
dense_1/BiasAdd/ReadVariableOpdense_1/BiasAdd/ReadVariableOp2>
dense_1/MatMul/ReadVariableOpdense_1/MatMul/ReadVariableOp2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp2@
dense_4/BiasAdd/ReadVariableOpdense_4/BiasAdd/ReadVariableOp2>
dense_4/MatMul/ReadVariableOpdense_4/MatMul/ReadVariableOp2@
dense_5/BiasAdd/ReadVariableOpdense_5/BiasAdd/ReadVariableOp2>
dense_5/MatMul/ReadVariableOpdense_5/MatMul/ReadVariableOp2@
dense_6/BiasAdd/ReadVariableOpdense_6/BiasAdd/ReadVariableOp2>
dense_6/MatMul/ReadVariableOpdense_6/MatMul/ReadVariableOp2@
dense_7/BiasAdd/ReadVariableOpdense_7/BiasAdd/ReadVariableOp2>
dense_7/MatMul/ReadVariableOpdense_7/MatMul/ReadVariableOp2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp28
embedding/embedding_lookupembedding/embedding_lookup2<
embedding_1/embedding_lookupembedding_1/embedding_lookup2<
embedding_2/embedding_lookupembedding_2/embedding_lookup2<
embedding_3/embedding_lookupembedding_3/embedding_lookup2<
embedding_4/embedding_lookupembedding_4/embedding_lookup2P
&layer_normalization/add/ReadVariableOp&layer_normalization/add/ReadVariableOp2T
(layer_normalization/mul_2/ReadVariableOp(layer_normalization/mul_2/ReadVariableOp2^
-readmission_prediction/BiasAdd/ReadVariableOp-readmission_prediction/BiasAdd/ReadVariableOp2\
,readmission_prediction/MatMul/ReadVariableOp,readmission_prediction/MatMul/ReadVariableOp:S O
'
_output_shapes
:?????????
$
_user_specified_name
inputs/DRG:YU
'
_output_shapes
:?????????
*
_user_specified_nameinputs/age_group:[W
'
_output_shapes
:?????????
,
_user_specified_nameinputs/bed_day_dep:a]
'
_output_shapes
:?????????
2
_user_specified_nameinputs/degree_of_urgency:`\
'
_output_shapes
:?????????
1
_user_specified_nameinputs/diagnosis_code_1:VR
'
_output_shapes
:?????????
'
_user_specified_nameinputs/gender:d`
'
_output_shapes
:?????????
5
_user_specified_nameinputs/mainDiagICD10Chapter
?	
d
E__inference_dropout_4_layer_call_and_return_conditional_losses_742397

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
d
E__inference_dropout_2_layer_call_and_return_conditional_losses_740737

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?	
d
E__inference_dropout_2_layer_call_and_return_conditional_losses_742265

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *????e
dropout/MulMulinputsdropout/Const:output:0*
T0*(
_output_shapes
:??????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*(
_output_shapes
:??????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *??>?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*(
_output_shapes
:??????????p
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*(
_output_shapes
:??????????j
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*(
_output_shapes
:??????????Z
IdentityIdentitydropout/Mul_1:z:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
G__inference_embedding_3_layer_call_and_return_conditional_losses_740272

inputs)
embedding_lookup_740266:
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_740266inputs*
Tindices0**
_class 
loc:@embedding_lookup/740266*+
_output_shapes
:?????????*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/740266*+
_output_shapes
:??????????
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
E__inference_embedding_layer_call_and_return_conditional_losses_741938

inputs)
embedding_lookup_741932:
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_741932inputs*
Tindices0**
_class 
loc:@embedding_lookup/741932*+
_output_shapes
:?????????*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/741932*+
_output_shapes
:??????????
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*+
_output_shapes
:?????????w
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*+
_output_shapes
:?????????Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
F
*__inference_dropout_2_layer_call_fn_742243

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_2_layer_call_and_return_conditional_losses_740462a
IdentityIdentityPartitionedCall:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:??????????:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
(__inference_dense_3_layer_call_fn_742208

inputs
unknown:
??
	unknown_0:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_3_layer_call_and_return_conditional_losses_740434p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:??????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
?
'__inference_i_fall_layer_call_fn_740621
drg
	age_group
bed_day_dep
degree_of_urgency
diagnosis_code_1

gender
maindiagicd10chapter
unknown:
	unknown_0:


	unknown_1:
	unknown_2:
	unknown_3:	?<@
	unknown_4:jj
	unknown_5:j
	unknown_6:j
	unknown_7:j
	unknown_8:	j?
	unknown_9:	?

unknown_10:	j?

unknown_11:	?

unknown_12:
??

unknown_13:	?

unknown_14:	j?

unknown_15:	?

unknown_16:
??

unknown_17:	?

unknown_18:	j?

unknown_19:	?

unknown_20:
??

unknown_21:	?

unknown_22:	j?

unknown_23:	?

unknown_24:	?

unknown_25:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalldrg	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermaindiagicd10chapterunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12
unknown_13
unknown_14
unknown_15
unknown_16
unknown_17
unknown_18
unknown_19
unknown_20
unknown_21
unknown_22
unknown_23
unknown_24
unknown_25*-
Tin&
$2"*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*=
_read_only_resource_inputs
	
 !*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_i_fall_layer_call_and_return_conditional_losses_740564o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*?
_input_shapes?
?:?????????:?????????:?????????:?????????:?????????:?????????:?????????: : : : : : : : : : : : : : : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:L H
'
_output_shapes
:?????????

_user_specified_nameDRG:RN
'
_output_shapes
:?????????
#
_user_specified_name	age_group:TP
'
_output_shapes
:?????????
%
_user_specified_namebed_day_dep:ZV
'
_output_shapes
:?????????
+
_user_specified_namedegree_of_urgency:YU
'
_output_shapes
:?????????
*
_user_specified_namediagnosis_code_1:OK
'
_output_shapes
:?????????
 
_user_specified_namegender:]Y
'
_output_shapes
:?????????
.
_user_specified_namemainDiagICD10Chapter"?	L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
3
DRG,
serving_default_DRG:0?????????
?
	age_group2
serving_default_age_group:0?????????
C
bed_day_dep4
serving_default_bed_day_dep:0?????????
O
degree_of_urgency:
#serving_default_degree_of_urgency:0?????????
M
diagnosis_code_19
"serving_default_diagnosis_code_1:0?????????
9
gender/
serving_default_gender:0?????????
U
mainDiagICD10Chapter=
&serving_default_mainDiagICD10Chapter:0?????????J
readmission_prediction0
StatefulPartitionedCall:0?????????tensorflow/serving/predict:??
?
layer-0
layer-1
layer-2
layer-3
layer-4
layer-5
layer-6
layer-7
	layer-8

layer_with_weights-0

layer-9
layer_with_weights-1
layer-10
layer_with_weights-2
layer-11
layer_with_weights-3
layer-12
layer_with_weights-4
layer-13
layer-14
layer-15
layer_with_weights-5
layer-16
layer_with_weights-6
layer-17
layer-18
layer_with_weights-7
layer-19
layer_with_weights-8
layer-20
layer-21
layer-22
layer_with_weights-9
layer-23
layer_with_weights-10
layer-24
layer-25
layer-26
layer_with_weights-11
layer-27
layer_with_weights-12
layer-28
layer-29
layer-30
 layer_with_weights-13
 layer-31
!layer_with_weights-14
!layer-32
"layer-33
#layer-34
$layer_with_weights-15
$layer-35
%	variables
&trainable_variables
'regularization_losses
(	keras_api
)__call__
**&call_and_return_all_conditional_losses
+_default_save_signature
,	optimizer
-
signatures"
_tf_keras_network
"
_tf_keras_input_layer
"
_tf_keras_input_layer
"
_tf_keras_input_layer
"
_tf_keras_input_layer
"
_tf_keras_input_layer
"
_tf_keras_input_layer
"
_tf_keras_input_layer
(
.	keras_api"
_tf_keras_layer
(
/	keras_api"
_tf_keras_layer
?
0	variables
1trainable_variables
2regularization_losses
3	keras_api
4__call__
*5&call_and_return_all_conditional_losses
6
embeddings"
_tf_keras_layer
?
7	variables
8trainable_variables
9regularization_losses
:	keras_api
;__call__
*<&call_and_return_all_conditional_losses
=
embeddings"
_tf_keras_layer
?
>	variables
?trainable_variables
@regularization_losses
A	keras_api
B__call__
*C&call_and_return_all_conditional_losses
D
embeddings"
_tf_keras_layer
?
E	variables
Ftrainable_variables
Gregularization_losses
H	keras_api
I__call__
*J&call_and_return_all_conditional_losses
K
embeddings"
_tf_keras_layer
?
L	variables
Mtrainable_variables
Nregularization_losses
O	keras_api
P__call__
*Q&call_and_return_all_conditional_losses
R
embeddings"
_tf_keras_layer
?
S	variables
Ttrainable_variables
Uregularization_losses
V	keras_api
W__call__
*X&call_and_return_all_conditional_losses"
_tf_keras_layer
?
Y	variables
Ztrainable_variables
[regularization_losses
\	keras_api
]__call__
*^&call_and_return_all_conditional_losses"
_tf_keras_layer
?
_	variables
`trainable_variables
aregularization_losses
b	keras_api
c__call__
*d&call_and_return_all_conditional_losses

ekernel
fbias"
_tf_keras_layer
?
g	variables
htrainable_variables
iregularization_losses
j	keras_api
k__call__
*l&call_and_return_all_conditional_losses
maxis
	ngamma
obeta"
_tf_keras_layer
?
p	variables
qtrainable_variables
rregularization_losses
s	keras_api
t__call__
*u&call_and_return_all_conditional_losses
v_random_generator"
_tf_keras_layer
?
w	variables
xtrainable_variables
yregularization_losses
z	keras_api
{__call__
*|&call_and_return_all_conditional_losses
}
activation

~kernel
bias"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias"
_tf_keras_layer
)
?	keras_api"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?
activation
?kernel
	?bias"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias"
_tf_keras_layer
)
?	keras_api"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?
activation
?kernel
	?bias"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias"
_tf_keras_layer
)
?	keras_api"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?
activation
?kernel
	?bias"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias"
_tf_keras_layer
)
?	keras_api"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?_random_generator"
_tf_keras_layer
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses
?kernel
	?bias"
_tf_keras_layer
?
60
=1
D2
K3
R4
e5
f6
n7
o8
~9
10
?11
?12
?13
?14
?15
?16
?17
?18
?19
?20
?21
?22
?23
?24
?25
?26"
trackable_list_wrapper
?
60
=1
D2
K3
R4
e5
f6
n7
o8
~9
10
?11
?12
?13
?14
?15
?16
?17
?18
?19
?20
?21
?22
?23
?24
?25
?26"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
%	variables
&trainable_variables
'regularization_losses
)__call__
+_default_save_signature
**&call_and_return_all_conditional_losses
&*"call_and_return_conditional_losses"
_generic_user_object
?
?trace_0
?trace_1
?trace_2
?trace_32?
'__inference_i_fall_layer_call_fn_740621
'__inference_i_fall_layer_call_fn_741516
'__inference_i_fall_layer_call_fn_741581
'__inference_i_fall_layer_call_fn_741192?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1z?trace_2z?trace_3
?
?trace_0
?trace_1
?trace_2
?trace_32?
B__inference_i_fall_layer_call_and_return_conditional_losses_741734
B__inference_i_fall_layer_call_and_return_conditional_losses_741922
B__inference_i_fall_layer_call_and_return_conditional_losses_741285
B__inference_i_fall_layer_call_and_return_conditional_losses_741378?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1z?trace_2z?trace_3
?B?
!__inference__wrapped_model_740203DRG	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermainDiagICD10Chapter"?
???
FullArgSpec
args? 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?
	?iter
?beta_1
?beta_2

?decay
?learning_rate6m?=m?Dm?Km?Rm?em?fm?nm?om?~m?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?	?m?6v?=v?Dv?Kv?Rv?ev?fv?nv?ov?~v?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?	?v?"
	optimizer
-
?serving_default"
signature_map
"
_generic_user_object
"
_generic_user_object
'
60"
trackable_list_wrapper
'
60"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
0	variables
1trainable_variables
2regularization_losses
4__call__
*5&call_and_return_all_conditional_losses
&5"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
*__inference_embedding_layer_call_fn_741929?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
E__inference_embedding_layer_call_and_return_conditional_losses_741938?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
&:$2embedding/embeddings
'
=0"
trackable_list_wrapper
'
=0"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
7	variables
8trainable_variables
9regularization_losses
;__call__
*<&call_and_return_all_conditional_losses
&<"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
,__inference_embedding_1_layer_call_fn_741945?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
G__inference_embedding_1_layer_call_and_return_conditional_losses_741954?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
(:&

2embedding_1/embeddings
'
D0"
trackable_list_wrapper
'
D0"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
>	variables
?trainable_variables
@regularization_losses
B__call__
*C&call_and_return_all_conditional_losses
&C"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
,__inference_embedding_2_layer_call_fn_741961?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
G__inference_embedding_2_layer_call_and_return_conditional_losses_741970?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
(:&2embedding_2/embeddings
'
K0"
trackable_list_wrapper
'
K0"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
E	variables
Ftrainable_variables
Gregularization_losses
I__call__
*J&call_and_return_all_conditional_losses
&J"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
,__inference_embedding_3_layer_call_fn_741977?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
G__inference_embedding_3_layer_call_and_return_conditional_losses_741986?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
(:&2embedding_3/embeddings
'
R0"
trackable_list_wrapper
'
R0"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
L	variables
Mtrainable_variables
Nregularization_losses
P__call__
*Q&call_and_return_all_conditional_losses
&Q"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
,__inference_embedding_4_layer_call_fn_741993?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
G__inference_embedding_4_layer_call_and_return_conditional_losses_742002?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
):'	?<@2embedding_4/embeddings
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
S	variables
Ttrainable_variables
Uregularization_losses
W__call__
*X&call_and_return_all_conditional_losses
&X"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
,__inference_concatenate_layer_call_fn_742013?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
G__inference_concatenate_layer_call_and_return_conditional_losses_742025?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
Y	variables
Ztrainable_variables
[regularization_losses
]__call__
*^&call_and_return_all_conditional_losses
&^"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_flatten_layer_call_fn_742030?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_flatten_layer_call_and_return_conditional_losses_742036?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
.
e0
f1"
trackable_list_wrapper
.
e0
f1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
_	variables
`trainable_variables
aregularization_losses
c__call__
*d&call_and_return_all_conditional_losses
&d"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
&__inference_dense_layer_call_fn_742045?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
A__inference_dense_layer_call_and_return_conditional_losses_742055?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
:jj2dense/kernel
:j2
dense/bias
.
n0
o1"
trackable_list_wrapper
.
n0
o1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
g	variables
htrainable_variables
iregularization_losses
k__call__
*l&call_and_return_all_conditional_losses
&l"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
4__inference_layer_normalization_layer_call_fn_742064?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
O__inference_layer_normalization_layer_call_and_return_conditional_losses_742106?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
 "
trackable_list_wrapper
':%j2layer_normalization/gamma
&:$j2layer_normalization/beta
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
p	variables
qtrainable_variables
rregularization_losses
t__call__
*u&call_and_return_all_conditional_losses
&u"call_and_return_conditional_losses"
_generic_user_object
?
?trace_0
?trace_12?
(__inference_dropout_layer_call_fn_742111
(__inference_dropout_layer_call_fn_742116?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
?
?trace_0
?trace_12?
C__inference_dropout_layer_call_and_return_conditional_losses_742121
C__inference_dropout_layer_call_and_return_conditional_losses_742133?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
"
_generic_user_object
.
~0
1"
trackable_list_wrapper
.
~0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
w	variables
xtrainable_variables
yregularization_losses
{__call__
*|&call_and_return_all_conditional_losses
&|"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_1_layer_call_fn_742142?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_1_layer_call_and_return_conditional_losses_742153?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses"
_tf_keras_layer
!:	j?2dense_1/kernel
:?2dense_1/bias
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_2_layer_call_fn_742162?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_2_layer_call_and_return_conditional_losses_742172?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
!:	j?2dense_2/kernel
:?2dense_2/bias
"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_0
?trace_12?
*__inference_dropout_1_layer_call_fn_742177
*__inference_dropout_1_layer_call_fn_742182?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
?
?trace_0
?trace_12?
E__inference_dropout_1_layer_call_and_return_conditional_losses_742187
E__inference_dropout_1_layer_call_and_return_conditional_losses_742199?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
"
_generic_user_object
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_3_layer_call_fn_742208?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_3_layer_call_and_return_conditional_losses_742219?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses"
_tf_keras_layer
": 
??2dense_3/kernel
:?2dense_3/bias
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_4_layer_call_fn_742228?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_4_layer_call_and_return_conditional_losses_742238?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
!:	j?2dense_4/kernel
:?2dense_4/bias
"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_0
?trace_12?
*__inference_dropout_2_layer_call_fn_742243
*__inference_dropout_2_layer_call_fn_742248?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
?
?trace_0
?trace_12?
E__inference_dropout_2_layer_call_and_return_conditional_losses_742253
E__inference_dropout_2_layer_call_and_return_conditional_losses_742265?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
"
_generic_user_object
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_5_layer_call_fn_742274?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_5_layer_call_and_return_conditional_losses_742285?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses"
_tf_keras_layer
": 
??2dense_5/kernel
:?2dense_5/bias
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_6_layer_call_fn_742294?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_6_layer_call_and_return_conditional_losses_742304?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
!:	j?2dense_6/kernel
:?2dense_6/bias
"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_0
?trace_12?
*__inference_dropout_3_layer_call_fn_742309
*__inference_dropout_3_layer_call_fn_742314?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
?
?trace_0
?trace_12?
E__inference_dropout_3_layer_call_and_return_conditional_losses_742319
E__inference_dropout_3_layer_call_and_return_conditional_losses_742331?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
"
_generic_user_object
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_7_layer_call_fn_742340?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_7_layer_call_and_return_conditional_losses_742351?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?	variables
?trainable_variables
?regularization_losses
?	keras_api
?__call__
+?&call_and_return_all_conditional_losses"
_tf_keras_layer
": 
??2dense_7/kernel
:?2dense_7/bias
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
(__inference_dense_8_layer_call_fn_742360?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
C__inference_dense_8_layer_call_and_return_conditional_losses_742370?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
!:	j?2dense_8/kernel
:?2dense_8/bias
"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_0
?trace_12?
*__inference_dropout_4_layer_call_fn_742375
*__inference_dropout_4_layer_call_fn_742380?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
?
?trace_0
?trace_12?
E__inference_dropout_4_layer_call_and_return_conditional_losses_742385
E__inference_dropout_4_layer_call_and_return_conditional_losses_742397?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0z?trace_1
"
_generic_user_object
0
?0
?1"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?
?trace_02?
7__inference_readmission_prediction_layer_call_fn_742406?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
?
?trace_02?
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_742417?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 z?trace_0
0:.	?2readmission_prediction/kernel
):'2readmission_prediction/bias
 "
trackable_list_wrapper
?
0
1
2
3
4
5
6
7
	8

9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
 31
!32
"33
#34
$35"
trackable_list_wrapper
0
?0
?1"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
'__inference_i_fall_layer_call_fn_740621DRG	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
'__inference_i_fall_layer_call_fn_741516
inputs/DRGinputs/age_groupinputs/bed_day_depinputs/degree_of_urgencyinputs/diagnosis_code_1inputs/genderinputs/mainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
'__inference_i_fall_layer_call_fn_741581
inputs/DRGinputs/age_groupinputs/bed_day_depinputs/degree_of_urgencyinputs/diagnosis_code_1inputs/genderinputs/mainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
'__inference_i_fall_layer_call_fn_741192DRG	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
B__inference_i_fall_layer_call_and_return_conditional_losses_741734
inputs/DRGinputs/age_groupinputs/bed_day_depinputs/degree_of_urgencyinputs/diagnosis_code_1inputs/genderinputs/mainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
B__inference_i_fall_layer_call_and_return_conditional_losses_741922
inputs/DRGinputs/age_groupinputs/bed_day_depinputs/degree_of_urgencyinputs/diagnosis_code_1inputs/genderinputs/mainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
B__inference_i_fall_layer_call_and_return_conditional_losses_741285DRG	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
B__inference_i_fall_layer_call_and_return_conditional_losses_741378DRG	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermainDiagICD10Chapter"?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
:	 (2	Adam/iter
: (2Adam/beta_1
: (2Adam/beta_2
: (2
Adam/decay
: (2Adam/learning_rate
?B?
$__inference_signature_wrapper_741451DRG	age_groupbed_day_depdegree_of_urgencydiagnosis_code_1gendermainDiagICD10Chapter"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
*__inference_embedding_layer_call_fn_741929inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_embedding_layer_call_and_return_conditional_losses_741938inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
,__inference_embedding_1_layer_call_fn_741945inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
G__inference_embedding_1_layer_call_and_return_conditional_losses_741954inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
,__inference_embedding_2_layer_call_fn_741961inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
G__inference_embedding_2_layer_call_and_return_conditional_losses_741970inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
,__inference_embedding_3_layer_call_fn_741977inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
G__inference_embedding_3_layer_call_and_return_conditional_losses_741986inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
,__inference_embedding_4_layer_call_fn_741993inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
G__inference_embedding_4_layer_call_and_return_conditional_losses_742002inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
,__inference_concatenate_layer_call_fn_742013inputs/0inputs/1inputs/2inputs/3inputs/4inputs/5inputs/6"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
G__inference_concatenate_layer_call_and_return_conditional_losses_742025inputs/0inputs/1inputs/2inputs/3inputs/4inputs/5inputs/6"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_flatten_layer_call_fn_742030inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_flatten_layer_call_and_return_conditional_losses_742036inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
&__inference_dense_layer_call_fn_742045inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
A__inference_dense_layer_call_and_return_conditional_losses_742055inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
4__inference_layer_normalization_layer_call_fn_742064inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
O__inference_layer_normalization_layer_call_and_return_conditional_losses_742106inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dropout_layer_call_fn_742111inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
(__inference_dropout_layer_call_fn_742116inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dropout_layer_call_and_return_conditional_losses_742121inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dropout_layer_call_and_return_conditional_losses_742133inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
'
}0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_1_layer_call_fn_742142inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_1_layer_call_and_return_conditional_losses_742153inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_2_layer_call_fn_742162inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_2_layer_call_and_return_conditional_losses_742172inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
*__inference_dropout_1_layer_call_fn_742177inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
*__inference_dropout_1_layer_call_fn_742182inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_1_layer_call_and_return_conditional_losses_742187inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_1_layer_call_and_return_conditional_losses_742199inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
(
?0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_3_layer_call_fn_742208inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_3_layer_call_and_return_conditional_losses_742219inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_4_layer_call_fn_742228inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_4_layer_call_and_return_conditional_losses_742238inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
*__inference_dropout_2_layer_call_fn_742243inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
*__inference_dropout_2_layer_call_fn_742248inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_2_layer_call_and_return_conditional_losses_742253inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_2_layer_call_and_return_conditional_losses_742265inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
(
?0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_5_layer_call_fn_742274inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_5_layer_call_and_return_conditional_losses_742285inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_6_layer_call_fn_742294inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_6_layer_call_and_return_conditional_losses_742304inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
*__inference_dropout_3_layer_call_fn_742309inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
*__inference_dropout_3_layer_call_fn_742314inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_3_layer_call_and_return_conditional_losses_742319inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_3_layer_call_and_return_conditional_losses_742331inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
(
?0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_7_layer_call_fn_742340inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_7_layer_call_and_return_conditional_losses_742351inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?non_trainable_variables
?layers
?metrics
 ?layer_regularization_losses
?layer_metrics
?	variables
?trainable_variables
?regularization_losses
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2??
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
(__inference_dense_8_layer_call_fn_742360inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
C__inference_dense_8_layer_call_and_return_conditional_losses_742370inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
*__inference_dropout_4_layer_call_fn_742375inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
*__inference_dropout_4_layer_call_fn_742380inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_4_layer_call_and_return_conditional_losses_742385inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
E__inference_dropout_4_layer_call_and_return_conditional_losses_742397inputs"?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
?B?
7__inference_readmission_prediction_layer_call_fn_742406inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_742417inputs"?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
R
?	variables
?	keras_api

?total

?count"
_tf_keras_metric
c
?	variables
?	keras_api

?total

?count
?
_fn_kwargs"
_tf_keras_metric
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
0
?0
?1"
trackable_list_wrapper
.
?	variables"
_generic_user_object
:  (2total
:  (2count
0
?0
?1"
trackable_list_wrapper
.
?	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
+:)2Adam/embedding/embeddings/m
-:+

2Adam/embedding_1/embeddings/m
-:+2Adam/embedding_2/embeddings/m
-:+2Adam/embedding_3/embeddings/m
.:,	?<@2Adam/embedding_4/embeddings/m
#:!jj2Adam/dense/kernel/m
:j2Adam/dense/bias/m
,:*j2 Adam/layer_normalization/gamma/m
+:)j2Adam/layer_normalization/beta/m
&:$	j?2Adam/dense_1/kernel/m
 :?2Adam/dense_1/bias/m
&:$	j?2Adam/dense_2/kernel/m
 :?2Adam/dense_2/bias/m
':%
??2Adam/dense_3/kernel/m
 :?2Adam/dense_3/bias/m
&:$	j?2Adam/dense_4/kernel/m
 :?2Adam/dense_4/bias/m
':%
??2Adam/dense_5/kernel/m
 :?2Adam/dense_5/bias/m
&:$	j?2Adam/dense_6/kernel/m
 :?2Adam/dense_6/bias/m
':%
??2Adam/dense_7/kernel/m
 :?2Adam/dense_7/bias/m
&:$	j?2Adam/dense_8/kernel/m
 :?2Adam/dense_8/bias/m
5:3	?2$Adam/readmission_prediction/kernel/m
.:,2"Adam/readmission_prediction/bias/m
+:)2Adam/embedding/embeddings/v
-:+

2Adam/embedding_1/embeddings/v
-:+2Adam/embedding_2/embeddings/v
-:+2Adam/embedding_3/embeddings/v
.:,	?<@2Adam/embedding_4/embeddings/v
#:!jj2Adam/dense/kernel/v
:j2Adam/dense/bias/v
,:*j2 Adam/layer_normalization/gamma/v
+:)j2Adam/layer_normalization/beta/v
&:$	j?2Adam/dense_1/kernel/v
 :?2Adam/dense_1/bias/v
&:$	j?2Adam/dense_2/kernel/v
 :?2Adam/dense_2/bias/v
':%
??2Adam/dense_3/kernel/v
 :?2Adam/dense_3/bias/v
&:$	j?2Adam/dense_4/kernel/v
 :?2Adam/dense_4/bias/v
':%
??2Adam/dense_5/kernel/v
 :?2Adam/dense_5/bias/v
&:$	j?2Adam/dense_6/kernel/v
 :?2Adam/dense_6/bias/v
':%
??2Adam/dense_7/kernel/v
 :?2Adam/dense_7/bias/v
&:$	j?2Adam/dense_8/kernel/v
 :?2Adam/dense_8/bias/v
5:3	?2$Adam/readmission_prediction/kernel/v
.:,2"Adam/readmission_prediction/bias/v?
!__inference__wrapped_model_740203?+6=DKRefno~???????????????????
???
???
$
DRG?
DRG?????????
0
	age_group#? 
	age_group?????????
4
bed_day_dep%?"
bed_day_dep?????????
@
degree_of_urgency+?(
degree_of_urgency?????????
>
diagnosis_code_1*?'
diagnosis_code_1?????????
*
gender ?
gender?????????
F
mainDiagICD10Chapter.?+
mainDiagICD10Chapter?????????
? "O?L
J
readmission_prediction0?-
readmission_prediction??????????
G__inference_concatenate_layer_call_and_return_conditional_losses_742025????
???
???
&?#
inputs/0?????????
&?#
inputs/1?????????
&?#
inputs/2?????????
&?#
inputs/3?????????

&?#
inputs/4?????????
&?#
inputs/5?????????
&?#
inputs/6?????????@
? ")?&
?
0?????????j
? ?
,__inference_concatenate_layer_call_fn_742013????
???
???
&?#
inputs/0?????????
&?#
inputs/1?????????
&?#
inputs/2?????????
&?#
inputs/3?????????

&?#
inputs/4?????????
&?#
inputs/5?????????
&?#
inputs/6?????????@
? "??????????j?
C__inference_dense_1_layer_call_and_return_conditional_losses_742153]~/?,
%?"
 ?
inputs?????????j
? "&?#
?
0??????????
? |
(__inference_dense_1_layer_call_fn_742142P~/?,
%?"
 ?
inputs?????????j
? "????????????
C__inference_dense_2_layer_call_and_return_conditional_losses_742172_??/?,
%?"
 ?
inputs?????????j
? "&?#
?
0??????????
? ~
(__inference_dense_2_layer_call_fn_742162R??/?,
%?"
 ?
inputs?????????j
? "????????????
C__inference_dense_3_layer_call_and_return_conditional_losses_742219`??0?-
&?#
!?
inputs??????????
? "&?#
?
0??????????
? 
(__inference_dense_3_layer_call_fn_742208S??0?-
&?#
!?
inputs??????????
? "????????????
C__inference_dense_4_layer_call_and_return_conditional_losses_742238_??/?,
%?"
 ?
inputs?????????j
? "&?#
?
0??????????
? ~
(__inference_dense_4_layer_call_fn_742228R??/?,
%?"
 ?
inputs?????????j
? "????????????
C__inference_dense_5_layer_call_and_return_conditional_losses_742285`??0?-
&?#
!?
inputs??????????
? "&?#
?
0??????????
? 
(__inference_dense_5_layer_call_fn_742274S??0?-
&?#
!?
inputs??????????
? "????????????
C__inference_dense_6_layer_call_and_return_conditional_losses_742304_??/?,
%?"
 ?
inputs?????????j
? "&?#
?
0??????????
? ~
(__inference_dense_6_layer_call_fn_742294R??/?,
%?"
 ?
inputs?????????j
? "????????????
C__inference_dense_7_layer_call_and_return_conditional_losses_742351`??0?-
&?#
!?
inputs??????????
? "&?#
?
0??????????
? 
(__inference_dense_7_layer_call_fn_742340S??0?-
&?#
!?
inputs??????????
? "????????????
C__inference_dense_8_layer_call_and_return_conditional_losses_742370_??/?,
%?"
 ?
inputs?????????j
? "&?#
?
0??????????
? ~
(__inference_dense_8_layer_call_fn_742360R??/?,
%?"
 ?
inputs?????????j
? "????????????
A__inference_dense_layer_call_and_return_conditional_losses_742055\ef/?,
%?"
 ?
inputs?????????j
? "%?"
?
0?????????j
? y
&__inference_dense_layer_call_fn_742045Oef/?,
%?"
 ?
inputs?????????j
? "??????????j?
E__inference_dropout_1_layer_call_and_return_conditional_losses_742187^4?1
*?'
!?
inputs??????????
p 
? "&?#
?
0??????????
? ?
E__inference_dropout_1_layer_call_and_return_conditional_losses_742199^4?1
*?'
!?
inputs??????????
p
? "&?#
?
0??????????
? 
*__inference_dropout_1_layer_call_fn_742177Q4?1
*?'
!?
inputs??????????
p 
? "???????????
*__inference_dropout_1_layer_call_fn_742182Q4?1
*?'
!?
inputs??????????
p
? "????????????
E__inference_dropout_2_layer_call_and_return_conditional_losses_742253^4?1
*?'
!?
inputs??????????
p 
? "&?#
?
0??????????
? ?
E__inference_dropout_2_layer_call_and_return_conditional_losses_742265^4?1
*?'
!?
inputs??????????
p
? "&?#
?
0??????????
? 
*__inference_dropout_2_layer_call_fn_742243Q4?1
*?'
!?
inputs??????????
p 
? "???????????
*__inference_dropout_2_layer_call_fn_742248Q4?1
*?'
!?
inputs??????????
p
? "????????????
E__inference_dropout_3_layer_call_and_return_conditional_losses_742319^4?1
*?'
!?
inputs??????????
p 
? "&?#
?
0??????????
? ?
E__inference_dropout_3_layer_call_and_return_conditional_losses_742331^4?1
*?'
!?
inputs??????????
p
? "&?#
?
0??????????
? 
*__inference_dropout_3_layer_call_fn_742309Q4?1
*?'
!?
inputs??????????
p 
? "???????????
*__inference_dropout_3_layer_call_fn_742314Q4?1
*?'
!?
inputs??????????
p
? "????????????
E__inference_dropout_4_layer_call_and_return_conditional_losses_742385^4?1
*?'
!?
inputs??????????
p 
? "&?#
?
0??????????
? ?
E__inference_dropout_4_layer_call_and_return_conditional_losses_742397^4?1
*?'
!?
inputs??????????
p
? "&?#
?
0??????????
? 
*__inference_dropout_4_layer_call_fn_742375Q4?1
*?'
!?
inputs??????????
p 
? "???????????
*__inference_dropout_4_layer_call_fn_742380Q4?1
*?'
!?
inputs??????????
p
? "????????????
C__inference_dropout_layer_call_and_return_conditional_losses_742121\3?0
)?&
 ?
inputs?????????j
p 
? "%?"
?
0?????????j
? ?
C__inference_dropout_layer_call_and_return_conditional_losses_742133\3?0
)?&
 ?
inputs?????????j
p
? "%?"
?
0?????????j
? {
(__inference_dropout_layer_call_fn_742111O3?0
)?&
 ?
inputs?????????j
p 
? "??????????j{
(__inference_dropout_layer_call_fn_742116O3?0
)?&
 ?
inputs?????????j
p
? "??????????j?
G__inference_embedding_1_layer_call_and_return_conditional_losses_741954_=/?,
%?"
 ?
inputs?????????
? ")?&
?
0?????????

? ?
,__inference_embedding_1_layer_call_fn_741945R=/?,
%?"
 ?
inputs?????????
? "??????????
?
G__inference_embedding_2_layer_call_and_return_conditional_losses_741970_D/?,
%?"
 ?
inputs?????????
? ")?&
?
0?????????
? ?
,__inference_embedding_2_layer_call_fn_741961RD/?,
%?"
 ?
inputs?????????
? "???????????
G__inference_embedding_3_layer_call_and_return_conditional_losses_741986_K/?,
%?"
 ?
inputs?????????
? ")?&
?
0?????????
? ?
,__inference_embedding_3_layer_call_fn_741977RK/?,
%?"
 ?
inputs?????????
? "???????????
G__inference_embedding_4_layer_call_and_return_conditional_losses_742002_R/?,
%?"
 ?
inputs?????????
? ")?&
?
0?????????@
? ?
,__inference_embedding_4_layer_call_fn_741993RR/?,
%?"
 ?
inputs?????????
? "??????????@?
E__inference_embedding_layer_call_and_return_conditional_losses_741938_6/?,
%?"
 ?
inputs?????????
? ")?&
?
0?????????
? ?
*__inference_embedding_layer_call_fn_741929R6/?,
%?"
 ?
inputs?????????
? "???????????
C__inference_flatten_layer_call_and_return_conditional_losses_742036\3?0
)?&
$?!
inputs?????????j
? "%?"
?
0?????????j
? {
(__inference_flatten_layer_call_fn_742030O3?0
)?&
$?!
inputs?????????j
? "??????????j?
B__inference_i_fall_layer_call_and_return_conditional_losses_741285?+6=DKRefno~???????????????????
???
???
$
DRG?
DRG?????????
0
	age_group#? 
	age_group?????????
4
bed_day_dep%?"
bed_day_dep?????????
@
degree_of_urgency+?(
degree_of_urgency?????????
>
diagnosis_code_1*?'
diagnosis_code_1?????????
*
gender ?
gender?????????
F
mainDiagICD10Chapter.?+
mainDiagICD10Chapter?????????
p 

 
? "%?"
?
0?????????
? ?
B__inference_i_fall_layer_call_and_return_conditional_losses_741378?+6=DKRefno~???????????????????
???
???
$
DRG?
DRG?????????
0
	age_group#? 
	age_group?????????
4
bed_day_dep%?"
bed_day_dep?????????
@
degree_of_urgency+?(
degree_of_urgency?????????
>
diagnosis_code_1*?'
diagnosis_code_1?????????
*
gender ?
gender?????????
F
mainDiagICD10Chapter.?+
mainDiagICD10Chapter?????????
p

 
? "%?"
?
0?????????
? ?
B__inference_i_fall_layer_call_and_return_conditional_losses_741734?+6=DKRefno~???????????????????
???
???
+
DRG$?!

inputs/DRG?????????
7
	age_group*?'
inputs/age_group?????????
;
bed_day_dep,?)
inputs/bed_day_dep?????????
G
degree_of_urgency2?/
inputs/degree_of_urgency?????????
E
diagnosis_code_11?.
inputs/diagnosis_code_1?????????
1
gender'?$
inputs/gender?????????
M
mainDiagICD10Chapter5?2
inputs/mainDiagICD10Chapter?????????
p 

 
? "%?"
?
0?????????
? ?
B__inference_i_fall_layer_call_and_return_conditional_losses_741922?+6=DKRefno~???????????????????
???
???
+
DRG$?!

inputs/DRG?????????
7
	age_group*?'
inputs/age_group?????????
;
bed_day_dep,?)
inputs/bed_day_dep?????????
G
degree_of_urgency2?/
inputs/degree_of_urgency?????????
E
diagnosis_code_11?.
inputs/diagnosis_code_1?????????
1
gender'?$
inputs/gender?????????
M
mainDiagICD10Chapter5?2
inputs/mainDiagICD10Chapter?????????
p

 
? "%?"
?
0?????????
? ?
'__inference_i_fall_layer_call_fn_740621?+6=DKRefno~???????????????????
???
???
$
DRG?
DRG?????????
0
	age_group#? 
	age_group?????????
4
bed_day_dep%?"
bed_day_dep?????????
@
degree_of_urgency+?(
degree_of_urgency?????????
>
diagnosis_code_1*?'
diagnosis_code_1?????????
*
gender ?
gender?????????
F
mainDiagICD10Chapter.?+
mainDiagICD10Chapter?????????
p 

 
? "???????????
'__inference_i_fall_layer_call_fn_741192?+6=DKRefno~???????????????????
???
???
$
DRG?
DRG?????????
0
	age_group#? 
	age_group?????????
4
bed_day_dep%?"
bed_day_dep?????????
@
degree_of_urgency+?(
degree_of_urgency?????????
>
diagnosis_code_1*?'
diagnosis_code_1?????????
*
gender ?
gender?????????
F
mainDiagICD10Chapter.?+
mainDiagICD10Chapter?????????
p

 
? "???????????
'__inference_i_fall_layer_call_fn_741516?+6=DKRefno~???????????????????
???
???
+
DRG$?!

inputs/DRG?????????
7
	age_group*?'
inputs/age_group?????????
;
bed_day_dep,?)
inputs/bed_day_dep?????????
G
degree_of_urgency2?/
inputs/degree_of_urgency?????????
E
diagnosis_code_11?.
inputs/diagnosis_code_1?????????
1
gender'?$
inputs/gender?????????
M
mainDiagICD10Chapter5?2
inputs/mainDiagICD10Chapter?????????
p 

 
? "???????????
'__inference_i_fall_layer_call_fn_741581?+6=DKRefno~???????????????????
???
???
+
DRG$?!

inputs/DRG?????????
7
	age_group*?'
inputs/age_group?????????
;
bed_day_dep,?)
inputs/bed_day_dep?????????
G
degree_of_urgency2?/
inputs/degree_of_urgency?????????
E
diagnosis_code_11?.
inputs/diagnosis_code_1?????????
1
gender'?$
inputs/gender?????????
M
mainDiagICD10Chapter5?2
inputs/mainDiagICD10Chapter?????????
p

 
? "???????????
O__inference_layer_normalization_layer_call_and_return_conditional_losses_742106\no/?,
%?"
 ?
inputs?????????j
? "%?"
?
0?????????j
? ?
4__inference_layer_normalization_layer_call_fn_742064Ono/?,
%?"
 ?
inputs?????????j
? "??????????j?
R__inference_readmission_prediction_layer_call_and_return_conditional_losses_742417_??0?-
&?#
!?
inputs??????????
? "%?"
?
0?????????
? ?
7__inference_readmission_prediction_layer_call_fn_742406R??0?-
&?#
!?
inputs??????????
? "???????????
$__inference_signature_wrapper_741451?+6=DKRefno~???????????????????
? 
???
$
DRG?
DRG?????????
0
	age_group#? 
	age_group?????????
4
bed_day_dep%?"
bed_day_dep?????????
@
degree_of_urgency+?(
degree_of_urgency?????????
>
diagnosis_code_1*?'
diagnosis_code_1?????????
*
gender ?
gender?????????
F
mainDiagICD10Chapter.?+
mainDiagICD10Chapter?????????"O?L
J
readmission_prediction0?-
readmission_prediction?????????