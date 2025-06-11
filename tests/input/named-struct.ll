; ModuleID = 'simple module'

%struct.coord2d = type {i32, i32}
%struct.vector = type {%struct.coord2d, %struct.coord2d}

@up = global %struct.vector {%struct.coord2d {i32 0, i32 0}, %struct.coord2d {i32 0, i32 1}}
