# Elm-Ogl

Elm bindings for Ogl.

Aims to provide a set of declarative APIs to use WebGL through Ogl.

## Backend

Currently we choose to use Ogl as WebGL backend for its simplicity. Other options are regl.

## Design

In Elm, we provide data structures and a compile API to form a sequence of drawing commands.

Example:

```
Circle [ Setting... ]
Group [ Setting ] [ Renderable ]
```

### Renderable

- A Program ID corresponding to a GL program pre-compiled.
- Configurations. (Key Value Pair)

### GL Program

Vertex + Fragment shader programs.

Geometry + uniforms.


