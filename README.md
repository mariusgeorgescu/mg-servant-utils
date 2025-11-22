# mg-servant-utils

A collection of utility modules for building Servant-based web services.

## Features

- **Authentication**: Basic authentication utilities for Servant APIs
- **CORS**: Configurable CORS middleware for WAI applications
- **Handlers**: Generic exception handlers for Servant handlers
- **ServiceProbe**: Health check and readiness check endpoints
- **Utils**: Common utility functions (e.g., port configuration from environment)

## Installation

Add to your `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/mariusgeorgescu/mg-servant-utils
  tag: v0.1.0.0
```

Or add to your `.cabal` file:

```cabal
build-depends: mg-servant-utils >= 0.1.0.0
```

## Usage

### Authentication

```haskell
import MGServantUtils.Auth

-- Get auth context from environment variables
authContext <- getBasicAuthFromEnv

-- Use in your API
type ProtectedAPI = BasicAuth "realm" AuthUser :> YourAPI
```

### CORS

```haskell
import MGServantUtils.CORS

-- Apply CORS middleware
app = MGServantUtils.CORS.setupCors yourApplication
```

### Service Probe

```haskell
import MGServantUtils.ServiceProbe

type API = ServiceProbe Text Text :<|> YourAPI

server = alwaysHealthy "my-service" :<|> alwaysReady "my-service" :<|> yourServer
```

### Exception Handlers

```haskell
import MGServantUtils.Handlers

-- Generic exception handler
runApp = runReaderT app ctx `catch` handleException
```

## License

Apache-2.0

## Author

Marius Georgescu

