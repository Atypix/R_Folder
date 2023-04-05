

let auth0 = null;

const fetchAuthConfig = () => fetch("assets/auth_config.json");

const configureClient = async () => {
  const response = await fetchAuthConfig();
  const config = await response.json();

  auth0 = await createAuth0Client({
    domain: config.domain,
    client_id: config.clientId
  });
};



window.onload = async () => {
  

  document.title = 'Analytics CUPS'
  await configureClient();
  const isAuthenticated = await auth0.isAuthenticated();
  console.log(isAuthenticated)
  if (isAuthenticated) {
    
    document.getElementById("logout").onclick = logout;
    return;
  }
  
  console.log("> User not authenticated");

  const query = window.location.search;
  const shouldParseResult = query.includes("code=") && query.includes("state=");

  if (shouldParseResult) {
    console.log("> Parsing redirect");
    try {
      const result = await auth0.handleRedirectCallback();

      if (result.appState && result.appState.targetUrl) {
        showContentFromUrl(result.appState.targetUrl);
      } 
      document.getElementById("logout").onclick = logout;
      console.log("Logged in!");
      
      return;
    } catch (err) {
      console.log("Error parsing redirect:", err);
      
    }
    
    window.history.replaceState({}, document.title, "/");
  }
  
    await auth0.loginWithRedirect({
        redirect_uri: window.location.origin
    });
    

  
 
}

    const logout = () => {
      auth0.logout({
        returnTo: window.location.origin
      });
    };












