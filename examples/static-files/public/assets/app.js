// Tapak Static File Server - Example JavaScript

console.log('🎯 Tapak static file server example loaded!');

// API test functionality
document.addEventListener('DOMContentLoaded', function() {
    const apiButton = document.getElementById('api-test-btn');
    const apiResponse = document.getElementById('api-response');

    if (apiButton && apiResponse) {
        apiButton.addEventListener('click', async function() {
            apiButton.disabled = true;
            apiButton.textContent = 'Loading...';
            apiResponse.textContent = 'Fetching data from API...';

            try {
                const response = await fetch('/api/hello');
                const data = await response.json();

                // Pretty print the JSON response
                apiResponse.textContent = JSON.stringify(data, null, 2);

                // Show response headers info
                const headers = response.headers;
                const headerInfo = {
                    'Content-Type': headers.get('Content-Type'),
                    'Cache-Control': headers.get('Cache-Control'),
                    'Status': response.status
                };

                console.log('Response Headers:', headerInfo);

            } catch (error) {
                apiResponse.textContent = 'Error: ' + error.message;
                console.error('API request failed:', error);
            } finally {
                apiButton.disabled = false;
                apiButton.textContent = 'Fetch from API';
            }
        });
    }

    // Log page load time
    const loadTime = performance.now();
    console.log(`Page loaded in ${loadTime.toFixed(2)}ms`);

    // Demonstrate browser caching
    const checkCaching = () => {
        const entries = performance.getEntriesByType('navigation');
        if (entries.length > 0) {
            const navEntry = entries[0];
            console.log('Navigation timing:', {
                transferSize: navEntry.transferSize,
                cached: navEntry.transferSize === 0 ? 'Yes (304 or cache)' : 'No (fresh download)'
            });
        }
    };

    window.addEventListener('load', checkCaching);
});

// Helper function to display current time
function displayCurrentTime() {
    const now = new Date();
    console.log('Current time:', now.toISOString());
}

// Display time on load
displayCurrentTime();
