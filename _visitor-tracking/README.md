# Visitor tracking

Each page view on the site is logged to a Google Sheet. A row holds the time, IP address, city,
region, country, ISP, page, referrer, browser, screen size, language and time zone.

How it works: `_visitor-tracker.html` is included at the bottom of every page through
`_quarto.yml`. When a page loads, it looks up the visitor's IP and approximate location
(ipapi.co, with ipwho.is as a fallback) and sends the result to a Google Apps Script, which adds a
row to the Sheet.

## One-time setup (about 5 minutes)

1. Create a new Google Sheet, for example "Website Visitors".
2. In the Sheet, open **Extensions → Apps Script**. Delete the starter code and paste in the
   contents of `apps-script.gs`. Save.
3. Click **Deploy → New deployment**. Choose the type **Web app** and set:
   - Execute as: **Me**
   - Who has access: **Anyone**
   
   Click **Deploy** and approve the permissions. Google will show an "unsafe app" warning because
   you wrote the script yourself. Choose **Advanced → Go to project**.
4. Copy the **Web app URL**. It ends in `/exec`. You can open it in a browser to check it: the
   page should say "Visitor logger is running."
5. Paste the URL into `_visitor-tracker.html`, replacing `PASTE_YOUR_APPS_SCRIPT_URL_HERE`.
6. Run `quarto render`, then commit and push.

Visits will show up in the **Visits** tab of the Sheet.

## Notes

- Your own local previews (localhost, or files opened directly) are not logged.
- Ad blockers and privacy browsers may block the location lookup. The page view is still logged,
  but without the IP and location.
- Locations come from the IP address, so they are approximate (usually city level). A visitor
  using a VPN shows the VPN's location.
- The free location lookup at ipapi.co allows about 1,000 lookups a day. The tracker looks up each
  visitor once per browser session.
- If you edit `apps-script.gs` later, go to **Deploy → Manage deployments → Edit → New version**.
  That keeps the same URL.
