*          DATA SET REREQ11    AT LEVEL 057 AS OF 04/23/01                      
*PHASE T80711A,*                                                                
RQ11     TITLE 'T80711 - REREQ11 - NEW REP REQUEST DEFINITION TABLE'            
***********************************************************************         
*                                                                     *         
*    REREQ11 (T80711) --- REQUEST PROGRAM                             *         
*                                                                     *         
*    REQUEST DEFINITION TABLE AND RECORD DEFAULT VALUES               *         
*     FOR REPORTS 50 -> 6F                                            *         
*                                                                     *         
*    INPUT:  0(R1) = A(REQWRK)                                        *         
*    RETURN: AREQTBL - A(REAL REQTBL)                                 *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
* UPDATE HISTORY:                                                     *         
* --------------                                                      *         
* 12/29/89  PJS  MOVED FROM BASE DUE TO SIZE PROBLEMS.                *         
*                                                                     *         
* MAR08/90 (MRR) --- ADD REPORTS 24/25/2A/2B/64                       *         
*                                                                     *         
* APR26/90 (MRR) --- UPDATES: SEE REREQ00                             *         
*                                                                     *         
* MAY30/90 (MRR) --- ADD DEFAULT LABEL FOR 19 REPORT.                 *         
*                                                                     *         
* AUG03/90 (MRR) --- ADD DEFAULT LABELS FOR:                          *         
*                     2E, 2F, 2G, 2H, 5C, 5D, 5E, 8F AND 8G REPORTS   *         
*                                                                     *         
* AUG06/90 (MRR) --- ADD TVB REGION, OWNER, MARKET, RANK, POINTPERSON *         
*                     AND NETWORK CONTRACT NUMBER AS COMMON FILTERS   *         
*                                                                     *         
* AUG13/90 (MRR) --- SPLIT DEFINE TABLES AND CREATE REREQ11 DUE TO    *         
*                     OVERLAY SIZE PROBLEM                            *         
*                                                                     *         
* AUG20/90 (MRR) --- MOVE OPTION 6 (ROPTN6) TO CARD 2, EVERYWHERE     *         
*                                                                     *         
* OCT05/90 (BU ) --- SET UP FOR SINGLE TABLE OVERLAY AT A TIME        *         
*                                                                     *         
* JAN24/94 (BU ) --- BREAK OUT REREQ12, NEW TABLE OVERLAY             *         
*                                                                     *         
* DEC16/94 (BU ) --- DATE CHANGE:  NO OTHER CHANGES                   *         
*                                                                     *         
* APR23/01 (BU ) --- BREAK OUT REREQ15, NEW TABLE OVERLAY             *         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         PRINT NOGEN                                                            
T80711   CSECT                                                                  
         NMOD1 0,T80711                                                         
*                                                                               
         L     R9,0(R1)                                                         
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
         LA    RE,REQTBL2          A(START OF REQTBL) FOR OTHER PHASES          
         ST    RE,AREQTBLC         SINGLE OVERLAY AT A TIME                     
         XIT1                                                                   
         PRINT GEN                                                              
         TITLE 'REREQ11 --- MACRO DEFINITIONS'                                  
       ++INCLUDE REREQMAC                                                       
         TITLE 'REREQ11 -- REQUEST DEFINITION TABLE'                            
REQTBL   EQU   *                                                                
       ++INCLUDE REREQDEF2                                                      
         DC    XL2'00'                                                          
         TITLE 'REREQ11 -- REQUEST DEFAULT DATA'                                
*                                                                               
*- REQUEST DEFAULT LISTS.                                                       
*                                                                               
*  * NOTE *  EACH REQUEST HAS ITS OWN LABEL 'DFXXX'  (XXX=REPORT ID)            
*            DO NOT BE LAZY AND SHARE LABELS -- ADD NEW ONES!                   
*                                                                               
*  FORMAT:                                                                      
* +00    XL2'DISPLACEMENT INTO REQUEST CARD(S)'                                 
*          - X'0000' = END OF DEFAULTING LIST.                                  
* +02    XL1 - DATA LENGTH (IN BYTES)                                           
* +03    CL?? - VARIABLE LENGTH DATA                                            
*                                                                               
         SPACE                                                                  
DFR52    EQU   *                                                                
DFR53    EQU   *                                                                
DFR54    EQU   *                                                                
DFR55    EQU   *                                                                
DFR56    EQU   *                                                                
DFR57    EQU   *                                                                
DFR62    EQU   *                                                                
DFR63    EQU   *                                                                
DFR68    EQU   *                                                                
DFR69    EQU   *                                                                
DFR6E    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR6B    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C' '             > OPTION 3             
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR5F    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'S'             > OPTION 3             
         DC    AL2(R2OPT6-REQCARD),X'1',C'M'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'A'           >                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR5S    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'M'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'A'           >                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR5C    EQU   *                                                                
DFR5E    EQU   *                                                                
DFR64    EQU   *                                                                
DFR65    EQU   *                                                                
DFR66    EQU   *                                                                
DFR67    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR5T    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'M'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR50    EQU   *                                                                
DFR51    EQU   *                                                                
DFR58    EQU   *                                                                
DFR59    EQU   *                                                                
DFR5A    EQU   *                                                                
DFR5B    EQU   *                                                                
DFR5D    EQU   *                                                                
DFR60    EQU   *                                                                
DFR61    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'Y'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR6C    EQU   *                                                                
DFR6D    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
*                                                                               
*- REQUEST WORK AREAS                                                           
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057REREQ11   04/23/01'                                      
         END                                                                    
