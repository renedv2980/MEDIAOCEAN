*          DATA SET REREQ15    AT LEVEL 061 AS OF 10/18/06                      
*PHASE T80715A,*                                                                
RQ15     TITLE 'T80715 - REREQ15 - NEW REP REQUEST DEFINITION TABLE'            
***********************************************************************         
*                                                                     *         
*    REREQ15 (T80715) --- REQUEST PROGRAM                             *         
*                                                                     *         
*    REQUEST DEFINITION TABLE AND RECORD DEFAULT VALUES               *         
*     FOR REPORTS 70 -> 9Z                                            *         
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
* APR23/01 (BU ) --- ADDED                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         PRINT NOGEN                                                            
T80715   CSECT                                                                  
         NMOD1 0,T80715                                                         
*                                                                               
         L     R9,0(R1)                                                         
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
         LA    RE,REQTBL6          A(START OF REQTBL) FOR OTHER PHASES          
         ST    RE,AREQTBLC         SINGLE OVERLAY AT A TIME                     
         XIT1                                                                   
         PRINT GEN                                                              
         TITLE 'REREQ15 --- MACRO DEFINITIONS'                                  
       ++INCLUDE REREQMAC                                                       
         TITLE 'REREQ15 -- REQUEST DEFINITION TABLE'                            
REQTBL   EQU   *                                                                
       ++INCLUDE REREQDEF6                                                      
         DC    XL2'00'                                                          
         TITLE 'REREQ15 -- REQUEST DEFAULT DATA'                                
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
DFR72    EQU   *                                                                
DFR75    EQU   *                                                                
DFR76    EQU   *                                                                
DFR77    EQU   *                                                                
DFR78    EQU   *                                                                
DFR79    EQU   *                                                                
DFR7A    EQU   *                                                                
DFR7F    EQU   *                                                                
DFR95    EQU   *                                                                
         DC    XL2'00'             REQUEST HAS NO DEFAULT VALUES                
         SPACE 2                                                                
DFR8G    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR8F    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'           >DETAIL                
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR8E    EQU   *                                                                
DFR8N    EQU   *                                                                
DFR8Y    EQU   *                                                                
DFR8Z    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'           >DETAIL                
         DC    AL2(ROPTN3-REQCARD),X'1',C'N'             >EXCLUDE               
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >OPTION 6              
         DC    XL2'00'                                                          
DFR8A    EQU   *                                                                
DFR8B    EQU   *                                                                
DFR8C    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR70    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'A'                                     
         DC    AL2(ROPTN2-REQCARD),X'1',C'B'                                    
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR90    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'S'                                     
         DC    AL2(ROPTN2-REQCARD),X'1',C'A'                                    
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR91    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'A'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR71    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'O'                                     
         DC    AL2(ROPTN2-REQCARD),X'1',C'O'                                    
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR73    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'B'                                     
         DC    AL2(ROPTN2-REQCARD),X'1',C'D'                                    
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR74    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'N'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR7B    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'E'                                     
         DC    AL2(ROPTN2-REQCARD),X'1',C'A'                                    
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR80    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'L'                                      
         DC    AL2(ROPTN-REQCARD),X'1',C'S'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR81    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'M'                                      
         DC    AL2(ROPTN-REQCARD),X'1',C'S'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR82    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'S'                                     
         DC    AL2(RSEQ-REQCARD),X'1',C'M'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR84    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    AL2(ROPTN-REQCARD),X'1',C'S'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR85    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'B'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
*                                                                               
*- REQUEST WORK AREAS                                                           
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061REREQ15   10/18/06'                                      
         END                                                                    
