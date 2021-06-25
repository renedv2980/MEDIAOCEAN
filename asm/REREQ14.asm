*          DATA SET REREQ14    AT LEVEL 073 AS OF 02/09/05                      
*PHASE T80714A,*                                                                
*INCLUDE HEXOUT                   *DUMMY INCLUDE*                               
*                                 USED TO RETURN END OF OVERLAY                 
*                                 ADDR                                          
RQ14     TITLE 'T80714 - REREQ14 - NEW REP REQUEST DEFINITION TABLE'            
***********************************************************************         
*                                                                     *         
*    REREQ14 (T80714) --- REQUEST PROGRAM                             *         
*                                                                     *         
*    REQUEST DEFINITION TABLE AND RECORD DEFAULT VALUES               *         
*     FOR REPORTS 30 -> 4Z                                            *         
*                                                                     *         
*    INPUT:  0(R1) = A(REQWRK)                                        *         
*    RETURN: AREQTBL5 - A(REAL REQTBL)                                *         
*            0(R1) = A(END OF THIS OVERLAY)                           *         
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
*                    PASS BACK OV END ADDR IN P1                      *         
*                                                                     *         
* AUG20/90 (MRR) --- MOVE OPTION 6 TO CARD 2, EVERYWHERE              *         
*                                                                     *         
* OCT05/90 (BU ) --- SET UP FOR SINGLE TABLE OVERLAY AT A TIME        *         
*                                                                     *         
* JUN06/91 (BU ) --- ADD DEFAULTS FOR RE30 (YADR)                     *         
*                                                                     *         
* FEB07/92 (BU ) --- ADD OPTION2 TO 1B REPORT                         *         
*                                                                     *         
* JUL09/93 (SKU) --- ADD OPTION3 TO 1J REPORT                         *         
*                                                                     *         
* FEB22/94 (SKU) --- ADD OPTION4 TO 1J REPORT                         *         
*                                                                     *         
* SEP30/94 (SKU) --- ADD SPOT LENGTH BREAK OUT OPTION TO 30           *         
*                                                                     *         
* MAR15/99 (BU ) --- SPLIT OUT 3/4 REPORTS INTO SEPARATE OVERLAY      *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         PRINT NOGEN                                                            
T80714   CSECT                                                                  
         NMOD1 0,T80714,RR=R2                                                   
*                                                                               
         L     R9,0(R1)                                                         
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
         LA    RE,REQTBL5          A(START OF REQTBL) FOR OTHER PHASES          
         ST    RE,AREQTBLC SINGLE OVERLAY AT A TIME                             
*                                                                               
         L     RE,=V(HEXOUT)                                                    
         AR    RE,R2                                                            
         ST    RE,0(R1)           PASS BACK ADDR IN P1                          
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
*        WE NEED AN LTORG HERE FOR V(HEXOUT)                                    
*                                                                               
         LTORG                                                                  
         PRINT GEN                                                              
         TITLE 'REREQ14 -- MACRO DEFINITIONS'                                   
       ++INCLUDE REREQMAC                                                       
         TITLE 'REREQ14 -- REQUEST DEFINITION TABLE'                            
REQTBL   EQU   *                                                                
       ++INCLUDE REREQDEF5                                                      
         DC    XL2'00'                                                          
         TITLE 'REREQ14 -- REQUEST DEFAULT DATA'                                
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
*        IF BYTES 1-3 OF DATA = C'PRO', REQUEST PROFILE IS TO BE                
*           USED.  THE NEXT BYTE IS THE BYTE DISPLACEMENT TO CHECK              
*           (ZERO RELATIVE).  THE NEXT BYTE IS THE BIT DISPLACEMENT             
*           IN THE BYTE.  THIS IS THE SHIFT FACTOR, TO LINE UP TEST.            
*           THE NEXT TWO BYTES ARE THE SETTING VALUES.  FIRST IS THE            
*           'ON' VALUE, FOLLOWED BY THE OFF-VALUE.                              
*                                                                               
         SPACE                                                                  
DFR3F    EQU   *                                                                
DFR3U    EQU   *                                                                
DFR4S    EQU   *                                                                
DFR4Y    EQU   *                                                                
         DC    XL2'00'             REQUEST HAS NO DEFAULT VALUES                
         SPACE 2                                                                
DFR41    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C' '           >                         
         DC    AL2(ROPTN3-REQCARD),X'1',C' '           >                        
         DC    XL2'00'                                                          
DFR43    EQU   *                                                                
DFR44    EQU   *                                                                
DFR48    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN-REQCARD),X'1',C' '           >                         
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > DEF: ALLMONTH           
         DC    XL2'00'                                                          
DFR4R    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'R'        > DEF: MARKET             
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > DEF: ALLMONTH           
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)        > CONTINUE                
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR3V    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'S'        > DEF: STAMKT             
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > DEF: ALLMONTH           
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR45    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'        > DEF: DETAIL             
         DC    AL2(ROPTN3-REQCARD),X'1',C'S'          > DEF: STATION            
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > DEF: ALLMONTH           
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR3M    EQU   *                                                                
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > DEF: ALLMONTH           
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR4B    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C' '           > NO                      
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > WAS ROPTN6              
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR37    EQU   *                                                                
DFR4G    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'                                  
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR3G    EQU   *                                                                
DFR3H    EQU   *                                                                
DFR3J    EQU   *                                                                
DFR3I    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'                                  
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR46    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > WAS ROPTN6              
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR30    EQU   *                            > RADAR REPORT DEFAULTS             
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    AL2(ROPTN-REQCARD),X'1',C'D'           > DETAIL                  
         DC    AL2(ROPTN2-REQCARD),X'1',C'S'          > SPOT                    
         DC    AL2(ROPTN3-REQCARD),X'1',C'N'          > NONE                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)        > CONTINUE                
         DC    AL2(R2OPT6-REQCARD),X'1',C'N'          > NONE                    
         DC    AL2(RSPLNOPT-REQCARD),X'1',C'N'        > NO                      
*                                                                               
*  THIS IS BEING INSERTED INTO THE FIRST POSITION OF CARD # 2                   
*                                                                               
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR34    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    AL2(ROPTN-REQCARD),X'1',C' '           > OFFICES                 
         DC    AL2(ROPTN2-REQCARD),X'1',C'C'                                    
         DC    AL2(RACCTOPT-REQCARD),X'1',C'A'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR36    EQU   *                                                                
DFR38    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR3K    EQU   *                                                                
DFR3N    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'        >ACCOUNTING OPT           
         DC    AL2(ROPTN-REQCARD),X'1',C'M'           >OPTION1                  
         DC    AL2(RSEQ-REQCARD),X'1',C'S'            >SEQUENCE                 
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR40    EQU   *                                                                
DFR42    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'M'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR47    EQU   *                                                                
         DC    AL2(ROPTN2-REQCARD),X'7',C'PRO',X'0',X'0',C'Y',C'N'              
*                                                                               
*   NOTE:  THIS IS PROFILE REFERENCE: 1ST BYTE/1ST BIT (ZERO REL) -             
*          IF ON,  OPTION2 IS SET TO Y, ELSE SET TO N                           
*                                                                               
         DC    AL2(RACCTOPT-REQCARD),X'1',C'A'                                  
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    AL2(ROPTN-REQCARD),X'1',C'M'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
*                                                                               
*                                                                               
*  REQUEST WORK AREAS                                                           
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073REREQ14   02/09/05'                                      
         END                                                                    
