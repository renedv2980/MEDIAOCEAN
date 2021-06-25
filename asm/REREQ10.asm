*          DATA SET REREQ10    AT LEVEL 067 AS OF 10/18/06                      
*PHASE T80710A,*                                                                
*INCLUDE HEXOUT                   *DUMMY INCLUDE*                               
*                                 USED TO RETURN END OF OVERLAY                 
*                                 ADDR                                          
RQ10     TITLE 'T80710 - REREQ10 - NEW REP REQUEST DEFINITION TABLE'            
***********************************************************************         
*                                                                     *         
*    REREQ10 (T80710) --- REQUEST PROGRAM                             *         
*                                                                     *         
*    REQUEST DEFINITION TABLE AND RECORD DEFAULT VALUES               *         
*     FOR REPORTS 00 -> 2Z                                            *         
*                                                                     *         
*    INPUT:  0(R1) = A(REQWRK)                                        *         
*    RETURN: AREQTBL1 - A(REAL REQTBL-PART1)                          *         
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
* MAR15/98 (BU ) --- SPLIT OUT 3 - 4 REPORTS INTO SEPARATE OVERLAY    *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         PRINT NOGEN                                                            
T80710   CSECT                                                                  
         NMOD1 0,T80710,RR=R2                                                   
*                                                                               
         L     R9,0(R1)                                                         
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
         LA    RE,REQTBL1          A(START OF REQTBL) FOR OTHER PHASES          
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
         TITLE 'REREQ10 -- MACRO DEFINITIONS'                                   
       ++INCLUDE REREQMAC                                                       
         TITLE 'REREQ10 -- REQUEST DEFINITION TABLE'                            
       ++INCLUDE REREQDEF1                                                      
         DC    XL2'00'                                                          
         TITLE 'REREQ10 -- REQUEST DEFAULT DATA'                                
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
DFR00    EQU   *                                                                
DFR10    EQU   *                   CHANGED FROM DEFAULT SET BELOW               
DFR11    EQU   *                                                                
DFR15    EQU   *                                                                
DFR18    EQU   *                                                                
DFR1A    EQU   *                                                                
DFR1B    EQU   *                                                                
DFR1L    EQU   *                                                                
DFR2D    EQU   *                                                                
         DC    XL2'00'             REQUEST HAS NO DEFAULT VALUES                
         SPACE 2                                                                
DFR14    EQU   *                                                                
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR1S    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'B'           >                         
*                                  OPT1 = B, EVEN THOUGH NO FIELD               
*                                     ON REQUEST SCREEN.                        
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR1K    EQU   *                   STRATEGY SEEDER                              
         DC    AL2(ROPTN2-REQCARD),X'1',C'C'          > CURR FORECST            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR19    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'Y'           > YES: GRP TOTS           
         DC    AL2(ROPTN2-REQCARD),X'1',C'Y'          > YES: RECAP              
         DC    AL2(ROPTN3-REQCARD),X'1',C' '          > DEF: NO FILTR           
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR1J    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'N'           > NO                      
         DC    AL2(ROPTN2-REQCARD),X'1',C'B'          > SORT BY BUYER           
         DC    AL2(ROPTN3-REQCARD),X'1',C'N'          > NOT LASER PRT           
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)        > CONTINUE                
         DC    AL2(R2OPT6-REQCARD),X'1',C'N'          > NO DETAIL               
         DC    XL2'00'                                                          
DFR17    EQU   *                           <---2 CARD RQST                      
DFR1C    EQU   *                           <---2 CARD RQST                      
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    XL2'00'                                                          
DFR1H    EQU   *                           <---2 CARD RQST                      
         DC    AL2(ROPTN-REQCARD),X'1',C'S'           >                         
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR1G    EQU   *                           <---2 CARD RQST                      
         DC    AL2(RSEQ-REQCARD),X'1',C'M'                                      
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR12    EQU   *                                                                
DFR13    EQU   *                                                                
DFR24    EQU   *                                                                
DFR25    EQU   *                                                                
DFR26    EQU   *                                                                
DFR27    EQU   *                                                                
DFR28    EQU   *                                                                
DFR29    EQU   *                                                                
DFR2E    EQU   *                                                                
DFR2F    EQU   *                                                                
DFR2G    EQU   *                                                                
DFR2H    EQU   *                                                                
DFR2I    EQU   *                                                                
DFR2K    EQU   *                                                                
DFR2X    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > WAS ROPTN6              
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR16    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'                                  
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    AL2(ROPTN3-REQCARD),X'1',C'N'          > CLOSEOUT REV            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR20    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'                                  
         DC    AL2(RSEQ-REQCARD),X'1',C'S'                                      
         DC    AL2(ROPTN3-REQCARD),X'1',C'S'          > STATION REPT            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR21    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'                                  
         DC    AL2(RSEQ-REQCARD),X'1',C'L'                                      
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR22    EQU   *                                                                
DFR23    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > WAS ROPTN6              
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFR2A    EQU   *                                                                
DFR2B    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(RACCTOPT-REQCARD),X'1',C'E'        > EST$/BOOKED$            
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > WAS ROPTN6              
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
**PAN#1  DC    CL21'067REREQ10   10/18/06'                                      
         END                                                                    
