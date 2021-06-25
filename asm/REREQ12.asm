*          DATA SET REREQ12    AT LEVEL 074 AS OF 10/18/11                      
*PHASE T80712A,*                                                                
RQ12     TITLE 'T80712 - REREQ12 - NEW REP REQUEST DEFINITION TABLE'            
***********************************************************************         
*                                                                     *         
*    REREQ12 (T80712) --- REQUEST PROGRAM                             *         
*                                                                     *         
*    REQUEST DEFINITION TABLE AND RECORD DEFAULT VALUES               *         
*     FOR REPORTS A0 -> ZZ                                            *         
*                                                                     *         
*    INPUT:  0(R1) = A(REQWRK)                                        *         
*    RETURN: AREQTBL - A(REAL REQTBL)                                 *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
* UPDATE HISTORY:                                                     *         
* --------------                                                      *         
* JAN24/94 (BU ) --- NEW OVERLAY MODULE FOR TABLE                     *         
*                                                                     *         
* JUL31/96 (BU ) --- SPLIT INTO REQ12/REQ13                           *         
*                                                                     *         
*                                                                     *         
*                    ***  END TABLE  ***                              *         
***********************************************************************         
         PRINT NOGEN                                                            
T80712   CSECT                                                                  
         NMOD1 0,T80712                                                         
*                                                                               
         L     R9,0(R1)                                                         
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
         LA    RE,REQTBL3          A(START OF REQTBL) FOR OTHER PHASES          
         ST    RE,AREQTBLC         SINGLE OVERLAY AT A TIME                     
         XIT1                                                                   
         PRINT GEN                                                              
         TITLE 'REREQ12 --- MACRO DEFINITIONS'                                  
       ++INCLUDE REREQMAC                                                       
         TITLE 'REREQ12 -- REQUEST DEFINITION TABLE'                            
REQTBL   EQU   *                                                                
       ++INCLUDE REREQDEF3                                                      
         DC    XL2'00'                                                          
         TITLE 'REREQ12 -- REQUEST DEFAULT DATA'                                
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
DFRCI    EQU   *                                                                
DFRAH    EQU   *                                                                
DFRFW    EQU   *                                                                
DFRFZ    EQU   *                                                                
         DC    XL2'00'             REQUEST HAS NO DEFAULT VALUES                
         SPACE 2                                                                
DFRA0    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C' '             > PRODUCT=NO           
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRA1    EQU   *                                                                
DFRB0    EQU   *                                                                
DFRB1    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'Y'             > ALL COLUMNS          
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRBD    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(RAGY-REQCARD),X'4',C'????'            >FORCE AN              
*                                                        >UNKNOWN AGY           
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRAC    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(RACCTOPT-REQCARD),X'1',C'A'           >AVAILS                
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >OPTION 6              
         DC    XL2'00'                                                          
DFRAO    EQU   *                                                                
DFRAS    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'N'             > ALL COLUMNS          
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRBL    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRD0    EQU   *                                                                
DFRD1    EQU   *                                                                
DFRD2    EQU   *                                                                
DFRD3    EQU   *                                                                
DFRD6    EQU   *                                                                
DFRD7    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'Y'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRG0    EQU   *                                   >'GEORGE'-SERIES             
DFRG1    EQU   *                                                                
DFRG2    EQU   *                                                                
DFRG3    EQU   *                                                                
DFRG4    EQU   *                                                                
DFRG5    EQU   *                                                                
DFRG8    EQU   *                                                                
DFRG9    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'C'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(RBEST$OP-REQCARD),X'1',C' '          BEST DOLLARS            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRC0    EQU   *                                                                
DFRC1    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'C'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(RBEST$OP-REQCARD),X'1',C' '          BEST DOLLARS            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRC3    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'X'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRFR    EQU   *                                        >FORECAST RPT           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'S'                                  
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRPR    EQU   *                                        >PENDING REPORT         
         DC    AL2(RACCTOPT-REQCARD),X'1',C'S'                                  
         DC    AL2(ROPTN-REQCARD),X'1',C'P'             >PENDING                
         DC    AL2(ROPTN3-REQCARD),X'1',C'M'            >MARKET $$              
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRF0    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'C'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRCO    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'          > WAS ROPTN6              
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN-REQCARD),X'1',C'F'                                     
         DC    XL2'00'                                                          
         SPACE 2                                                                
*                                                                               
*- REQUEST WORK AREAS                                                           
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074REREQ12   10/18/11'                                      
         END                                                                    
