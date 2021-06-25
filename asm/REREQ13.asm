*          DATA SET REREQ13    AT LEVEL 089 AS OF 01/10/02                      
*PHASE T80713A,*                                                                
RQ13     TITLE 'T80713 - REREQ13 - NEW REP REQUEST DEFINITION TABLE'            
***********************************************************************         
*                                                                     *         
*    REREQ13 (T80713) --- REQUEST PROGRAM                             *         
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
* JUL31/96 (BU ) --- NEW OVERLAY MODULE FOR TABLE                     *         
*                                                                     *         
*                                                                     *         
*                    ***  END TABLE  ***                              *         
***********************************************************************         
         PRINT NOGEN                                                            
T80713   CSECT                                                                  
         NMOD1 0,T80713                                                         
*                                                                               
         L     R9,0(R1)                                                         
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
         LA    RE,REQTBL4          A(START OF REQTBL) FOR OTHER PHASES          
         ST    RE,AREQTBLC         SINGLE OVERLAY AT A TIME                     
         XIT1                                                                   
         PRINT GEN                                                              
         TITLE 'REREQ13 --- MACRO DEFINITIONS'                                  
       ++INCLUDE REREQMAC                                                       
         TITLE 'REREQ13 -- REQUEST DEFINITION TABLE'                            
REQTBL   EQU   *                                                                
       ++INCLUDE REREQDEF4                                                      
         DC    XL2'00'                                                          
         TITLE 'REREQ13 -- REQUEST DEFAULT DATA'                                
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
DFRTK    EQU   *                                                                
         DC    AL2(ROPTN-REQCARD),X'1',C'O'             >OFFICE                 
         DC    AL2(ROPTN2-REQCARD),X'1',C'A'            >ALL                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRPK    EQU   *                                                                
         DC    AL2(ROPTN2-REQCARD),X'1',C'B'            >ALL                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'M'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRRK    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'P'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRVR    EQU   *                                        >MKT ADV/PRD            
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRMP    EQU   *                                        >MKT ADV/PRD            
         DC    AL2(RACCTOPT-REQCARD),X'1',C'S'                                  
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRP0    EQU   *                                                                
DFRP2    EQU   *                                                                
DFRP3    EQU   *                                                                
DFRP4    EQU   *                                                                
DFRP5    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'Y'             > ALL COLUMNS          
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRPP    EQU   *                                                                
DFRN1    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             > WAS ROPTN6           
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRRS    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(RACCTOPT-REQCARD),X'1',C'B'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFROR    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN3-REQCARD),X'1',C'Y'                                    
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRSZ    EQU   *                                                                
DFRWB    EQU   *                                                                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(ROPTN2-REQCARD),X'1',C'S'                                    
         DC    AL2(ROPTN3-REQCARD),X'1',C'F'                                    
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
DFRPS    EQU   *                                        >COMP S/P               
***      DC    AL2(RACCTOPT-REQCARD),X'1',C'A'          >MANAGER'S RPT          
         DC    AL2(ROPTN2-REQCARD),X'1',C'A'            >MANAGER'S RPT          
****     DC    AL2(ROPTN3-REQCARD),X'1',C'M'            >MARKET $$              
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRK1    EQU   *                                       +>KATZ SERIES            
DFRK3    EQU   *                                       +>KATZ SERIES            
DFRK5    EQU   *                                       +>KATZ SERIES            
DFRK7    EQU   *                                       +>KATZ SERIES            
DFRK2    EQU   *                                       +>KATZ SERIES            
DFRK4    EQU   *                                       +>KATZ SERIES            
DFRK6    EQU   *                                       +>KATZ SERIES            
DFRKC    EQU   *                                       +>KATZ SERIES            
DFRKD    EQU   *                                       +>KATZ SERIES            
         DC    AL2(ROPTN3-REQCARD),X'1',C'V'            >VIDEO FORMAT           
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRKP    EQU   *                                       +>KATZ SERIES            
DFRKQ    EQU   *                                       +>KATZ SERIES            
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRK8    EQU   *                                       +>KATZ SERIES            
         DC    AL2(ROPTN-REQCARD),X'1',C'Y'             >3-YEAR TRACK           
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(ROPTN3-REQCARD),X'1',C'Y'            >3YEAR REPORT           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRK9    EQU   *                                       +>KATZ SERIES            
DFRK0    EQU   *                                       +>KATZ SERIES            
DFRKA    EQU   *                                       +>KATZ SERIES            
DFRKB    EQU   *                                       +>KATZ SERIES            
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRNO    EQU   *                                                                
DFRXK    EQU   *                                                                
DFRXL    EQU   *                                                                
DFRXM    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRNM    EQU   *                                                                
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(ROPTN-REQCARD),X'1',C'A'             >MKT/ADV SEQ            
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'             >WAS ROPTN6            
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRX3    EQU   *                                        >PENDING REPORT         
DFRPY    EQU   *                                        >PENDING REPORT         
         DC    AL2(RACCTOPT-REQCARD),X'1',C'S'                                  
         DC    AL2(ROPTN-REQCARD),X'1',C'P'             >PENDING                
         DC    AL2(ROPTN3-REQCARD),X'1',C'M'            >MARKET $$              
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRPX    EQU   *                                        >COMPANY REPORT         
*                                                       >(TEMPORARY)            
         DC    AL2(ROPTN-REQCARD),X'1',C'F'             >PENDING                
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRSR    EQU   *                                        >SHARE REPORT           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'S'                                  
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(ROPTN3-REQCARD),X'1',C'B'             > ALL COLUMNS          
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRSS    EQU   *                                        >SHARE REPORT           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'D'                                  
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(ROPTN3-REQCARD),X'1',C'B'             > ALL COLUMNS          
         DC    XL2'00'                                                          
         SPACE 2                                                                
DFRSO    EQU   *                                        >SHARE REPORT           
         DC    AL2(RACCTOPT-REQCARD),X'1',C'O'                                  
         DC    AL2(CTINCOL),X'1',AL1(CTINCHAR)                                  
         DC    AL2(R2OPT6-REQCARD),X'1',C'A'            >WAS ROPTN6             
         DC    AL2(ROPTN3-REQCARD),X'1',C'B'             > ALL COLUMNS          
         DC    XL2'00'                                                          
         SPACE 2                                                                
*                                                                               
*- REQUEST WORK AREAS                                                           
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089REREQ13   01/10/02'                                      
         END                                                                    
