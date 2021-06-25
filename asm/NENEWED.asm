*          DATA SET NENEWED    AT LEVEL 008 AS OF 08/10/00                      
*PHASE T31E0BA                                                                  
*                                                                               
         TITLE 'T31E** - NETWORK EDIT UTILITIES'                                
*******************************************************************             
*                                                                               
T31E0B   CSECT                                                                  
         PRINT NOGEN                                                            
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD                                                         
         SRL   RF,24               GET ROUTINE NUMBER                           
         SLL   RF,2                MULTIPLY BY 4                                
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
****************  ORDERED AS IN FILE NENVEQUS                                   
VBRANCH  B     VVERR               0                                            
         B     VV                  1                                            
         B     VV                  2                                            
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
VVERR    DC    H'0'                                                             
         EJECT                                                                  
***************************************************                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
VV       DS    0H                                                               
VVP  XIT B     XIT                                                              
*                                                                               
************                                                                    
*  UTILITIES: EDITING UTILITIES FOR GENERAL USE WITHIN                          
*                                                                               
**************************************************                              
* VUMOVE - STRAIGHT ALPHA MOVE TO OUPUT AREA                                    
*   INPUTS:  CUROUTA - A(CURRENT OUTPUT AREA)                                   
*            CURMAXL - MAXIMUM LENGTH OF OUTAREA                                
*            CURMAXC - MAXIMUM COLUMNS OF OUTAREA                               
*            CURLEN - THE CURRENT LENGTH OF STRING IN EDAREA                    
*            EDAREA - CONTAINS THE MAXIMUM OUTPUT STRING TO MOVE                
*   OUTPUTS: MOVES DATA TO LOCATION SPECIFIED IN CUROUTA                        
*                                                                               
*   IF CURLEN LE CURMAXL THEN MOVE FULL STRING                                  
*   ELSEIF CURMAXC EQ 1 THEN MOVE MAX LENGTH OF STRING                          
*   ELSE FOR EACH COLUMN:                                                       
*     FIND SPACE NEAREST END OF EACH COLUMN AND MOVE THAT MUCH.                 
*                                                                               
*   POSSIBLE FUTURE INPUTS - LEFT, RIGHT OR CENTER ALIGNMENT                    
***************************************************                             
         EJECT                                                                  
**************************************************                              
* VUPCOST - CONVERT COST TO INTERNAL COST FORMAT.                               
*              (ONE BYTE TO TELL IF BLANK OR ZERO, PL8 FOR NUMERIC)             
*   INPUTS:  CURFULL - FULLWORD CONTAINING COST                                 
*            ZEROBYTE- SET IF TRUE ZERO IF COST=0                               
*   OUTPUTS: CURCSTB - CURRENT COST BYTE (SET IF TRUE ZERO)                     
*            CURCSTP - PL8 PACKED VERSION OF COST                               
*                                                                               
***************************************************                             
*                                                                               
**************************************************                              
* VUADDCST - ADD CURRENT COST TO ANOTHER COST FIELD                             
*   INPUTS:  CURCSTB - CURRENT COST BYTE (SET IF TRUE ZERO)                     
*            CURCSTP - PL8 PACKED VERSION OF COST                               
*            CUROUTA - A(COST FIELD TO ADD INTO)                                
*   OUTPUTS: ADDS DATA TO COST SPECIFIED IN CUROUTA                             
*                                                                               
*   IF CURCSTP IS NON-ZERO, SET WAS NON-ZERO BIT                                
*   IF CURCSTP IS ZERO, OR IN CURCSTB TRUE ZERO BIT                             
***************************************************                             
*                                                                               
**************************************************                              
* VUPRCOST - PRINT A COST FIELD                                                 
*   INPUTS:  CURCSTB - CURRENT COST BYTE (SET IF TRUE ZERO)                     
*            CURCSTP - PL8 PACKED VERSION OF COST                               
*            CUROUTA - A(AREA TO PUT OUTPUT)                                    
*            CURMAXL - MAXIMUM LENGTH OF OUTAREA                                
*            CURMAXC - MAXIMUM COLUMNS OF OUTAREA                               
*   OUTPUTS: PRINTS DATA TO FIELD SPECIFIED BY CUROUTA                          
*                                                                               
*   IF CURCSTB IS ZERO                                                          
*     IF CURCSTP IS ZERO                                                        
*        PRINT BLANK                                                            
*        XIT                                                                    
*   PRINT COST WITH DOLLAR SIGN, NEGATIVE SIGN IN EDAREA                        
*   SET CURLEN BASED ON THIS.                                                   
*   IF CURLEN <= CURMAXL THEN MOVE WHOLE THING                                  
*   ELSEIF CURLEN =CURMAXL-1 THEN DROP $ SIGN AND MOVE WHOLE THING              
*   ELSEIF CURMAXC=1 THEN DROP CENTS, USE M ETC. UNTIL IT FITS                  
*   ELSE LOOK FOR A COLUMN WHERE WHOLE STRING FITS                              
*      IF NO FIT, USE ABOVE ON LARGEST SPACE                                    
*                                                                               
*   POSSIBLE FUTURE INPUTS - NO DECIMAL PRINTING.                               
***************************************************                             
         EJECT                                                                  
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
* LITERAL POOL                                                                  
         LTORG                                                                  
*****                                                                           
**** ADDRESSABLE NEEDS                                                          
WORKAREA DS    CL64                                                             
EDWORK   DS    CL64                                                             
SYSRB    DS    A                                                                
**** ARGS PASSED AROUND                                                         
CUROUTA  DS    A                   A(CURRENT OUTPUT AREA)                       
CURMAXL  DS    CL1                 MAXIMUM LENGTH                               
CURMAXC  DS    CL1                 MAXIMUM COLUMNS                              
CURLEN   DS    CL1                 LENGTH OF DATA IN EDWORK                     
CURINA   DS    A                   A(CURRENT INPUT AREA)                        
CURCSTB  DS    CL1                 BYTE TO DESCRIBE THE COST FIELD              
CURCSTP  DS    PL8                 PACKED COST                                  
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NETDEMOD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008NENEWED   08/10/00'                                      
         END                                                                    
