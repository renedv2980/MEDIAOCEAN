*          DATA SET FRRDJFCB   AT LEVEL 001 AS OF 03/25/16                      
*PROCESS USING(WARN(15))                                                        
*======================================================================         
*                                                                               
*  WE BELIEVE THAT THIS IS FRED ROE'S SOURCE CODE FOR THE FOLLOWING             
*  PRODUCTION LOAD MODULE:                                                      
*    'DDS.LOADLIB(FRRDJFCB)'                                                    
*    'DDS.LOADLIB(FRGETDSN)'   (ALIAS)                                          
*                                                                               
*  NOTES: THIS LOAD MODULE WAS LAST LINKED IN 1995. THIS SOURCE CODE IS         
*         BEING PLACED IN 'PAN.APPL.LIBRARY' AS A PUBLIC SERVICE, IN            
*         CASE THIS PROGRAM EVER NEEDS EXAMINATION OR MODIFICATION.             
*                                                                               
*         ALSO: THE PHASE NAME FOR THIS MODULE VIOLATES THE RULE THAT           
*         NO LOAD MODULE BASE NAME MAY EXCEED SEVEN CHARACTERS IN               
*         LENGTH. THAT ISSUE WOULD HAVE TO BE ADDRESSED IF WE EVER              
*         TRY TO PROMOTE THIS SOURCE MODULE VIA PANAPT.                         
*                                                                               
*======================================================================         
*PHASE FRRDJFCB                                                                 
*ENTRY RDJFCB                                                                   
*ALIAS FRGETDSN                                                                 
         PRINT NOGEN                                                            
         REQUS                                                                  
RDJFCB   CSECT                                                                  
RDJFCB   AMODE 31                                                               
RDJFCB   RMODE 24              PARTS OF READJFCB MUST BE BELOW LINE             
         ENTRY FRGETDSN                                                         
*********USING *,RF                                                             
FRGETDSN STM   RE,RC,12(RD)    SAVE REGS IN CALLER SAVE AREA                    
         BASR  RC,0                                                             
         USING *,RC                                                             
*                                                                               
         L     R0,SIZE                                                          
         LR    R7,R1           SAVE INPUT PARMS REGISTER                        
         USING CALLPRMS,R7                                                      
         STORAGE OBTAIN,LENGTH=(0)                                              
         LR    RA,R1                                                            
         USING DATA,RA                                                          
*                                                                               
         ST    RD,SAVE+4       SAVE CALLERS SAVE AREA IN MINE                   
         LA    R3,SAVE                                                          
         ST    R3,8(RD)        STORE MY SAVE AREA IN HIS                        
         LR    RD,R3           SET R13 TO MY SAVE AREA                          
         MVC   EYECATCH,=C'EXTRACT*'                                            
         LA    R4,JFCBAREA     GET AREA TO PUT JFCB INTO                        
         USING INFMJFCB,R4                                                      
         ST    R4,EXITLST      STORE ADDRESS IN THE EXIT LIST                   
         MVI   EXITLST,HIGHBIT+JFCBEXIT                                         
         L     R5,P1           GET ADDRESS OF DDNAME TO BE USED                 
         LA    R6,THEFILE      MY DCB W/EXIT LIST                               
         USING IHADCB,R6        MAP THE DCB                                     
         MVC   DCBDDNAM(8),0(R5)     CALLER SUPPLIED DDNAME                     
*                                                                               
         PRINT GEN                                                              
         RDJFCB  ((6)),MF=(E,RDJFCBL)  T ERRORS COME ASM MSGIEC155I             
         PRINT NOGEN                                                            
         DROP  R6                                                               
*                                                                               
         LTR   RF,RF                                                            
         BNZ   ERROR1                                                           
*                                                                               
         SR    R5,R5                                                            
         L     R9,P2                GET ADDRESS OF AREA TO PUT DSN              
         IC    R5,JFCBNVOL          NUMBER OF VOLUMES PRESENT.                  
         MVC   0(44,R9),JFCBDSNM    DATA SET NAME 44 BYTES LONG                 
         MVC   FIVEVOLS,JFCBVOLS                                                
*                                                                               
*** EXIT HERE                                                                   
EXIT     L     R0,SIZE                                                          
*NOP     STORAGE RELEASE,LENGTH=(0),ADDR=(RA)                                   
         L     RD,4(,RD)       GET PTR TO CALLERS SAVE AREA                     
         L     RE,12(,RD)      PICK UP RETURN ADDRESS                           
         LM    R0,RC,20(RD)    RESTORE REGS                                     
         BR    RE              RETURN TO CALLER                                 
*                                                                               
ERROR1   EX    (R1),DIE                                                         
         ABEND 1                                                                
*                                                                               
RDJFCBL  RDJFCB (0),MF=L                                                        
*                                                                               
DIE      EX    (RF),ERROR1                                                      
         EJECT                                                                  
HIGHBIT  EQU   X'80'           END OF LIST INDICATOR                            
JFCBEXIT EQU   X'07'           CODE FOR ORIGINAL JFCB EXIT                      
EXITLST  DS    2F                                                               
THEFILE  DCB   DSORG=PS,EXLST=(EXITLST),MACRF=(GM)                              
CALLPRMS DSECT                                                                  
P1       DS    AL4            DDNAME                                            
P2       DS    AL4            TOKEN                                             
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
         DSECT                                                                  
         IEFJFCBN  LIST=YES                                                     
*                                                                               
DATA     DSECT                                                                  
SAVE     DS    18F                                                              
JFCBAREA DS    176X                                                             
EYECATCH DS    CL8                                                              
DSN      DS    CL44                                                             
FIVEVOLS DS    5CL6                                                             
*                                                                               
RDJFCB   CSECT                                                                  
SIZE     DC    F'400'    REMEMBER THE DOUBLE WORD ALIGNMENT OF GETMAIN          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001FRRDJFCB  03/25/16'                                      
         END                                                                    
