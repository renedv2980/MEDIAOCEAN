*          DATA SET CTMAD11    AT LEVEL 005 AS OF 05/01/02                      
*PHASE TA0C11A,*                                                                
         TITLE 'TA0C11 - $MAD DOWNLOAD RADIO CALL LETTER CHANGES'               
TA0C11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C11,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
*                                  INITIALIZE SYSTEM                            
         GOTO1 SETSYS,DMCB,=C'SPOT',=CL8'SPTDIR',=CL8'SPTFIL'                   
         BNE   EXIT                                                             
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE REQUEST              
* OBJECT AND STARTS FILLING THE FRAME WITH ANY CALL LETTER CHANGE               
* OBJECTS.                                                                      
*                                                                               
PROCSTRT NTR1  ,                                                                
         GOTO1 GETITEM             VALIDATE REQUEST OBJECT                      
         BNE   EXIT                                                             
         CLC   TYPENUM,=A(ITDNLCAL)                                             
         BNE   INVLOBJ                                                          
         CLC   DATALEN,=A(REQLEN)                                               
         BNE   INVLOBJL                                                         
*                                                                               
         L     RF,ADATA            MOVE REQUEST DATA TO LOCAL STORAGE           
         MVC   REQUEST(REQLEN),0(RF)                                            
*                                                                               
         CLI   REQSUB,C'R'         TEST FOR RADIO                               
         BNE   INVLSUB             INVALID SUB-FILE                             
         CLI   REQSRC,C'A'         TEST SOURCE=ARB                              
         BNE   INVLSRC             NO                                           
*                                                                               
* GET A(STATION CALL LETTER TABLE) AND SET POINTERS                             
*                                                                               
         MVC   TABLIST,TABLISTL                                                 
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         GOTO1 CDEMADDR,DMCB,(X'FF',TABLIST),ACOMFACS                           
         L     R4,ASTATAB                                                       
         USING STAHDRD,R4                                                       
         ST    R4,ASTAHEAD                                                      
         LA    RE,STAHDRD+STAHDRLN                                              
         ST    RE,ASTADATA                                                      
         BAS   RE,PROCSTA          PROCESS THE STATION TABLE                    
*                                                                               
PSX      B     XIT                                                              
         SPACE 2                                                                
* TERMINATE DOWNLOAD REQUEST DUE TO ERROR IN REQUEST OBJECT                     
*                                                                               
*                                  INVALID OBJECT CODE                          
INVLOBJ  MVC   APPLERR,=Y(ER11OBCD)                                             
         B     ERROBJ                                                           
*                                  INVALID OBJECT LENGTH                        
INVLOBJL MVC   APPLERR,=Y(ER11OBLN)                                             
         B     ERROBJ                                                           
*                                  INVALID SUBFILE CODE                         
INVLSUB  MVC   APPLERR,=Y(ER11SUB)                                              
         B     ERROBJ                                                           
*                                  INVALID SOURCE CODE                          
INVLSRC  MVC   APPLERR,=Y(ER11SRC)                                              
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,ITAMFMER,4,FULL                                     
         BNE   EXIT                                                             
*                                  PUT END OF DATA ITEM                         
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT CURRENTLY DOES NOTHING.               
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         BAS   RE,PROCSTA                                                       
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE STATION CALL LETTER CHANGE TABLE                   
*                                                                               
PROCSTA  NTR1  ,                                                                
         L     R4,ASTAHEAD         R4=A(STATION HEADER)                         
         USING STAHDRD,R4                                                       
         L     R6,ASTADATA         R6=A(STATION DATA ELEMENT)                   
         USING STADTAD,R6                                                       
         CLI   OVERMODE,C'M'       TEST MIDDLE MODE                             
         BNE   PS10                NO                                           
         OC    STASRC(2),STASRC    TEST IF PREVIOUSLY AT EOT                    
         BZ    PS90                YES                                          
         B     PS22                NO-RESUME PROCESSING                         
*                                                                               
PS10     OC    STASRC(2),STASRC    TEST FOR EOT                                 
         BZ    PS90                                                             
         CLC   REQSRC,STASRC       MATCH ON SOURCE                              
         BNE   PS15                                                             
         CLC   REQSUB,STAMED       MATCH ON SUB-FILE(MEDIA)                     
         BNE   PS15                                                             
*                                                                               
         MVC   BOOK,STASBOOK                                                    
         XC    BOOK,=X'FFFF'       REVERSE BOOK COMPLEMENT                      
         CLC   BOOK,LOWBOOK        IGNORE OLD DATA                              
         BH    PS20                                                             
*                                                                               
PS15     ICM   R4,7,STAAET                                                      
         LA    R4,1(R4)            POINT TO NEXT HEADER                         
         B     PS10                                                             
*                                                                               
PS20     ST    R4,ASTAHEAD                                                      
         LA    R6,STAHDRLN(R4)     R6=A(FIRST DATA ENTRY)                       
*                                                                               
PS22     SR    R2,R2                                                            
         ICM   R2,3,STALDE         BXLE INCREMENT=L'DATA ENTRY                  
         SR    R3,R3                                                            
         ICM   R3,7,STAAET         BXLE LIMIT = EOT - 1                         
         BCTR  R3,0                                                             
*                                                                               
PS25     ST    R6,ASTADATA                                                      
         MVC   RETOCALL,STAOCALL                                                
         MVC   RETNCALL,STANCALL                                                
         GOTO1 HEXOUT,DMCB,BOOK,RETBOOK,2                                       
         MVI   RETTYPE,RETTCALL                                                 
         GOTO1 PUTITEM,DMCB,ITRDCALL,L'RETURN,RETURN                            
         BNE   EXIT                                                             
         CLI   EOFFLAG,C'Y'        TEST IF FRAME FILLED                         
         BE    PSTAX                                                            
*                                                                               
         BXLE  R6,R2,PS25                                                       
         B     PS15                                                             
*                                                                               
PS90     ST    R4,ASTAHEAD                                                      
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
         CLI   EOFFLAG,C'Y'        TEST FOR END OF FRAME                        
         BE    PSTAX                                                            
         MVI   MDLAST,C'Y'         SET LAST FRAME                               
         B     EXIT                                                             
*                                                                               
PSTAX    B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
* CONSTANT TO DEFINE OLD DATA (PRE 9/90)                                        
*                                                                               
LOWBOOK  DC    AL1(91,06)                                                       
         SPACE 2                                                                
* STATION TABLE LIST FOR DEMADDR                                                
*                                                                               
TABLISTL DC    X'D2',3X'00',X'FF'                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
*                                                                               
BOOK     DS    XL2                                                              
*                                                                               
ASTAHEAD DS    A                   STATION TABLE HEADER POINTER                 
ASTADATA DS    A                   STATION TABLE DATA POINTER                   
TABLIST  DS    0XL5                                                             
ASTATAB  DS    A                                                                
         DS    XL1                                                              
*                                                                               
*                                                                               
REQUEST  DS    0C                  REQUEST OBJECT                               
REQSUB   DS    CL1                 SUB-FILE(R=RADIO, T=USTV)                    
REQSRC   DS    CL1                 SOURCE(A=ARB,N=NSI)                          
REQDATE  DS    CL6                 EFFECTIVE DATE OR '000000'                   
REQLEN   EQU   *-REQUEST                                                        
*                                                                               
RETURN   DS    0CL15               RETURN OBJECT                                
RETOCALL DS    CL5                 OLD CALL LETTERS                             
RETNCALL DS    CL5                 NEW CALL LETTERS                             
RETBOOK  DS    0CL4                                                             
RETBYEAR DS    CL2                 BOOK YEAR                                    
RETBMON  DS    CL2                 BOOK MONTH                                   
RETTYPE  DS    CL1                                                              
RETTCALL EQU   C'0'                CALL LETTER CHANGE                           
RETTREUS EQU   C'1'                REUSE OF CALL LETTERS                        
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
* DDCOMFACS                                                                     
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTMAD11   05/01/02'                                      
         END                                                                    
