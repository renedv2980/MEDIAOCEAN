*          DATA SET CTMAD0D    AT LEVEL 107 AS OF 07/01/02                      
*PHASE TA0C0DA,*                                                                
         TITLE 'TA0C0D - $MAD DOWNLOAD ESTIMATE RECORD'                         
TA0C0D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C0D,RA                                                      
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
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE INQUIRY              
* FILTER OBJECT PASSED BY ALINK AND RETURNS THE FIRST FRAME FULL OF             
* INQUIRY OBJECTS.                                                              
*                                                                               
PROCSTRT NTR1                                                                   
         GOTO1 GETITEM             VALIDATE REQUEST OBJECT                      
         BNE   EXIT                                                             
         CLC   TYPENUM,=A(ITVALAGY)         AGENCY DATA                         
         BNE   PS5                                                              
*                                                                               
         XC    KEY,KEY             BUILD AGENCY KEY                             
         LA    R4,KEY                                                           
         USING AGYHDRD,R4                                                       
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,SIGNON2C                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                IF AGENCY KEY NOT FOUND                      
         CLC   KEY(L'AGYKEY),KEYSAVE                                            
         BNE   ERRAGY              THEN ERROR                                   
         GOTO1 GETREC              ELSE GET RECORD                              
*                                                                               
         USING AGYHDRD,R6                                                       
         L     R6,AIO                                                           
         MVC   RETANAME,AGYNAME                                                 
         MVC   RETADDR,AGYADDR                                                  
                                                                                
PS2      GOTO1 PUTITEM,DMCB,ITAGYRET,L'RETURN4,RETURN4                          
         BNE   EXIT                                                             
         B     PS50                                                             
                                                                                
PS5      CLC   TYPENUM,=A(ITVALSCH)         ESTIMATE DATA                       
         BNE   ERRVSRQ                                                          
         CLC   DATALEN,=A(L'REQUEST)                                            
         BNE   ERRVSRQ                                                          
*                                                                               
         L     RF,ADATA            MOVE REQUEST DATA TO LOCAL STORAGE           
         MVC   REQUEST,0(RF)                                                    
         OC    REQUEST,=CL9' '                                                  
*                                  VALIDATE MEDIA                               
         CLI   REQMED,C'R'         IF MEDIA NOT R, THEN USE T                   
         BE    *+8                                                              
         MVI   REQMED,C'T'                                                      
         GOTO1 VALIMED,DMCB,REQMED                                              
         BNE   ERROBJ                                                           
*                                  VALIDATE CLIENT                              
         GOTO1 VALICLT,DMCB,REQCLT                                              
         BNE   ERROBJ                                                           
         CLC   PCACTION,=Y(ACVALSC2)  IF THIS IS NEW DOWNLOAD                   
         BE    PS7                                                              
         CLC   PCACTION,=Y(ACVALSC3)                                            
         BNE   PS10                                                             
*                                  VALIDATE PRODUCT                             
PS7      L     R6,AIO              SAVE CLT NAME                                
         USING CLTHDRD,R6                                                       
         MVC   RETCNAME,CNAME                                                   
         MVC   SVOFFC,COFFICE                                                   
         MVC   SVCACCS,CACCESS                                                  
*                                                                               
         BRAS  RE,CALLOFCR                                                      
         BNE   ERRNOCL                                                          
*                                  VALIDATE PRODUCT                             
PS10     GOTO1 VALIPRD,DMCB,REQPRD                                              
         BNE   ERROBJ                                                           
         CLC   PCACTION,=Y(ACVALSC2)  IF THIS IS NEW DOWNLOAD                   
         BE    PS15                                                             
         CLC   PCACTION,=Y(ACVALSC3)                                            
         BNE   PS20                                                             
*                                  VALIDATE PRODUCT                             
PS15     L     R6,AIO              SAVE PRD NAME                                
         USING PRDHDRD,R6                                                       
         MVC   RETPNAME,PNAME                                                   
*                                  VALIDATE ESTIMATE                            
PS20     GOTO1 VALIEST,DMCB,REQEST                                              
         BNE   ERROBJ                                                           
         CLC   PCACTION,=Y(ACVALSC2)  IF THIS IS NEW DOWNLOAD                   
         BE    PS25                                                             
         CLC   PCACTION,=Y(ACVALSC3)                                            
         BNE   PS30                                                             
*                                                                               
PS25     L     R6,AIO              SAVE EST NAME                                
         USING ESTHDRD,R6                                                       
         MVC   RETENAME,EDESC                                                   
         MVI   RETOWFLG,C'N'                                                    
         CLI   EOWSDAY,0                                                        
         BE    *+8                                                              
         MVI   RETOWFLG,C'Y'                                                    
         CLC   PCACTION,=Y(ACVALSC3)                                            
         BNE   PS30                                                             
         MVI   RETDWFLG,C'W'                                                    
         CLI   EDAILY,C'Y'                                                      
         BNE   *+8                                                              
         MVI   RETDWFLG,C'D'                                                    
*                                                                               
PS30     MVC   RETSTART,ESTSTRT                                                 
         CLI   RETSTART,X'FA'                                                   
         BL    PS31                                                             
         ZIC   R1,RETSTART                                                      
         S     R1,=F'10'                                                        
         STC   R1,RETSTART                                                      
PS31     MVC   RETEND,ESTEND                                                    
         CLI   RETEND,X'FA'                                                     
         BL    PS32                                                             
         ZIC   R1,RETEND                                                        
         S     R1,=F'10'                                                        
         STC   R1,RETEND                                                        
PS32     GOTO1 HEXOUT,DMCB,ESTBOOK,RETBYEAR,1                                   
         GOTO1 HEXOUT,DMCB,ESTBOOK+1,RETBMON,1                                  
*                                                                               
         XC    RETDEMO,RETDEMO                                                  
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,REQMED                                                  
         GOTO1 DEMOCON,DMCB,(14,ESTDEMOS),(6,RETDEMO),DBLOCK                    
         OC    RETDEMO,=CL84' '                                                 
*                                                                               
         CLC   PCACTION,=Y(ACVALSC2)  IF THIS IS NEW DOWNLOAD                   
         BE    PS35                                                             
         CLC   PCACTION,=Y(ACVALSC3)                                            
         BNE   PS40                                                             
*                                                                               
PS35     MVC   RETURN2(L'RETURN),RETURN   MOVE OBJECT TO RETURN2                
         CLC   PCACTION,=Y(ACVALSC3)                                            
         BNE   PS37                                                             
         GOTO1 PUTITEM,DMCB,ITESTRET,L'RETURN3,RETURN3                          
         BNE   EXIT                                                             
         B     PS50                                                             
*                                  PUT ESTIMATE RETURN OBJECT TO OUTPUT         
PS37     GOTO1 PUTITEM,DMCB,ITESTRET,L'RETURN2,RETURN2                          
         BNE   EXIT                                                             
         B     PS50                                                             
*                                  PUT ESTIMATE RETURN OBJECT TO OUTPUT         
PS40     GOTO1 PUTITEM,DMCB,ITESTRET,L'RETURN,RETURN                            
         BNE   EXIT                                                             
*                                  PUT END OF DATA OBJECT TO OUTPUT             
PS50     GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT CURRENTLY DOES NOTHING.               
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
*        INVALID VALIDATE SCHEDULE REQUEST OBJECT                               
ERRVSRQ  MVC   APPLERR,=Y(ER0DVSRQ)                                             
         B     ERROBJ                                                           
*        INVALID AGENCY DATA REQUEST OBJECT                                     
ERRAGYQ  MVC   APPLERR,=Y(ER0DAGYQ)                                             
         B     ERROBJ                                                           
*        INVALID AGENCY                                                         
ERRAGY   MVC   APPLERR,=Y(ERA1AGY)                                              
         B     ERROBJ                                                           
*        NO CLIENT ACCESS                                                       
ERRNOCL  MVC   APPLERR,=Y(ER0DNOCL)                                             
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
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS,TWAACCS          OR HAVE LIMIT ACCESS                    
         JZ    YES                                                              
*                                                                               
         CLI   TWAACCS,C'+'        MARKET SECURITY                              
         JE    YES                 NO FURTHER TESTS!                            
         DROP  RE                                                               
*                                                                               
         L     R0,AIO                                                           
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ACOMFACS         INITIALIZE SECRET                            
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',AIO),0                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         MVC   OFCAUTH,TWAACCS     SET AUTH CODE PASSED TO US                   
         MVC   OFCLMT(4),TWAACCS   4 CHAR VERSION TOO                           
         CLI   TWAACCS+2,C'+'                                                   
         BNE   *+10                                                             
         XC    OFCLMT+2(2),OFCLMT+2    DON'T PASS MARKET LIMITS                 
         DROP  RE                                                               
         MVC   OFCAGY,SIGNON2C                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMED                                                 
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,AIO                                                      
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',BLOCK),ACOMFACS                                  
         CLI   0(R1),0                                                          
         JE    YES                                                              
         J     NO                                                               
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
* SPGENAGY                                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* SPGENEST                                                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
SVOFFC   DS    C                                                                
SVCACCS  DS    CL3                                                              
*                                                                               
REQUEST  DS    0CL9                REQUEST OBJECT                               
REQMED   DS    C                   MEDIA                                        
REQCLT   DS    CL3                 CLIENT                                       
REQPRD   DS    CL3                 PRODUCT                                      
REQEST   DS    CL2                 ESTIMATE                                     
*                                                                               
RETURN   DS    0CL100              RETURN OBJECT                                
RETSTART DS    CL6                 START DATE                                   
RETEND   DS    CL6                 END DATE                                     
RETBYEAR DS    CL2                 BOOK YEAR                                    
RETBMON  DS    CL2                 BOOK MONTH                                   
RETDEMO  DS    CL84                DEMOS                                        
*                                                                               
RETURN2  DS    0CL161              RETURN OBJECT                                
RETSTRT2 DS    CL6                 START DATE                                   
RETEND2  DS    CL6                 END DATE                                     
RETBYR2  DS    CL2                 BOOK YEAR                                    
RETBMON2 DS    CL2                 BOOK MONTH                                   
RETDEMO2 DS    CL84                DEMOS                                        
RETOWFLG DS    CL1                 OUT OF WEEK ROTATOR FLAG                     
RETCNAME DS    CL20                CLIENT LONG NAME                             
RETPNAME DS    CL20                PRD LONG NAME                                
RETENAME DS    CL20                EST LONG NAME                                
*                                                                               
         ORG   RETURN2                                                          
RETURN3  DS    0CL162              RETURN OBJECT                                
RTDATA3  DS    CL161               SAME AS RETURN 2                             
RETDWFLG DS    CL1                 D/W DAILY WEEKLY FLAG                        
*                                                                               
RETURN4  DS    0CL66               RETURN OBJECT                                
RETANAME DS    CL33                AGENCY NAME                                  
RETADDR  DS    CL33                AGENCY ADDRESS                               
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107CTMAD0D   07/01/02'                                      
         END                                                                    
