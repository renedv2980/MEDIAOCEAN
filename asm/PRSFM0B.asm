*          DATA SET PRSFM0B    AT LEVEL 022 AS OF 08/08/17                      
*PHASE T41C0BA                                                                  
*                                                                               
* KWAN   08/17 FIX MAX RECORD ERROR AND BUMP UP MAX TO 400                      
*                                                                               
* BPLA   03/15     NEW VALUE FOR REQUIRED FIELD                                 
*                  B= REQUIRED OR BILLING                                       
*                                                                               
* SMYE   11/03/00  ADDED CLIENT VALIDATION (SECURITY) TO DK                     
*                                                                               
* KWAN   10/99 ALLOW HEADLINES ON BILLS (H) ON SHOW ON BILLS? FIELD             
*                                                                               
         TITLE 'PRSFM0B USERFIELD DEFINITION RECDS MAINT'                       
*                                                                               
T41C0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C0B*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
* DISALLOW ACTION=ADD, DELETE, RESTORE                                          
* REMOVE WHEN SECURITY SYSTEM IS INSTALLED, I.E. LET SECURITY HANDLE IT         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTDEL                                                    
         BE    ACTERR                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    ACTERR                                                           
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY                                                           
*                                                                               
VK       LA    R2,SUSMEDH          MEDIA                                        
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         LA    R2,SUSCLTH          CLIENT                                       
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE RECORD                                                        
*                                                                               
VR       L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
         MVI   ELCODE,X'20'        FIND ELEMENT                                 
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING PCLTUDEF,R3                                                      
*                                                                               
         MVI   RECSW,C'P'          PROCESSING PRODUCT FIELDS                    
         LA    R2,SUSP1H           PRD DESC 1                                   
         MVC   TPLEN,MAXLFLD1                                                   
         BAS   RE,BLDBLK                                                        
         MVC   PCLTPU1(L'USERBLK),USERBLK                                       
*                                                                               
         MVI   RECSW,C'P'          PROCESSING PRODUCT FIELDS                    
         LA    R2,SUSP2H           PRD DESC 2                                   
         MVC   TPLEN,MAXLFLD2                                                   
         BAS   RE,BLDBLK                                                        
         MVC   PCLTPU2(L'USERBLK),USERBLK                                       
*                                                                               
         MVI   RECSW,C'E'          PROCESSING ESTIMATE FIELDS                   
         LA    R2,SUSE1H           EST DEC 1                                    
         MVC   TPLEN,MAXLFLD1                                                   
         BAS   RE,BLDBLK                                                        
         MVC   PCLTEU1(L'USERBLK),USERBLK                                       
*                                                                               
         MVI   RECSW,C'E'          PROCESSING ESTIMATE FIELDS                   
         LA    R2,SUSE2H           EST DESC 2                                   
         MVC   TPLEN,MAXLFLD2                                                   
         BAS   RE,BLDBLK                                                        
         MVC   PCLTEU2(L'USERBLK),USERBLK                                       
*                                                                               
         OC    ELEMENT,ELEMENT     WAS ANYTHING INPUT                           
         BZ    VRX                                                              
         MVC   ELEMENT(2),=X'2062'                                              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,SUSMEDH          CURSOR TO MEDIA                              
         CLC   PCLTLEN,=H'625'     MAX SUPPORTED REC SIZE IN PPNEWFILE?         
         BH    MAXERR                                                           
*                                                                               
VRX      B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*        BUILD DATA BLOCKS                                                      
*                                                                               
BLDBLK   NTR1                                                                   
*        INPUT R2    - DESCRIPTION FIELD                                        
*              TPLEN - LENGTH OF TYPE                                           
*              RECSW - PRD OR EST FIELDS                                        
*                                                                               
         XC    USERBLK,USERBLK                                                  
         XC    FLAG,FLAG                                                        
*                                                                               
         LR    R4,R2               SAVE DESC ADDRESS                            
         ZIC   R1,5(R2)            DESC FIELD                                   
         LTR   R1,R1                                                            
         BZ    BLD10                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   USRDESC(0),8(R2)    DESCRIPTION                                  
         OI    FLAG,DESCQ          DESCRIPTION GIVEN                            
*                                                                               
BLD10    BAS   RE,BUMP                                                          
         BAS   RE,BUMP             BUMP TO REQUIRED FIELD                       
         CLI   5(R2),0             REQUIRED FIELD                               
         BE    BLD30                                                            
         TM    FLAG,DESCQ          DESCRIPTION GIVEN                            
         BNO   MISSERR                                                          
         CLI   RECSW,C'E'          DOING EST FIELDS?                            
         BE    BLD15                                                            
BLD10X   BAS   RE,TESTYN           ONLY ALLOW Y/N                               
         BNE   BLD30                                                            
         OI    USRFLAG,X'80'       YES, REQUIRED INDICATED                      
         B     BLD30                                                            
*                                                                               
BLD15    CLI   8(R2),C'B'                                                       
         BNE   BLD10X                                                           
         OI    USRFLAG,X'01'   YES, REQUIRED  FOR BILL INDICATED                
         B     BLD30                                                            
*                                                                               
BLD30    BAS   RE,BUMP                                                          
         BAS   RE,BUMP             EDIT RULE FIELD                              
         CLI   5(R2),0                                                          
         BE    BLD50                                                            
         TM    FLAG,DESCQ          DESCRIPTION GIVEN                            
         BNO   MISSERR                                                          
         CLI   8(R2),C'D'          CHECK TYPE - DATE                            
         BE    BLD40                                                            
         CLI   8(R2),C'C'                                                       
         BE    BLD40                                                            
         CLI   8(R2),C'N'                                                       
         BNE   INVERR                                                           
*                                                                               
BLD40    MVC   USRTYPE,8(R2)                                                    
*                                                                               
BLD50    BAS   RE,BUMP                                                          
         BAS   RE,BUMP             EDIT RULE FIELD                              
         TM    FLAG,DESCQ          IF DESCRIPTION INPUT                         
         BO    BLD60               CONTINUE                                     
         CLI   5(R2),0             IF THERE IS LEN INPUT                        
         BE    BLD70                                                            
         B     MISSERR             MUST HAVE DESCRIPTION                        
*                                                                               
BLD60    GOTO1 ANY                 MUST HAVE LENGTH                             
         ZIC   R1,5(R2)            L'INPUT                                      
         BCTR  R1,0                                                             
         MVC   WORK(2),=2X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)       CHECKING FOR VALID NUMERICS                  
         CLC   WORK(2),=2X'F0'                                                  
         BNE   NUMBERR                                                          
*                                                                               
         EX    R1,*+8              CONVERT INPUT LENGTH TO BINARY               
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0               CHECK IF INPUT = 0                           
         BZ    INVERR                                                           
         CH    R0,TPLEN            CHECK IF INPUT EXCEEDS MAXIMUM               
         BH    INVERR              ALLOWED                                      
         STC   R0,USRLEN           SAVE L'TYPE                                  
*                                                                               
BLD70    BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             SHOW ON BILLS                                
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLD90                                                            
         TM    FLAG,DESCQ          YES - THERE MUST BE A DESCRIPTION            
         BNO   MISSERR                                                          
*                                                                               
         CLI   8(R2),C'F'          FRONT OF BILL?                               
         BNE   BLD70H                                                           
         OI    USRFLAG,X'12'       ON BILLS AND FRONT OF BILL                   
         B     BLD90                                                            
*                                                                               
BLD70H   CLI   8(R2),C'H'          HEADLINES ON BILL?                           
         BNE   BLD70K                                                           
         OI    USRFLAG,X'14'       ON BILLS AND HEADLINES ON BILL               
         B     BLD90                                                            
*                                                                               
BLD70K   DS    0H                  FOR FUTURE USE                               
*                                                                               
BLD70Y   BAS   RE,TESTYN           Y/N                                          
         BNE   *+8                                                              
         OI    USRFLAG,X'10'                                                    
*                                                                               
BLD90    BAS   RE,BUMP                                                          
         BAS   RE,BUMP             52 FIELD                                     
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLD110                                                           
         TM    FLAG,DESCQ          THERE MUST BE A DESCRIPTION                  
         BNO   MISSERR                                                          
         BAS   RE,TESTYN           Y/N                                          
         BNE   *+8                                                              
         OI    USRFLAG,X'40'       SHOW 52.                                     
*                                                                               
BLD110   BAS   RE,BUMP                                                          
         BAS   RE,BUMP             EC FIELD                                     
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLD120                                                           
         TM    FLAG,DESCQ          THERE MUST BE A DESCRIPTION                  
         BNO   MISSERR                                                          
         BAS   RE,TESTYN           Y/N                                          
         BNE   *+8                                                              
         OI    USRFLAG,X'20'       SHOW EC                                      
*                                                                               
BLD120   BAS   RE,BUMP                                                          
         BAS   RE,BUMP             MBI FIELD                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLDX                                                             
         TM    FLAG,DESCQ          THERE MUST BE A DESCRIPTION                  
         BNO   MISSERR                                                          
         BAS   RE,TESTYN           Y/N                                          
         BNE   *+8                                                              
         OI    USRFLAG,X'08'       SHOW MBI                                     
*                                                                               
BLDX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DISPLAY RECORD                                                         
*                                                                               
DR       DS    0H                                                               
         TWAXC SUSP1H                                                           
*                                                                               
         CLI   ACTNUM,ACTSEL      TEST IF FROM LIST SCREEN                      
         BNE   DR10                                                             
         LA    R3,LISTDIR          LIST SCREEN INFO                             
         CLI   SELLISTN,0          SEE IF FIRST                                 
         BE    DR3                                                              
         ZIC   R0,SELLISTN         RELATIVE LINE NUMBER                         
         LA    R3,6(R3)            NEXT LIST ENTRY                              
         BCT   R0,*-4                                                           
*                                                                               
DR3      CLI   0(R3),C'D'          SEE IF TRYING TO DELETE FROM LIST            
         BNE   DR10                                                             
         B     ACTERR                                                           
*                                                                               
DR10     DS    0H                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,X'20'        UDEF ELEMENT                                 
         USING PCLTUDEF,R4                                                      
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         MVC   USERBLK,PCLTPU1                                                  
         LA    R2,SUSP1H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         MVC   USERBLK,PCLTPU2                                                  
         LA    R2,SUSP2H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         MVC   USERBLK,PCLTEU1                                                  
         LA    R2,SUSE1H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
         MVC   USERBLK,PCLTEU2                                                  
         LA    R2,SUSE2H                                                        
         BAS   RE,DISBLK                                                        
*                                                                               
DRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        DISPLAY DATA BLOCK                                                     
*        R2      - DESCRIPTION FIELD-HEADER                                     
*        USERBLK - DATA                                                         
*                                                                               
DISBLK   NTR1                                                                   
         OC    USRDESC,USRDESC     CHECK IF THERE IS ANY DATA                   
         BZ    DBX                                                              
         MVC   8(L'USRDESC,R2),USRDESC                                          
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             REQUIRED FIELD                               
*                                                                               
         MVI   8(R2),C'N'          ASSUME NO                                    
         TM    USRFLAG,X'80'       IF "REQUIRED" INDICATED                      
         BZ    *+8                                                              
         MVI   8(R2),C'Y'          YES                                          
         TM    USRFLAG,X'01'       REQUIRED FOR BILLING                         
         BZ    *+8                 (EST ONLY FOR NOW)                           
         MVI   8(R2),C'B'          YES                                          
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             TYPE/LENGTH FIELD                            
         MVC   8(1,R2),USRTYPE     USER TYPE (A/C/N).                           
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             TYPE/LENGTH FIELD                            
         EDIT  USRLEN,(2,8(R2)),FILL=0                                          
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             SHOW FIELD                                   
         MVI   8(R2),C'N'          ASSUME NO                                    
         TM    USRFLAG,X'10'       SHOW ON BILLS                                
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
         TM    USRFLAG,X'02'       SHOW ON BILLS- FRONT (X'12')                 
         BZ    *+8                                                              
         MVI   8(R2),C'F'                                                       
         TM    USRFLAG,X'04'       SHOW ON BILLS- HEADLINES (X'14')             
         BZ    *+8                                                              
         MVI   8(R2),C'H'                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             52 FIELD                                     
         MVI   8(R2),C'N'          ASSUME NO                                    
         TM    USRFLAG,X'40'       SHOW 52                                      
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             EC FIELD                                     
         MVI   8(R2),C'N'          ASSUME NO                                    
         TM    USRFLAG,X'20'       SHOW EC                                      
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             MBI FIELD                                    
         MVI   8(R2),C'N'          ASSUME NO                                    
         TM    USRFLAG,X'08'       SHOW MBI                                     
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
DBX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
DK       L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
*                                                                               
         MVC   SUSMED,PCLTKMED     MEDIA                                        
         OI    SUSMEDH+6,X'80'                                                  
         MVC   SUSCLT,PCLTKCLT                                                  
         OI    SUSCLTH+6,X'80'                                                  
*                                                                               
         MVC   MYKEYSV(L'PCLTKEY),PCLTKEY                                       
*                                                                               
         LA    R2,SUSMEDH                                                       
         MVI   5(R2),1                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,SUSCLTH                                                       
         MVI   5(R2),3                                                          
         GOTO1 VALICLT                                                          
*                                  RESTORE KEY AND RECORD                       
         MVC   KEY(L'PCLTKEY),MYKEYSV                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'PCLTKEY),MYKEYSV     SAME RECORD ?                         
         BE    *+6                 YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
         GOTO1 GETREC                                                           
*                                                                               
*                                                                               
XDK      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
MYKEYSV  DS    CL25                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
TESTYN   NTR1                                                                   
         CLI   8(R2),C'N'          Y/N                                          
         BE    NO                                                               
         CLI   8(R2),C'Y'                                                       
         BE    YES                                                              
         B     INVERR                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         LR    R2,R4                                                            
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
NUMBERR  MVI   ERROR,NOTNUM                                                     
         B     ERRXIT                                                           
*                                                                               
ACTERR   MVI   ERROR,INVACT                                                     
         B     ERRXIT                                                           
*                                                                               
MAXERR   MVI   ERROR,MAXSERR                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         DS    0F                                                               
MAXLFLD1 DC    H'32'               MAXIMUM LENGTH OF PROD/EST 1                 
MAXLFLD2 DC    H'16'               MAXIMUM LENGTH OF PROD/EST 2                 
         SPACE 4                                                                
MAXSERR  EQU   090                 MAXIMUM RECORD SIZE                          
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMFBD                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
*                                                                               
TPLEN    DS    H                   MAXIMUM LENGTH OF DESCRIPTION                
RECSW    DS    C                                                                
FLAG     DS    C                                                                
DESCQ    EQU   X'80'               DESCRIPTION GIVEN                            
*                                                                               
*                                                                               
USERBLK  DS    0CL24                                                            
USRDESC  DS    CL20                USER FIELD DESRIPTION                        
USRTYPE  DS    CL1                 FIELD TYPE                                   
USRLEN   DS    XL1                 FIELD LENGTH                                 
USRFLAG  DS    XL1                 USER FLAG                                    
USRFLG2  DS    XL1                 USER FLAG # 2                                
USERBLKQ EQU   *-USERBLK           SHOULD = CUSERLNQ.                           
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
*                                                                               
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022PRSFM0B   08/08/17'                                      
         END                                                                    
