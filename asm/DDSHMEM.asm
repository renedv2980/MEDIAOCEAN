*          DATA SET DDSHMEM    AT LEVEL 007 AS OF 02/22/19                      
*PHASE SHMEMA                                                                   
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE DMDMGRL                                                                
         TITLE 'Mediaocean interface for UNIX SHARED MEMORY'                    
*============================================================                   
* DSPACE=                                                                       
* PROC     CREATES SHRMEM fOR STAPACK, RCVB, WRKF, PRTQ                         
* REMOVE   -- SAME AS ABOVE --                                                  
* USER MUST SPECIFY RESOURCE NAME                                               
*============================================================                   
                                                                                
SHMCTL   CSECT                                                                  
         NBASE 0,**SHMEM*,=V(REGSAVE)                                           
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
         USING COMMON,RC                                                        
         L     RC,=A(COMMON)                                                    
*                                                                               
CARD010  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END OF INPUT CARDS?                          
         BE    CARDX                                                            
*                                                                               
         USING SSBD,RF                                                          
         CLC   =C'DSPACE=',CARD                                                 
         BNE   CARD020                                                          
         L     RF,=A(SSB)          Set DSPACE in SSB                            
         MVC   SSODSPAC,CARD+7                                                  
         MVC   DSPACE,CARD+7                                                    
         B     CARD010                                                          
         DROP  RF                                                               
*                                                                               
CARD020  CLC   =C'DDSIO=',CARD                                                  
         BNE   CARD030                                                          
         ICM   RF,15,=V(DDSIO)                                                  
         MVC   0(8,RF),CARD+6                                                   
         B     CARD010                                                          
*                                                                               
CARD030  MVC   ACTION,#CREATE                                                   
         MVI   ACTIND,SHMCRE       Action create                                
         MVC   MEMNAME,CARD+7                                                   
         CLC   =C'CREATE=',CARD                                                 
         BE    CARD040                                                          
*                                                                               
         MVC   ACTION,#ATTACH                                                   
         MVI   ACTIND,SHMATC       Action attach                                
         CLC   =C'ATTACH=',CARD                                                 
         BE    CARD040                                                          
*                                                                               
         MVC   ACTION,#REMOVE                                                   
         MVI   ACTIND,SHMREM       Action remove                                
         CLC   =C'REMOVE=',CARD                                                 
         BE    CARD040                                                          
*                                                                               
         MVC   ACTION,#RESET                                                    
         MVI   ACTIND,SHMRES       Action reset                                 
         CLC   =C'RESET=',CARD                                                  
         JNE   *+2                 No such action                               
         MVC   MEMNAME,CARD+6                                                   
*                                                                               
         USING SHMD,R4                                                          
CARD040  LA    R4,SHRLIST                                                       
CARD042  CLI   0(R4),EOT           End of table?                                
         JE    *+2                 No such combination                          
         CLC   MEMNAME,SHMNAME                                                  
         JNE   CARD048                                                          
         CLC   ACTIND,SHMACT                                                    
         JE    CARD050                                                          
CARD048  AHI   R4,SHMLNQ                                                        
         J     CARD042                                                          
*                                                                               
         USING SSBD,RF                                                          
CARD050  DS    0H                                                               
         L     RF,=A(SSB)                                                       
         CLI   SSODSPAC,C' '                                                    
         JNH   *+2                 Must be set                                  
         MVC   MSG1DSP,SSODSPAC                                                 
         MVC   MSG1ACT,ACTION      MOVE ACTION NAME                             
         MVC   MSG1SHM,MEMNAME     MOVE MEMORY NAME                             
         DROP  RF                                                               
*                                                                               
         LT    RF,SHMRTN                                                        
         JZ    *+2                 Hugh?                                        
         BASR  RE,RF                                                            
         DROP  R4                                                               
*                                                                               
CARDX    DS    0H                                                               
         XBASE ,                                                                
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
CREATE   DS    0H                                                               
ATTACH   DS    0H                                                               
REMOVE   DS    0H                                                               
         XC    P1(24),P1                                                        
         LA    RE,ACTION                                                        
         ST    RE,P1                                                            
*                                                                               
         LA    RE,MEMNAME          POINT TO SHMEM NAME                          
         ST    RE,P2                                                            
         GOTO1 =V(DMSHMUSS),P1                                                  
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG1LEN',MSG1)                         
         B     CARDX                                                            
         EJECT ,                                                                
***********************************************************************         
* Special command for STAPACK. Rebuild table                                    
***********************************************************************         
RESETSP  DS    0H                                                               
*                                  FOR NOW, ONLY RESET STAPACK TABLE            
         MVC   DUB(8),=CL8'T00A7A' LOAD STAPACK                                 
         LOAD  EPLOC=DUB,ERRET=RESETER                                          
         LR    RF,R0                                                            
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'Z'                                                     
         MVC   STAPACOM,=A(COMFACS)                                             
         MVC   STAPAGY,=X'FFFF'                                                 
         DROP  R1                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG1LEN',MSG1)                         
         B     CARDX                                                            
*                                                                               
RESETER  DC    H'0'                LOAD STAPACK ERROR                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* Reset for WRKF shared memory file index table                                 
***********************************************************************         
RESETNDX DS    0H                                                               
*                                                                               
         GOTO1 =V(DMSHMUSS),P1,#ATTACH,MEMNAME,0,0                              
         ICM   R2,15,P3                                                         
         BZ    REWF020                                                          
                                                                                
         SAM31                                                                  
         USING SIHDRD,R2                                                        
         OI    SIHDR,X'80'                                                      
         XC    SIHEYE,SIHEYE                                                    
*                                                                               
         LOCASCB STOKEN=SIHMSTOK   LOCATE ASCB FROM JOB STOKEN                  
         LTR   RF,RF                                                            
         BNZ   REWF030                                                          
*                                                                               
         LR    R4,R1                                                            
         USING ASCB,R4                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   REWF030                                                          
*                                                                               
         L     R3,SIHMECB                                                       
         POST  (R3),99,ASCB=(R4),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTPA)          
         LTR   RF,RF                                                            
         BNZ   REWF030                                                          
*                                                                               
         SAM24                                                                  
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG1LEN',MSG1)                         
         B     CARDX                                                            
*                                                                               
REWF020  MVC   MSG1SYS,=CL7'RESET'                                              
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG1XLN',MSG1)                         
         B     CARDX                                                            
*                                                                               
REWF030  SAM24                                                                  
         MVC   MSG1SYS,=CL7'POST'                                               
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG2XLN',MSG1)                         
         B     CARDX                                                            
         DROP  R2                                                               
*                                                                               
POSTPA   POST   ECBKEY=YES,MF=L                                                 
         EJECT                                                                  
                                                                                
*====================================================================           
* RCVB CREATE CALL TO SHMUSS GETS A BUFFER FOR EVERY SYSTEM THAT                
* HAS A RECOVERY FILE WITH SHRMEM=Y                                             
* NOTE HAVE TO PASS A(SELIST) TO SHMUSS VIA P4                                  
* ON RETURN, P7 HAS A LIST OF TWO BYTE ENTRIES                                  
* SE NUMBER AND RETURN CODE X'00'=OK, X'FF'=ERROR                               
*====================================================================           
                                                                                
*====================================================================           
* REMEMBER THAT PARM NUMBERS TO SHMUSS ARE 1 DIFFERENT BECAUSE                  
* P1 IS SHMUSS CALL TO DATAMGR                                                  
*====================================================================           
                                                                                
RCVBCR   NTR1                                                                   
         XC    DMCB(24),DMCB      SYSFLES                                       
         MVI   DMCB+11,1          Get Service first system                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'SYSFLES'                          
         LT    R2,12(,R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SYSFLSTD,R2                                                      
RCVBCR02 CLI   SYSFSYS#,FF         End of table?                                
         BE    RCVBCR90            Done                                         
         MVC   SE#,SYSFSYS#        Save off SE#                                 
*                                                                               
         LH    R0,SYSF#FLS         GET NUMBER OF FILES THIS SYSTEM              
         LR    R3,R0                                                            
         MHI   R3,SYSFLNQ                                                       
         LA    R2,SYSFLIST         POINT TO FIRST FILE                          
         AR    R3,R2               R3 = Next set of files                       
*                                                                               
RCVBCR04 TM    SYSFIND1,SFRCV      RECOVERY FILE?                               
         BO    RCVBCR10            Yes                                          
         LA    R2,SYSFLNQ(,R2)                                                  
         BCT   R0,RCVBCR04                                                      
         B     RCVBCR02            None for system                              
*                                                                               
         USING DTFPHD,R4                                                        
RCVBCR10 XR    R4,R4                                                            
         ICM   R4,7,SYSFADTF       A(recovery file DTF)                         
         JZ    *+2                 Fail                                         
         TM    DTFFLAG,DTFSHMEM    Shared memory recovery?                      
         BO    RCVBCR12            Yes                                          
         LR    R2,R3               No, next list                                
         B     RCVBCR02                                                         
         DROP  R4                                                               
*                                                                               
RCVBCR12 XR    R1,R1                                                            
         LLC   R1,SE#                                                           
         ST    R1,P4               P4 TO SHMUSS                                 
*                                                                               
         L     R1,=V(SELIST)                                                    
         LH    RE,0(,R1)                                                        
         L     RF,2(,R1)                                                        
         LA    R1,6(,R1)                                                        
*                                                                               
         USING SELISTD,R1                                                       
RCVBCR16 CLC   SESYS,SE#           Find system SE                               
         BE    RCVBCR20                                                         
         BXLE  R1,RE,RCVBCR16                                                   
         DC    H'0'                                                             
*                                                                               
RCVBCR20 MVC   MSG1SYS,SENAME                                                   
         DROP  R1                                                               
*                                                                               
         L     R6,P4               Keep P4 since P3-P6 are destroied            
         GOTO1 =V(DMSHMUSS),P1,#CREATE,MEMNAME                                  
         JL    RCVERR                                                           
         GOTO1 =V(DMSHMUSS),P1,#ATTACH,MEMNAME,0,(R6)                           
         JL    RCVERR                                                           
         SAM31                                                                  
         L     RF,P5                   A(Start of header)                       
         MVC   0(8,RF),=C'**RCVB**'    SET EYECATCHER                           
         MVC   8(7,RF),MSG1SYS                                                  
         MVI   15(RF),C'*'                                                      
         SAM24                                                                  
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG1XLN',MSG1)                         
         J     RCVBCR30                                                         
*                                                                               
RCVERR   GOTO1 CDATAMGR,DMCB,=C'OPMSG',('MSG2XLN',MSG1)                         
*                                                                               
RCVBCR30 LR    R2,R3                                                            
         B     RCVBCR02                                                         
*                                                                               
RCVBCR90 B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* COMMONLY ADDRESSABLE STORAGE                                        *         
***********************************************************************         
         LTORG                                                                  
         ORG   SHMCTL+(((*-SHMCTL)+4095)/4096*4096)                             
COMMON   DS    0X                                                               
***********************************************************************         
* EXIT POINTS AND USEFUL ROUTINES                                     *         
***********************************************************************         
EXITL    CLI   *,255               SET CC LOW                                   
         J     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         J     EXIT                                                             
*                                                                               
EXITOK   SR    RF,RF               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* Constants and working storage                                       *         
***********************************************************************         
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FF       EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
         DS    0D                                                               
*                                                                               
#CREATE  DC    CL8'CREATE'                                                      
#ATTACH  DC    CL8'ATTACH'                                                      
#REMOVE  DC    CL8'REMOVE'                                                      
#RESET   DC    CL8'RESET '                                                      
*                                                                               
SHRLIST  DS    0D                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'STAPACK',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'STAPACK',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'STAPACK',A(REMOVE)                         
         DC    AL1(SHMRES,0,0,0),CL8'STAPACK',A(RESETSP)                        
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'CBLLST ',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'CBLLST ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'CBLLST ',A(REMOVE)                         
*        DC    AL1(SHMRES,0,0,0),CL8'CBLLST ',A(RESETCL)                        
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'RCVB   ',A(RCVBCR)                         
         DC    AL1(SHMATC,0,0,0),CL8'RCVB   ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'RCVB   ',A(REMOVE)                         
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'WRKF   ',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'WRKF   ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'WRKF   ',A(REMOVE)                         
         DC    AL1(SHMRES,0,0,0),CL8'WRKF   ',A(RESETNDX)                       
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'PRTQ   ',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'PRTQ   ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'PRTQ   ',A(REMOVE)                         
         DC    AL1(SHMRES,0,0,0),CL8'PRTQ   ',A(RESETNDX)                       
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'TORS   ',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'TORS   ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'TORS   ',A(REMOVE)                         
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'UTLS   ',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'UTLS   ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'UTLS   ',A(REMOVE)                         
*                                                                               
         DC    AL1(SHMCRE,0,0,0),CL8'SELIST ',A(CREATE)                         
         DC    AL1(SHMATC,0,0,0),CL8'SELIST ',A(ATTACH)                         
         DC    AL1(SHMREM,0,0,0),CL8'SELIST ',A(REMOVE)                         
         DC    AL1(EOT)                                                         
*                                                                               
CARD     DC    CL80' '                                                          
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
P7       DS    F                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
*                                                                               
ACTION   DC    CL8' '                                                           
MEMNAME  DS    CL8                                                              
ACTIND   DS    C                                                                
*                                                                               
WAITTIME DC    F'1000'                                                          
SE#      DS    X                                                                
*                                                                               
DSPACE   DC    C' '                                                             
*                                                                               
STAWORK  DS    XL32                STAPACK INTERFACE AREA                       
*&&US                                                                           
MSG1     DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
*&&                                                                             
*&&UK                                                                           
MSG1     DC    C'AUTONOTE*EU-FAC-TEAM:'                                         
*&&                                                                             
MSG1ACT  DC    CL8'ACTION  '                                                    
MSG1SHM  DC    CL8'SHRMEM  '                                                    
         DC    C'FOR DSPACE='                                                   
MSG1DSP  DS    C                                                                
MSG1LEN  EQU   *-MSG1                                                           
         DC    C' '                                                             
MSG1SYS  DC    CL7'SYSTEM'                                                      
MSG1XLN  EQU   *-MSG1                                                           
         DC    C' '                                                             
MSG1FAIL DC    CL6'FAILED'                                                      
MSG2XLN  EQU   *-MSG1                                                           
*                                                                               
MSGRS    DC    C'AUTONOTE*US-MF_FAC_NOTIFY:SHM IS RESET FOR DSPACE='            
MSGRSA   DS    C                                                                
MSGRSQ   EQU   *-MSGRS                                                          
*                                                                               
COMFACS  DS    0F                                                               
       ++INCLUDE DDCOMFACSC                                                     
                                                                                
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
*                                                                               
UTL      DC    F'0',X'00'          FOR DATAMGR (MUST SPECIFY SYSTEM)            
                                                                                
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
SHMD     DSECT                                                                  
SHMACT   DS    AL1                 Action                                       
SHMCRE   EQU   C'C'                .  Create                                    
SHMATC   EQU   C'A'                .  Attach                                    
SHMREM   EQU   C'D'                .  Remove/Delete                             
SHMRES   EQU   C'R'                .  Reset                                     
         DS    AL3                 spare                                        
SHMNAME  DS    CL8                 Memory name                                  
SHMRTN   DS    A                   Routine to use                               
SHMLNQ   EQU   *-SHMD                                                           
*                                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSYSFD                                                        
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* SPSTAPACKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
* SHFID                                                                         
         PRINT OFF                                                              
       ++INCLUDE SHFID                                                          
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
         PRINT ON                                                               
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDSHMEM   02/22/19'                                      
         END                                                                    
