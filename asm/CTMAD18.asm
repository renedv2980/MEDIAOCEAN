*          DATA SET CTMAD18    AT LEVEL 117 AS OF 04/13/09                      
*PHASE TA0C18B                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'CTMAD18 - $MAD UPLOAD SPOT BUYS'                                
TA0C18   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,TA0C18,RA,R8,RR=R2                                   
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
*                                                                               
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMORY)               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         ST    RF,AWORKSTR         SAVE A(UNSAVED WORKING STORAGE)              
*                                                                               
         ST    R2,APRELO                                                        
         ST    RB,APBASE1          SAVE PROGRAM BASE REGISTER 1                 
         ST    RA,APBASE2          SAVE PROGRAM BASE REGISTER 2                 
         EJECT                                                                  
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
MAIN     BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   *+12                                                             
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
         CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   *+12                                                             
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
         BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FIELDS                                                             
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
         L     RE,AWORKSTR         SAVE A(UNSAVED WORKING STORAGE)              
         USING WORKD,RE                                                         
         LA    RF,MINBLK           MINIO PARAMETER BLOCK                        
         ST    RF,AMINBLK                                                       
*                                                                               
         LR    RF,RE                                                            
         AH    RF,=Y(MINBUFFS-WORKD)                                            
         ST    RF,AMINBUFF         MINIO BUFFERS                                
*                                                                               
         LA    RF,MINRECTB         MINIO RECORD TABLE                           
         ST    RF,AMINRECT                                                      
         DROP  RE                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   GLOBBER,CGLOBBER    A(GLOBBER)                                   
         MVC   MINIO,CMINIO        A(MINIO)                                     
         DROP  RE                                                               
*                                                                               
         L     RE,=V(RECUP)                                                     
         AR    RE,R2                                                            
         ST    RE,RECUP            A(RECUP)                                     
         L     RE,=V(MEDGET)                                                    
         AR    RE,R2                                                            
         ST    RE,MEDGET           A(MEDGET)                                    
*                                                                               
         GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0                                                         
         BE    *+14                                                             
         MVC   MDACTION,=Y(ERSWITCH)  ERROR SWITCHING TO SPOT SYSTEM            
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         BAS   RE,INITMIN          INITIALIZE MINIO                             
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4,=X'D9000A2B'    SPGETBUY FOR 2 BYTE BUYLINES              
         GOTO1 CALLOV,DMCB                                                      
         MVC   AGETBUY,DMCB                                                     
*                                                                               
         BRAS  RE,GBUYINIT         INITIALIZE GETBUY                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE MINIO                                                              
***********************************************************************         
INITMIN  NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LR    RE,R5               CLEAR MINIO PARAMETER BLOCK                  
         LH    RF,=Y(MINBLKL)                                                   
         XCEFL                                                                  
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=C'SPTFIL  ' FILE NAME                                    
         MVC   MINDIR,=C'SPTDIR  ' DIR NAME                                     
         MVI   MINFKLEN,L'SPUPKEY  KEY LENGTH                                   
         MVI   MINNCTL,1           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=Y(LENMINRC)  MAXIMUM RECORD LENGTH                     
         MVI   MINEKLEN,L'SPUPEKEY ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,SPUPEKEY-SPUPREC  DISPLACEMENT TO ELEMENT KEY           
         MVC   MINBUFF,AMINBUFF    A(FIRST BUFFER)                              
         MVC   MINRTAB,AMINRECT    A(AREA FOR RECORD TABLE)                     
         LA    RF,ELEMENT                                                       
         ST    RF,MINELEM          A(AREA FOR MINIO ELEMENT)                    
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   MINRECUP,RECUP      A(RECUP)                                     
         MVC   MINRTABL,=Y(MINRCTBL) LENGTH OF RECORD TABLE                     
         MVC   MINSPCT,=AL2(90)    USE 90/10 SPLIT INSTEAD OF 50/50             
         MVC   MINMAXEL,=H'250'    MAX LENGTH OF ELEMENT                        
         MVI   MINNBUF,NUMMNBUF    NUMBER OF AVAILABLE BUFFERS                  
         MVC   MINWRITE,SVWRTFLG   'N' = DON'T WRITE TO FILE                    
         LA    R3,MINMKEY          SET MASTER KEY                               
         USING SPUPKEY,R3                                                       
         MVC   SPUPTYPE,=X'0D78'   RECORD TYPE                                  
         MVC   SPUPAM,SVBAM        AGENCY/MEDIA                                 
         MVC   SPUPCLT,SVBCLT      CLIENT                                       
         MVC   SPUPPRD,SVBPRD      PRODUCT                                      
         MVC   SPUPEST,SVEST+1     ESTIMATE                                     
         MVC   SPUPAGY,SVAGY       AGENCY                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINETOVF                                                  
         BNE   INITMX                                                           
         MVI   ERFLDNUM,0          FIELD NOT IN ERR BECAUSE IT'S MINIO          
         MVI   ERRNUM,0            SET MINIO ERROR NUMBER IN ERRNUM             
         MVC   ERRNUM+1(L'MINERR),MINERR                                        
         XC    SUBNUM,SUBNUM       SUB RECORD NUMBER                            
         BAS   RE,SETERR           SET ERROR                                    
         B     EXIT                LEAVE CTMAD18                                
*                                                                               
INITMX   B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT GETS THE FILE HEADER               
* OBJECT, INITIALIZES MINIO, THEN EXITS AND WAITS FOR BUYS.                     
***********************************************************************         
PROCSTRT NTR1                                                                   
*                                                                               
         CLI   SENTHDR,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         MVI   MDLAST,C'N'         NOT YET LAST OUTPUT DATA FRAME               
         MVI   CONFFLAG,0          HAVEN'T CONFIRM YET                          
         MVI   MISCFLG1,0          CLEAR MISCELLANEOUS FLAG 1                   
         MVI   ENDOBJCT,C'N'       NOT END OF OBJECTS                           
         XC    BUYNUM,BUYNUM       NO BUYS YET                                  
*****    XC    LASTSTA,LASTSTA     NO STATIONS YET                              
         XC    TOTALDEL,TOTALDEL   NO BUYS DELETED YET                          
*                                                                               
         BAS   RE,GETDATA          GET DATA                                     
*                                                                               
         CLC   TYPENUM,=A(ITSBYHDR) THIS MUST BE FILE HEADER OBJECT             
         BE    PS30                                                             
         MVC   MDACTION,=Y(ER11OBCD)  INVALID OBJECT CODE                       
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
PS30     BAS   RE,SETSAVE          SET SAVED DATA                               
*                                                                               
         BAS   RE,VALHEADR         VALIDATE HEADER RECORD FIELDS                
*                                                                               
         CLI   ERFLDNUM,0          ANY ERROR ON HEADER?                         
         BE    PS40                                                             
         BAS   RE,HDRERR           HEADER RECORD IN ERROR                       
         B     PSX                                                              
*                                                                               
PS40     MVI   SEQNUM,0            NEW EST SO RESET SEQUENCE NUMBER             
         BAS   RE,UPDACTV          UPDATE MINIO ACTIVITY ELEMENT                
*                                                                               
         BAS   RE,CLEANUP          CLEAN UP REC IF BAD RETURN FROM $BUY         
*                                                                               
         MVI   SENTHDR,C'N'                                                     
         LA    RE,SVHDROBJ         BUILD HEADER OBJECT HERE                     
         USING SHDRODAT,RE                                                      
         L     RF,ADATA            OBJECT AS GIVEN TO ME                        
         XC    SHDRODAT(SHD2LENQ),SHDRODAT                                      
         MVC   SHDRTYPE,=C'HDR*'                                                
         MVC   SHDRSTRT(SHD2RLNQ),0(RF)   OBJECT DATA                           
*** SBY2                                                                        
         CLC   SHDRUTYP,=C'BY2'    IS THIS A '20 DEMO' UPLOAD?                  
         BNE   *+8                                                              
         OI    MISCFLG1,MF1SBY2     - YUP IT IS, TURN ON THE FLAG               
*** SBY2                                     MHC  01/16/03                      
         DROP  RE                                                               
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'PTX'),,(3,0)                               
         BNE   EXIT                                                             
*                                                                               
         MVI   TMPOPEND,C'Y'       TEMPSTR IS OPEN                              
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   *+12                 - NO, REGULAR                               
         LH    R3,=Y(SHD2LENQ)     LONGER LENGTH (BY2)                          
         B     *+8                                                              
*** SBY2                                     MHC  01/16/03                      
         LH    R3,=Y(SHDRLENQ)     WRITE FILE HEADER OBJECT TO TEMPSTR          
*                                                                               
         GOTO1 PUTTMP,DMCB,SVHDROBJ,(R3)                                        
         BNE   EXIT                                                             
         MVI   SENTHDR,C'Y'                                                     
         GOTO1 TMPCLOSE            PUT HEADER TO TEMPFILE                       
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         BAS   RE,CALLBUY                                                       
*                                                                               
         CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BE    PSX                                                              
         BAS   RE,PROCMID          YES                                          
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET SAVED STORAGE FIELDS                                                      
***********************************************************************         
SETSAVE  NTR1                                                                   
         L     R2,ADATA            OBJECT AS GIVEN TO ME                        
         USING UHDRD,R2                                                         
         MVC   SVSYS,UHDRSYS       SYSTEM                                       
         MVC   SVTYPE,UHDRUTYP     UPLOAD TYPE EITHER BUY OR BY2                
         MVC   SVAGY,UHDRAGID      BUYING AGENCY ID                             
         MVC   SVMED,UHDRMED       MEDIA CODE                                   
         MVC   SVCLT,UHDRCLT       CLIENT CODE                                  
         MVC   SVPRD,UHDRPRD       PRODUCT CODE                                 
         MVC   SVPRD2,UHDRPRD2     PRODUCT 2 CODE                               
*** SBY2                                                                        
         CLC   UHDRUTYP,=C'BY2'    IS IT BY2 (20 DEMOS)?                        
         BNE   SSV05                - NO, REGULAR                               
         OI    MISCFLG1,MF1SBY2     - YUP IT IS, TURN ON THE FLAG               
         MVC   SVLOCID,UHD2UID2    LOCATION ID - 1ST 2 CHARS-UNIQUE ID          
         MVC   SVHNOSP,UHD2NOSP        (SBY2)                                   
         B     SSV07                                                            
*** SBY2                                      MHC  01/16/03                     
*                                                                               
SSV05    MVC   SVLOCID,UHDRUID2    LOCATION ID - 1ST 2 CHARS-UNIQUE ID          
         MVC   SVHNOSP,UHDRNOSP                                                 
*                                                                               
SSV07    SR    R1,R1               NO ESTIMATE GIVEN -- ERROR                   
         CLC   UHDREST,SPACES                                                   
         BE    SSV20               NO ESTIMATE GIVEN -- ERROR                   
         CLI   UHDREST+2,C' '                                                   
         BE    *+14                                                             
         PACK  DUB,UHDREST(3)                                                   
         B     SSV10                                                            
         CLI   UHDREST+1,C' '                                                   
         BE    *+14                                                             
         PACK  DUB,UHDREST(2)                                                   
         B     *+10                                                             
         PACK  DUB,UHDREST(1)                                                   
*                                                                               
SSV10    CVB   R1,DUB                                                           
*                                                                               
SSV20    STCM  R1,3,SVEST          ESTIMATE NUMBER                              
*                                                                               
         MVI   SVWRTFLG,C'Y'       DEFAULT TO WRITE=YES                         
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   *+12                 - NO, REGULAR                               
         CLI   UHD2TSTR,C'Y'       WRITE INHIBIT FLAG GIVEN?  (BY2)             
         B     *+8                                                              
         CLI   UHDRTSTR,C'Y'       WRITE INHIBIT FLAG GIVEN?                    
         BNE   *+8                                                              
*                                                                               
         MVI   SVWRTFLG,C'N'       CORRECT -- WRITE=NO                          
         L     R1,AMINBLK                                                       
         MVC   MINWRITE-MINBLKD(,R1),SVWRTFLG 'N' = DON'T WRITE TO FILE         
*                                                                               
         MVI   SVUPDFLG,C'N'       DEFAULT TO NOT UPDATES ONLY                  
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   SSV25                - NO, REGULAR                               
         CLI   UHD2UPDO,C'Y'       UPDATES ONLY? (BY2)                          
         B     *+8                                                              
*** SBY2                                            MHC  01/16/03               
*                                                                               
SSV25    CLI   UHDRUPDO,C'Y'       UPDATES ONLY?                                
         BNE   *+8                                                              
         MVI   SVUPDFLG,C'Y'       YES                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE HEADER RECORD FIELDS, SO WE DON'T UPLOAD A SHITLOAD OF           
* BOGUS BUYS FOR NOTHING.                                                       
***********************************************************************         
VALHEADR NTR1                                                                   
*                                                                               
         MVI   ERFLDNUM,SHDRSYSQ   SYSTEM                                       
         CLI   SVSYS,C'S'                                                       
         BNE   VHINV                                                            
         MVI   ERFLDNUM,SHDRUTYQ   UPLOAD TYPE                                  
         CLC   SVTYPE,=C'BUY'                                                   
         BE    VH05                 - YES, CONTINUE                             
*** SBY2                                                                        
         CLC   SVTYPE,=C'BY2'       - NO, THEN IS IT C'BY2'?  (20 DEMS)         
         BNE   VHINV                  - NOPE, INVALID                           
*** SBY2                                     MHC  01/16/03                      
VH05     MVI   ERFLDNUM,SHDRAGIQ   AGENCY                                       
         CLC   SVAGY,SPACES                                                     
         BNH   VHMISS                                                           
         MVI   ERFLDNUM,SHDRMEDQ   MEDIA                                        
         CLI   SVMED,C' '                                                       
         BNH   VHMISS                                                           
         MVI   ERFLDNUM,SHDRCLTQ   CLIENT                                       
         CLC   SVCLT,SPACES                                                     
         BNH   VHMISS                                                           
         MVI   ERFLDNUM,SHDRPRDQ   PRODUCT                                      
         CLC   SVPRD,SPACES                                                     
         BNH   VHMISS                                                           
         MVI   ERFLDNUM,SHDRESTQ   ESTIMATE                                     
         OC    SVEST,SVEST                                                      
         BZ    VHMISS                                                           
*                                                                               
         MVI   ERFLDNUM,SHDRAGIQ   AGENCY                                       
         LA    R5,KEY                                                           
         USING AGYHDRD,R5                                                       
         XC    AGYKEY,AGYKEY       BUILD AGENCY KEY                             
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,SVAGY                                                    
         BAS   RE,RDSPOT                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFIL',KEY+14,AIO,DMWORK         
         CLI   DMCB+8,0                                                         
         BNE   VHRECNFN                                                         
         L     R5,AIO                                                           
         MVC   SVAPRF14,AGYPROF+14                                              
         DROP  R5                                                               
*                                                                               
         MVI   ERFLDNUM,SHDRMEDQ   MEDIA                                        
         GOTO1 MEDGET,DMCB,(SVMED,SVAGY),DATAMGR,WORK                           
         CLI   8(R1),X'FF'         INVALID                                      
         BE    VHINV                                                            
         L     R1,8(R1)                                                         
         MVC   SVBAM,0(R1)         SAVE AGY/MED                                 
*                                                                               
         MVI   ERFLDNUM,SHDRCLTQ   CLIENT                                       
         GOTO1 CLPACK,DMCB,SVCLT,SVBCLT                                         
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         XC    CKEY,CKEY           BUILD CLIENT HEADER KEY                      
         MVC   CKEYAM,SVBAM        SET A/M BYTE                                 
         MVC   CKEYCLT,SVBCLT      CLIENT                                       
         BAS   RE,RDSPOT                                                        
         CLI   DMCB+8,0                                                         
         BNE   VHRECNFN                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFIL',KEY+14,AIO,DMWORK         
         CLI   DMCB+8,0                                                         
         BNE   VHRECNFN                                                         
         MVI   ERFLDNUM,SHDRPRDQ   PRODUCT                                      
         L     R5,AIO1                                                          
         LA    R4,CLIST                                                         
*                                                                               
VH20     OC    0(4,R4),0(R4)                                                    
         BZ    VHINV                                                            
         CLC   0(3,R4),SVPRD       PRODUCT FOUND                                
         BE    VH30                                                             
         LA    R4,4(R4)                                                         
         B     VH20                                                             
*                                                                               
VH30     MVC   SVBPRD,3(R4)        BINARY PRODUCT CODE                          
         DROP  R5                                                               
*                                                                               
         MVI   ERFLDNUM,SHDRPRDQ   PRODUCT                                      
         LA    R5,KEY                                                           
         USING PRDHDRD,R5                                                       
         XC    PKEY,PKEY           BUILD PRODUCT HEADER KEY                     
         MVC   PKEYAM,SVBAM        SET A/M BYTE                                 
         MVC   PKEYCLT,SVBCLT      CLIENT                                       
         MVC   PKEYPRD,SVPRD       PRODUCT                                      
         BAS   RE,RDSPOT                                                        
         CLI   DMCB+8,0                                                         
         BNE   VHRECNFN                                                         
         DROP  R5                                                               
*                                                                               
         CLC   SVPRD2,SPACES                                                    
         BNH   VH40                                                             
         MVI   ERFLDNUM,SHDRPR2Q   PRODUCT 2                                    
         LA    R5,KEY                                                           
         USING PRDHDRD,R5                                                       
         XC    PKEY,PKEY           BUILD PRODUCT HEADER KEY                     
         MVC   PKEYAM,SVBAM        SET A/M BYTE                                 
         MVC   PKEYCLT,SVBCLT      CLIENT                                       
         MVC   PKEYPRD,SVPRD2      PRODUCT 2                                    
         BAS   RE,RDSPOT                                                        
         CLI   DMCB+8,0                                                         
         BNE   VHRECNFN                                                         
         DROP  R5                                                               
*                                                                               
VH40     MVI   ERFLDNUM,SHDRESTQ   ESTIMATE                                     
         LA    R5,KEY                                                           
         USING ESTHDRD,R5                                                       
         XC    EKEY,EKEY           BUILD ESTIMATE HEADER KEY                    
         MVC   EKEYAM,SVBAM        SET A/M BYTE                                 
         MVC   EKEYCLT,SVBCLT      CLIENT                                       
         MVC   EKEYPRD,SVPRD       PRODUCT                                      
         MVC   EKEYEST,SVEST+1     ESTIMATE                                     
         BAS   RE,RDSPOT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   ERRNUM,=H'16'       INVALID ESTIMATE                             
         B     VHX                                                              
*                                                                               
         CLC   SVPRD2,SPACES       PRODUCT 2                                    
         BNH   VH50                                                             
         MVI   ERFLDNUM,SHDRESTQ   ESTIMATE                                     
         LA    R5,KEY                                                           
         USING ESTHDRD,R5                                                       
         XC    EKEY,EKEY           BUILD ESTIMATE HEADER KEY                    
         MVC   EKEYAM,SVBAM        SET A/M BYTE                                 
         MVC   EKEYCLT,SVBCLT      CLIENT                                       
         MVC   EKEYPRD,SVPRD2      PRODUCT 2                                    
         MVC   EKEYEST,SVEST+1     ESTIMATE                                     
         BAS   RE,RDSPOT                                                        
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   ERRNUM,=H'16'       INVALID ESTIMATE                             
         B     VHX                                                              
*                                                                               
VH50     CLI   SVAPRF14,C'Y'       TEST BUYER/BILLER SAVED                      
         BNE   VH60                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0MK'                                                 
         MVC   WORK+4(2),SVAGY                                                  
         MVC   WORK+6(1),SVMED                                                  
         MVC   WORK+7(3),SVCLT                                                  
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGETPROF                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,WORK,WORK+16,DATAMGR                                   
*                                                                               
         CLI   WORK+16+4,C'Y'      TEST TO VALIDATE BUYER NAME                  
         BNE   VH60                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DE4'                                                  
         MVC   KEY+2(1),SVBAM                                                   
         NI    KEY+2,X'F0'         DROP MEDIA                                   
*                                                                               
         L     R1,ADATA                                                         
         MVC   KEY+3(8),SHDRBID-SHDRSYS(R1)                                     
         OC    KEY+3(10),SPACES                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',KEY,KEY                   
         MVI   ERFLDNUM,SHDRBIDQ   BUY ID                                       
         CLC   KEY(13),KEYSAVE                                                  
         BE    VH60                                                             
         MVC   ERRNUM,=H'927'      BUYER NAME ISN'T ON FILE                     
         B     VHX                                                              
*                                                                               
*                                  INITMNIO DOESN'T HAVE VALUES BEFORE          
VH60     BAS   RE,FRSTMIN            VALHEADR                                   
***************                                                                 
* SPECIAL FOR SJR                                                               
***************                                                                 
*                                  GBUYINIT ALSO DID NOT HAVE VALUES            
         CLC   SVAGY,=C'SJ'           BEFORE VALHEADR                           
         BNE   VHX                                                              
         CLC   SVCLT,=C'TBL'       ONLY CLT TBL ON SJR HAS 2 BYTES              
         BNE   VHX                                                              
         MVI   SV1OR2,2            HARD CODE THIS                               
***************                                                                 
*                                                                               
VHX      B     XIT                 HEADER RECORD FIELDS ARE VALID               
*                                                                               
VHMISS   MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
*                                                                               
VHRECNFN MVC   ERRNUM,=H'53'       RECORD NOT FOUND                             
         B     VHX                                                              
*                                                                               
VHINV    MVC   ERRNUM,=H'2'        INVALID INPUT FIELD                          
         B     VHX                                                              
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* READ SPOT FILE                                                                
***********************************************************************         
RDSPOT   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'SPTDIR',KEY,KEY                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEAL WITH FIRST MINIO RECORD FOR ESTIMATE                                     
***********************************************************************         
FRSTMIN  NTR1                                                                   
         MVI   UPLOC,C'N'                                                       
         MVI   ERFLDNUM,0          ALL FIELDS ARE OK                            
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         LA    R3,MINMKEY                                                       
         USING SPUPKEY,R3                                                       
         MVC   SPUPTYPE,=X'0D78'   RECORD TYPE                                  
         MVC   SPUPAM,SVBAM        AGENCY/MEDIA                                 
         MVC   SPUPCLT,SVBCLT      CLIENT                                       
         MVC   SPUPPRD,SVBPRD      PRODUCT                                      
         MVC   SPUPEST,SVEST+1     ESTIMATE                                     
         MVC   SPUPAGY,SVAGY       AGENCY                                       
         XC    MINEKEY,MINEKEY     SEE IF RECORD EXISTS                         
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
         BAS   RE,READMIN                                                       
         BNE   FM50                                                             
*                                  CHECK IF UPLOADED BY LOCATION                
         LA    R3,ELEMENT                                                       
         USING SPUPEL01,R3                                                      
         TM    SPUPSTAT,SPUPSLOC   IF FIRST ONE WAS BY LOC                      
         BNO   FM10                                                             
         CLC   SVLOCID,SPACES      ALL OTHERS MUST BE                           
         BNH   FM20                                                             
         B     FM30                EXIT                                         
*                                                                               
FM10     CLC   SVLOCID,SPACES      IF 1ST ONE NOT BY LOCATION                   
         BNH   FM40                NO ON ELSE CAN BE                            
*                                                                               
FM20     XC    MINEKEY,MINEKEY     UNLESS NO RECORDS ON FILE                    
         MVI   MINEKEY,X'90'       LOOK FOR FIRST BUY OF ESTIMATE               
         MVI   MINFILTL,1          L'COMPARE OF ELEMENT KEY (CODE)              
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      THERE ARE BUYS                               
         BNE   FMINV               ERROR                                        
         XC    MINEKEY,MINEKEY     GET ACTIVITY ELEMENT AGAIN                   
         MVI   MINEKEY,X'01'                                                    
         BAS   RE,READMIN                                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                WE JUST READ IT                              
         NI    SPUPSTAT,X'FF'-SPUPSLOC                                          
         CLC   SVLOCID,SPACES                                                   
         BNH   *+8                                                              
         OI    SPUPSTAT,SPUPSLOC   SET BY LOCATION                              
         BAS   RE,WRTMIN                                                        
*                                                                               
FM30     MVI   UPLOC,C'Y'                                                       
*                                                                               
FM40     B     FMX                                                              
*                                                                               
FM50     XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT          RECORD DOESN'T EXIST YET                     
         USING SPUPEL01,R3         ADD IT - WITH AN ACTIVITY ELEMENT            
         MVI   SPUPEL01,X'01'      ELEMENT CODE                                 
         MVI   SPUPEL1L,SPUPEL1Q   ELEMENT LENGTH                               
         GOTO1 DATCON,DMCB,(5,0),(3,SPUPUDT)   UPLOAD DATE                      
         MVI   SPUPSEQN,0          FIRST UPLOAD OF THIS ESTIMATE                
         CLC   SVLOCID,SPACES      UPLOADED BY LOCATION                         
         BNH   *+8                                                              
         OI    SPUPSTAT,SPUPSLOC                                                
         DROP  R3                                                               
*                                                                               
         BAS   RE,ADDMIN           ADD MINIO                                    
*                                                                               
         BAS   RE,CLSMIN                                                        
*                                                                               
FMX      B     XIT                                                              
*                                                                               
FMINV    MVC   ERRNUM,=H'2'        INVALID INPUT FIELD                          
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   *+12                 - NO, REGULAR                               
         MVI   ERFLDNUM,SHD2UIDQ   LOCATION ID (BY2)                            
         B     *+8                                                              
*** SBY2                                         MHC  01/16/03                  
         MVI   ERFLDNUM,SHDRUIDQ   LOCATION ID                                  
         B     FMX                                                              
         EJECT                                                                  
***********************************************************************         
* STOP PROCESSING IF HEADER IN ERROR                                            
***********************************************************************         
HDRERR   NTR1                                                                   
         LA    RF,WORK                                                          
         USING DSTATUSD,RF                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         MVC   DSTARECN,=C'00000'  RECORD ZERO = HEADER                         
         MVC   DSTASUBR,=C'00'     NO SUB-RECORD                                
         ZIC   R0,ERFLDNUM         FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
         SR    R0,R0                                                            
         ICM   R0,3,ERRNUM         ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
         L     R3,=A(ITSBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         DROP  RF                                                               
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES THE ACTIVITY ELEMENT IN THE MINIO RECORD                 
***********************************************************************         
UPDACTV NTR1                                                                    
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY     RECORD MUST EXIST                            
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ELEMENT                                                       
         USING SPUPEL01,R3                                                      
         MVC   SPUPLDT,SPUPUDT     SAVE PREVIOUS UPLOAD DATE                    
         GOTO1 DATCON,DMCB,(5,0),(3,SPUPUDT)   NEW UPLOAD DATE                  
         CLC   SPUPLDT,SPUPUDT     SAME DAY AS PREVIOUS UPLOAD?                 
         BNE   UPDA10                                                           
         SR    R1,R1               YES -- INCREMENT COUNTER                     
         IC    R1,SPUPSEQN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SPUPSEQN                                                      
*                                                                               
UPDA10   BAS   RE,WRTMIN                                                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL CLEAN UP THE MINIO RECORD IF WE RETURNED FROM               
* $BUY WITH A DUMP.  IT WILL UNMARK RECORDS THAT WERE MARKED FOR                
* DELETION IF THE RECORD WAS NEVER DELETED FROM THE SPOTFILE AND                
* IT WILL CHECK IF ELEMENTS WITH A LINE NUMBER OF ZERO, WERE ADDED              
* TO THE SPOTFILE (VIA UNIQUE ID'S)                                             
***********************************************************************         
CLEANUP  NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         XC    WORK,WORK                                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SPUPELQ     X'90' - LOOK FOR 1ST BUY OF EST              
         MVI   MINFILTL,1          LOOK FOR ALL OF THAT ELEMENT CODE            
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      ANY HISTORY FOR THIS ESTIMATE?               
         BE    CU50                NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CU10     LA    R3,ELEMENT                                                       
         USING SPUPMEL,R3                                                       
         CLI   SPUPSLN,0           IF THE SPOTPAK LINE NUMBER = 0               
         BNE   CU12                                                             
         BAS   RE,CHECKSPT         AND ITS NOT ON THE SPOT FILE                 
         BNE   CU15                DELETE THE ELEMENT                           
         CLC   KEY(9),KEYSAVE                                                   
         BNE   CU15                                                             
         BAS   RE,CHECKREC         GET RECORD & COMPARE UNIQUE ID               
         BNE   CU15                NOT FOUND - DELETE ELEMENT                   
         B     CU20                                                             
*                                                                               
CU12     TM    SPUPUSRC,X'80'      IS THIS MARKED FOR DELETION?                 
         BZ    CU20                                                             
         BAS   RE,CHECKSPT         SEE IF ITS ON THE SPOT FILE                  
         BNE   CU15                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CU15                                                             
         NI    SPUPUSRC,X'FF'-X'80' YES -- TAKE OFF DELETE BIT                  
         MVI   WORK,X'90'          SAVE ELEMENT KEY                             
         MVC   WORK+1(3),SPUPSTA                                                
         MVC   WORK+4(1),SPUPSLN                                                
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(6),WORK     RESTORE ELEMENT KEY                          
         BAS   RE,READMIN                                                       
         BE    CU20                                                             
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
CU15     MVC   WORK(6),MINEKEY     SAVE ELEMENT KEY                             
         BAS   RE,DELMIN                                                        
         MVC   MINEKEY(6),WORK     NOW FIND THE NEXT ONE                        
*                                                                               
CU20     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE BUYS FOR THIS STATION               
         BE    CU50                NO                                           
         CLI   MINERR,0                                                         
         BE    CU10                                                             
         DC    H'0'                                                             
***************                                                                 
* MIGHT HAVE TO CLEAN UP THE X'92' ELEMENTS AS WELL                             
***************                                                                 
CU50     XC    WORK,WORK                                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SPU2ELQ     X'92'                                        
         MVI   MINFILTL,1          LOOK FOR ALL OF THAT ELEMENT CODE            
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      ANY HISTORY FOR THIS ESTIMATE?               
         BE    CUX                 NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CU60     LA    R3,ELEMENT                                                       
         USING SPU2MEL,R3                                                       
         TM    SPU2SLN,X'80'       IF A SEQUENCE NUMBER                         
         BZ    CU62                                                             
         BAS   RE,CHECKSPT         AND ITS NOT ON THE SPOT FILE                 
         BNE   CU65                DELETE THE ELEMENT                           
         CLC   KEY(9),KEYSAVE                                                   
         BNE   CU65                                                             
         BAS   RE,CHECKREC         GET RECORD & COMPARE UNIQUE ID               
         BNE   CU65                NOT FOUND - DELETE ELEMENT                   
         B     CU70                                                             
*                                                                               
CU62     TM    SPU2USRC,X'80'      IS THIS MARKED FOR DELETION?                 
         BZ    CU70                                                             
         BAS   RE,CHECKSPT         SEE IF ITS ON THE SPOT FILE                  
         BNE   CU65                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CU65                                                             
         NI    SPU2USRC,X'FF'-X'80' YES -- TAKE OFF DELETE BIT                  
         MVI   WORK,X'92'          SAVE ELEMENT KEY                             
         MVC   WORK+1(3),SPU2STA                                                
         MVC   WORK+4(2),SPU2SLN                                                
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(6),WORK     RESTORE ELEMENT KEY                          
         BAS   RE,READMIN                                                       
         BE    CU70                                                             
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
CU65     MVC   WORK(6),MINEKEY     SAVE ELEMENT KEY                             
         BAS   RE,DELMIN                                                        
         MVC   MINEKEY(6),WORK     NOW FIND THE NEXT ONE                        
*                                                                               
CU70     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE BUYS FOR THIS STATION               
         BE    CUX                 NO                                           
         CLI   MINERR,0                                                         
         BE    CU60                                                             
         DC    H'0'                                                             
*                                                                               
CUX      CLI   MINCHG,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,CLSMIN                                                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS THE SPOT FILE TO SEE IF A BUY EXIST ON THE FILE           
***********************************************************************         
CHECKSPT NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LA    R3,MINMKEY                                                       
         USING SPUPKEY,R3          FROM MASTER RECORD                           
         MVC   SPUPTYPE,=X'0D78'   RECORD TYPE                                  
         LA    R4,KEY                                                           
         USING BUYHDRD,R4                                                       
         XC    BUYKEY,BUYKEY       BUILD BUY RECORD KEY                         
         MVC   BUYKAM,SPUPAM                                                    
         MVC   BUYKCLT,SPUPCLT                                                  
         MVC   BUYKPRD,SPUPPRD                                                  
         MVC   BUYKEST,SPUPEST                                                  
*                                                                               
         LA    R3,ELEMENT                                                       
         CLI   0(R3),SPUPELQ       X'90' - OLD INSERTION ELEMENT                
         BNE   CKSPT10                                                          
         MVC   BUYKBUY+2(1),SPUPSLN-SPUPMEL(R3)                                 
         B     CKSPT20                                                          
*                                                                               
CKSPT10  CLI   0(R3),SPU2ELQ       X'92' - INS ELEM FOR 2 BYTE BUYLINES         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUYKBUY+1(2),SPU2SLN-SPU2MEL(R3)                                 
*                                                                               
CKSPT20  BRAS  RE,GBUYHIGH                                                      
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BNE   NO                  RECORD NOT FOUND                             
         B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS ALL THE RECORDS & COMPARES UNIQUE ID'S                     
***********************************************************************         
CHECKREC NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LA    R3,ELEMENT          R3 WILL BE EITHER X'90' OR X'92'             
*                                                                               
CR10     BRAS  RE,GBUYGETR                                                      
         L     R4,AIO                                                           
         USING BUYHDRD,R4                                                       
*                                                                               
         LA    R4,BDELEM           GET UNIQUE ID ELEMENT                        
CR20     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    CR50                                                             
         CLI   0(R4),BUPCODEQ      X'95' - UNIQUE ID ELEMENT                    
         BNE   CR20                                                             
*                                                                               
         USING BUPELEM,R4                                                       
         CLI   0(R3),SPUPELQ       X'90' ELEM?                                  
         BNE   CR25                                                             
         USING SPUPMEL,R3                                                       
         CLC   BUPUID,SPUPUUID     DO THE UNIQUE ID'S MATCH                     
         BNE   CR50                                                             
         B     CR30                                                             
*                                                                               
         USING SPU2MEL,R3          X'92' ELEM                                   
CR25     CLC   BUPUID,SPU2UUID     DO THE UNIQUE ID'S MATCH                     
         BNE   CR50                                                             
*                                                                               
CR30     LA    R4,KEY                                                           
         USING BUYHDRD,R4                                                       
         CLI   0(R3),SPUPELQ       X'90' ELEMENT??                              
         BNE   CR35                                                             
         USING SPUPMEL,R3                                                       
         MVC   SPUPSLN,BUYKBUY+2   YES - SET ONE BYTE LINE #                    
         B     CR40                                                             
*                                                                               
         USING SPU2MEL,R3          X'92' ELEM                                   
CR35     MVC   SPU2SLN,BUYKBUY+1   SET TWO BYTE LINE #                          
*                                                                               
CR40     BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     YES                                                              
*                                                                               
CR50     BRAS  RE,GBUYRSEQ                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    CR10                                                             
*                                                                               
CRNO     B     NO                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT GETS ALL BUYS               
* FOR ALL STATIONS AND PUTS THEM IN THE TEMPSTR BUFFERS.                        
***********************************************************************         
PROCMID  NTR1                                                                   
*                                                                               
         CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BNE   PM05                                                             
         CLI   SENTHDR,C'Y'                                                     
         BE    PM30                                                             
                                                                                
PM05     CLI   ENDOBJCT,C'Y'       IF END OF OBJECTS                            
         BE    PM30                THEN WE CAN CONFIRM                          
         CLI   ENDBLOCK,C'Y'       IF END OF BLOCK                              
         BE    PM30                THEN WE CAN CONFIRM                          
*                                                                               
         CLI   TMPOPEND,C'Y'       IS TEMPSTR ALREADY OPEN?                     
         BE    PM10                                                             
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'PTX'),,(3,0)                               
         BNE   EXIT                                                             
         MVI   TMPOPEND,C'Y'       TEMPSTR IS OPEN                              
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   *+12                 - NO, REGULAR                               
         LH    R3,=Y(SHD2LENQ)     LONGER LENGTH (BY2)                          
         B     *+8                                                              
*** SBY2                                     MHC  01/16/03                      
         LH    R3,=Y(SHDRLENQ)     WRITE FILE HEADER OBJECT TO TEMPSTR          
         GOTO1 PUTTMP,DMCB,SVHDROBJ,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
PM10     BAS   RE,GETOBJCT                                                      
*                                                                               
         CLI   EIFFLAG,C'Y'        ANY MORE DATA FOR THIS INPUT FRAME?          
         BE    PM20                NO MORE                                      
*                                                                               
         CLI   ENDOBJCT,C'Y'       ANY MORE OBJECTS COMING?                     
         BE    PM15                                                             
         CLI   ENDBLOCK,C'Y'       IF END OF BLOCK                              
         BNE   PM10                                                             
*                                                                               
PM15     GOTO1 TMPCLOSE            DONE WITH PUTTING INTO TEMPFILE              
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
*                                                                               
         BAS   RE,CALLBUY          CALL SPOT BUY PROGRAM                        
         BC    0,*                                                              
*                                                                               
PM20     CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BE    PMX                                                              
         MVI   OVERMODE,C'M'       YES                                          
         B     PMX                                                              
*                                                                               
PM30     CLI   CONFFLAG,X'FF'      CONFIRM USED BEFORE?                         
         BE    PM40                YES                                          
*                                  DID WE GET CONTROL BACK FROM BUY?            
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   PM40                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVXCTL                                  
         CLI   DMCB+8,0                                                         
         BE    PM40                                                             
         MVC   MDACTION,=Y(ER10GDEL)  PROBLEM WITH GLOBBER DELETE               
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
PM40     BAS   RE,CONFIRM          SEND OUT CONFIRMATIONS                       
         CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BNE   PM45                                                             
         CLI   SENTHDR,C'Y'                                                     
         BE    PM60                                                             
*                                                                               
PM45     CLI   ENDOBJCT,C'Y'       DID WE SEE THE END-OF-DATA OBJECT?           
         BNE   PM10                NO - GO GET NEXT                             
*                                                                               
         OC    TOTALDEL,TOTALDEL   ANY BUYS DELETED?                            
         BZ    PM50                                                             
         LA    R2,WORK             BUILD RECORD DELETED TOTAL OBJECT            
         XC    WORK,WORK                                                        
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'04'     RECORDS DELETED OBJECT                       
         L     R0,TOTALDEL         TOTAL NUMBER OF BUYS DELETED                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTATOTD,DUB+4(4)                                                
         L     R3,=A(ITSBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
         DROP  R2                                                               
                                                                                
PM50     GOTO1 PUTITEM,DMCB,ITEOD,0   THAT'S ALL, FOLKS!                        
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'            LAST OUTPUT DATA FRAME                    
         B     PMX                                                              
                                                                                
PM60     MVI   SENTHDR,C'N'                                                     
         B     PM10                   GO GET NEXT                               
                                                                                
PMX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
***********************************************************************         
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE BUYS FOR A STATION FROM THE INPUT FRAMES                
***********************************************************************         
GETOBJCT NTR1                                                                   
         MVI   MINCHG,C'N'         NO CHANGES TO MINIO RECORD YET               
*                                                                               
GOBJLOOP BAS   RE,GETDATA                                                       
*                                                                               
         CLI   EIFFLAG,C'Y'        END OF FRAME?                                
         BE    GOBJ50              NO                                           
         CLC   TYPENUM,=A(ITEOD)   IF OBJECT IS END-OF-DATA                     
         BNE   *+12                                                             
         MVI   ENDOBJCT,C'Y'       NO MORE OBJECTS TO READ                      
         B     GOBJ50                                                           
*                                                                               
         CLC   TYPENUM,=A(ITEOB)   IF OBJECT IS END-OF-BLOCK                    
         BNE   *+12                                                             
         MVI   ENDBLOCK,C'Y'                                                    
         B     GOBJ50                                                           
*                                                                               
         L     R1,AIO              COPY THE OBJECT TO LOCAL STORAGE             
         LA    R1,4(R1)            LEAVE ROOM FOR OBJECT IDENTIFIER             
         MVI   0(R1),C' '                                                       
         MVC   1(255,R1),0(R1)     PRE-FILL WITH BLANKS                         
         LR    R0,R1                                                            
         L     R1,DATALEN                                                       
         L     RE,ADATA                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,VALOBJCT         VALIDATE THE OBJECT                          
*                                                                               
         B     GOBJLOOP                                                         
*                                                                               
GOBJ50   CLI   MINCHG,C'Y'         ANY CHANGE TO THIS MINIO RECORD?             
         BNE   GOBJX                                                            
*                                                                               
         L     R5,AMINBLK          YES -- SAVE IT AT END OF FRAME               
         USING MINBLKD,R5                                                       
         BAS   RE,CLSMIN                                                        
                                                                                
GOBJX    B     XIT                 RETURN TO CALLER                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TESTS IF WE NEED TEST OR REAL DATA & GETS IT                     
***********************************************************************         
GETDATA  NTR1                                                                   
         MVI   TSTDATAF,0                                                       
         L     R1,ATWA             USE INTERNAL TABLE ENTRY?                    
         LA    R1,MADDATA-TA0CFFD(R1)                                           
         CLI   0(R1),C'X'          ONE OF OURS?                                 
         BNE   GD10                                                             
         MVC   TSTDATAF,1(R1)                                                   
         CLI   1(R1),C'1'                                                       
         BE    *+12                                                             
         CLI   1(R1),C'2'                                                       
         BNE   GD10                                                             
*                                                                               
         GOTO1 GETTITEM            GET TEST TABLE ITEM                          
         B     GDX                                                              
*                                                                               
GD10     GOTO1 GETITEM             GET FIRST OBJECT                             
         BNE   EXIT                                                             
*                                                                               
GDX      B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE OBJECT WE GOT                                      
***********************************************************************         
VALOBJCT NTR1                                                                   
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         CLC   TYPENUM,=A(ITSBYBUY) BUY OBJECT?                                 
         BNE   VO10                                                             
         BAS   RE,VALBUY                                                        
         B     VOBJX                                                            
*                                                                               
VO10     CLC   TYPENUM,=A(ITSBYROT) ROTATION OBJECT?                            
         BNE   VO20                                                             
         BAS   RE,SETROT                                                        
         B     VOBJX                                                            
*                                                                               
VO20     CLC   TYPENUM,=A(ITSBYORB) ORBIT OBJECT?                               
         BNE   VO30                                                             
         BAS   RE,SETORB                                                        
         B     VOBJX                                                            
*                                                                               
VO30     CLC   TYPENUM,=A(ITSBYCOM) COMMENT OBJECT?                             
         BNE   VO40                                                             
         BAS   RE,SETCOM                                                        
         B     VOBJX                                                            
*                                                                               
VO40     CLC   TYPENUM,=A(ITSBYSKD) SCHEDULE OBJECT?                            
         BNE   VO50                                                             
         BAS   RE,SETSKD                                                        
         B     VOBJX                                                            
*                                                                               
VO50     CLC   TYPENUM,=A(ITSBYDEM) DEMO OBJECT?                                
         BNE   VO60                                                             
         BAS   RE,SETDEM                                                        
         B     VOBJX                                                            
*                                                                               
VO60     CLC   TYPENUM,=A(ITSBYEBY) END OF BUY OBJECT?                          
         BNE   VO70                                                             
         BAS   RE,SETEIN                                                        
         B     VOBJX                                                            
*                                                                               
VO70     CLC   TYPENUM,=A(ITSBYEOE) END OF EST OBJECT?                          
         BNE   VO80                                                             
         BAS   RE,SETEOE                                                        
         B     VOBJX                                                            
*                                                                               
VO80     DS    0H                                                               
*                                                                               
VOBJX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE BUY OBJECT                                         
***********************************************************************         
VALBUY   NTR1                                                                   
         MVI   SVACT,C' '                                                       
         XC    MYSTAT,MYSTAT                                                    
         L     R6,AIO              BUY OBJECT                                   
         USING UBUYD,R6                                                         
         MVC   UBUYTYPE,=C'BUY*'                                                
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
*                                                                               
         LA    R1,WORK                                                          
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,SVAGY                                                    
         MVC   STAPMED,SVMED                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,UBUYSTA                                                 
*                                                                               
         CLI   STAPQSTA,C'0'       IS THIS CABLE?                               
         BL    *+14                                                             
         MVC   STAPQSTA(L'UBUYSTA),UBUYSTA   THEN COPY THE NETWORK ALSO         
         B     VB10                                                             
*                                                                               
         CLI   STAPQSTA+4,C' '     ELSE MAKE SURE BAND IS SET                   
         BH    VB10                                                             
         MVC   STAPQSTA+4(1),SVMED                                              
VB10     DS    0H                                                               
         GOTO1 STAPACK,(R1)                                                     
         MVC   SVSTA,STAPSTA                                                    
         DROP  R1                                                               
*                                                                               
*****    CLC   LASTSTA,SVSTA                                                    
*****    BE    VB15                                                             
         MVI   SEQNUM,1            ALWAYS RESET SEQNUM                          
*****    MVC   LASTSTA,SVSTA                                                    
*                                                                               
VB15     MVI   ENDBLOCK,C'N'       NOT END OF BLOCK                             
*                                                                               
         XC    MINEKEY,MINEKEY     ANY BUYS FOR THIS STATION?                   
         MVI   MINEKEY,SPUPELQ     X'90' ELEM                                   
         MVC   MINEKEY+1(3),SVSTA                                               
         MVI   MINFILTL,4                                                       
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF                                                   
         BE    VB20                NO, MIGHT BE A X'92' ELEM                    
         CLI   MINERR,0                                                         
         BE    VB30                                                             
         DC    H'0'                                                             
*                                                                               
VB20     MVI   MINEKEY,SPU2ELQ     X'92' ELEM                                   
         MVC   MINEKEY+1(3),SVSTA                                               
         MVI   MINFILTL,4                                                       
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF                                                   
         BE    VB40                NO -- ADD THIS BUY                           
         CLI   MINERR,0                                                         
         BE    VB30                                                             
         DC    H'0'                                                             
*                                                                               
VB30     LA    R3,ELEMENT                                                       
         CLI   0(R3),SPUPELQ       X'90' ELEM?                                  
         BNE   VB31                                                             
         CLC   SPUPUUID-SPUPMEL(,R3),UBUYUID    SAME UNIQUE ID?                 
         BE    VB50                YES - IT'S A CHANGE/DEL                      
* DON'T BUMP SEQ # IF ELEMENT IS ALREADY ASSOCIATED TO A SPOTPAK LINE#          
         CLI   SPUPSLN-SPUPMEL(R3),0                                            
         BNE   VB34                                                             
         B     VB33                                                             
*                                                                               
VB31     CLC   SPU2UUID-SPU2MEL(,R3),UBUYUID    SAME UNIQUE ID?                 
         BE    VB50                YES - IT'S A CHANGE/DEL                      
* DON'T BUMP SEQ # IF ELEMENT IS ALREADY ASSOCIATED TO A SPOTPAK LINE#          
         TM    SPU2SLN-SPU2MEL(R3),X'80'                                        
         BZ    VB34                                                             
*                                                                               
VB33     LLC   R1,SEQNUM           SAME BUY STATION                             
         LA    R1,1(R1)            SO INCREMENT SEQUENCE NUMBER                 
         CHI   R1,256                                                           
         BL    *+6                                                              
         DC    H'0'                THIS MIGHT BE POSSIBLE                       
         STC   R1,SEQNUM                                                        
*                                                                               
VB34     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE BUYS FOR THIS STATION?              
         BNE   VB35                                                             
         CLI   MINEKEY,SPU2ELQ     ALREADY PROCESSED X'92'?                     
         BE    VB40                YES, ADD THIS BUY THEN                       
         B     VB20                NO, PROCESS X'92' AS WELL                    
*                                                                               
VB35     CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VB30                                                             
*                                                                               
VB40     CLI   UBUYDEL,C'Y'        DELETING WHAT I GOING TO ADD?                
         BNE   VB48                                                             
         OI    MYSTAT,MYSDEL       SET BUY DELETED                              
         MVI   SVACT,C'D'                                                       
*                                                                               
         LA    RE,MYOBJECT         BUILD BUY OBJECT HERE                        
         USING SBUYD,RE                                                         
         XC    SBUYD(SBY2LENQ),SBUYD                                            
         MVC   SBUYRNUM,=C'00'                                                  
         MVC   SBUYLEN,=Y(SBUYLENQ) L'RECORD                                    
         MVC   SBUYSTRT(SBUYRLNQ),UBUYD  OBJECT DATA                            
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   VB42                 - NOPE                                      
         MVC   SBUYLEN,=Y(SBY2LENQ) L'RECORD                                    
         MVC   SBUYSTRT(SBY2RLNQ),UBUYD  OBJECT DATA                            
*** SBY2                                     MHC  01/16/03                      
*                                                                               
VB42     XC    SBUYLIN2,SBUYLIN2   OVERWRITTEN BY "MVC  SBUYSTRT"               
         CLI   SVACT,C'A'                                                       
         BNE   *+14                                                             
         MVC   SBUYSEQ,SEQNUM      SEQUENCE # (FOR ADD)                         
         B     VB44                                                             
*                                                                               
         MVC   SBUYLINE,SVLINE+1   SPOTPAK LINE NUMBER (FOR CHANGE)             
         MVC   SBUYLIN2,SVLINE                                                  
*                                                                               
VB44     MVC   SBUYACTN,SVACT      ACTION                                       
         MVC   SBUYERNO,=H'53'     RECORD DOES NOT EXIST                        
         MVI   SBUYERF,16          POINT TO UID FIELD                           
         SR    R1,R1                                                            
         PACK  DUB,SBUYNUM                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,SBUYRNUM       BUY RECORD NUMBER                            
         STCM  R1,3,BUYNUM         BUY RECORD NUMBER                            
         LH    RF,SBUYLEN          RECORD LENGTH                                
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         L'OBJECT FOR TEMPSTR SO FAR                  
         DROP  RE                                                               
         B     VBX                                                              
*                                                                               
VB48     BAS   RE,SETUPEL          ADD SPUPEL/SPU2EL TO MINIO                   
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),ELEMENT                                               
         MVC   MINEKEY+1(L'SPUPEKEY-1),ELEMENT+2                                
         BAS   RE,HIGHMIN                                                       
*                                                                               
VB50     CLI   UBUYDEL,C'Y'        DELETE THIS BUY?                             
         BNE   VB60                                                             
         OI    MYSTAT,MYSDEL       SET BUY DELETED                              
         MVI   SVACT,C'D'                                                       
         LA    R3,ELEMENT                                                       
         CLI   0(R3),SPUPELQ                                                    
         BNE   VB53                                                             
         USING SPUPMEL,R3                                                       
         OI    SPUPUSRC,X'80'      YES - MARK IT FOR DELETION                   
         NI    SPUPUSRC,X'FF'-X'41'      & TURN OFF KEEP FLAG                   
         B     VB56                                                             
*                                                                               
         USING SPU2MEL,R3                                                       
VB53     OI    SPU2USRC,X'80'      YES - MARK IT FOR DELETION                   
         NI    SPU2USRC,X'FF'-X'41'      & TURN OFF KEEP FLAG                   
*                                                                               
VB56     BAS   RE,WRTMIN                                                        
         BE    *+10                                                             
         MVC   UBUYSTA,=C'1AAAA'   FORCE STATION ERROR                          
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     VBX                                                              
*                                                                               
VB60     CLI   SVACT,C'A'          IF ACTION IS ADD                             
         BE    VB80                CONTINUE                                     
         MVI   SVACT,C'C'          POSSIBLE CHANGE OF BUY                       
         LA    R3,ELEMENT                                                       
         CLI   0(R3),SPUPELQ                                                    
         BNE   VB63                                                             
         USING SPUPMEL,R3                                                       
         OI    SPUPUSRC,X'40'      KEEP THIS BUY                                
         MVC   SPUPBNUM,UBUYNUM    BUY LINE NUMBER IN FILE                      
         B     VB66                                                             
*                                                                               
         USING SPU2MEL,R3                                                       
VB63     OI    SPU2USRC,X'40'      KEEP THIS BUY                                
         MVC   SPU2BNUM,UBUYNUM    BUY LINE NUMBER IN FILE                      
         B     VB66                                                             
*                                                                               
VB66     BAS   RE,WRTMIN                                                        
         BE    *+10                                                             
         MVC   UBUYSTA,=C'1AAAA'   FORCE STATION ERROR                          
*                                                                               
         USING SPUPMEL,R3                                                       
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         XC    SVLINE,SVLINE                                                    
         MVC   SVLINE+1(1),SPUPSLN  SPOTPAK LINE NUMBER                         
*                                                                               
         CLI   SPUPMEL,SPU2ELQ     X'92' - 2 BYTE BUYLINE ELEMENT?              
         BNE   VB80                                                             
         USING SPU2MEL,R3                                                       
         MVC   SVLINE,SPU2SLN                                                   
         DROP  R3                                                               
*                                                                               
VB80     LA    RE,MYOBJECT         BUILD BUY OBJECT HERE                        
         USING SBUYD,RE                                                         
         XC    SBUYD(SBY2LENQ),SBUYD                                            
         MVC   SBUYRNUM,=C'00'                                                  
         MVC   SBUYACTN,SVACT      ACTION                                       
         MVC   SBUYLEN,=Y(SBUYLENQ) L'RECORD                                    
         MVC   SBUYSTRT(SBUYRLNQ),UBUYD  OBJECT DATA                            
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   VB83                 - NOPE                                      
         MVC   SBUYLEN,=Y(SBY2LENQ) L'RECORD                                    
         MVC   SBUYSTRT(SBY2RLNQ),UBUYD  OBJECT DATA                            
*** SBY2                                     MHC  01/16/03                      
VB83     XC    SBUYLIN2,SBUYLIN2   OVERWRITTEN BY "MVC  SBUYSTRT"               
         CLI   SVACT,C'A'                                                       
         BNE   VB86                                                             
         MVC   SBUYSEQ,SEQNUM      SEQUENCE # (FOR ADD)                         
         B     VB90                                                             
*                                                                               
VB86     MVC   SBUYLINE,SVLINE+1   SPOTPAK LINE NUMBER (FOR CHANGE)             
         CLI   SVLINE,0               DO WE HAVE A 2 BYTE BUYLINE?              
         BE    VB90                   NO                                        
         MVC   SBUYLIN2,SVLINE        YES, SAVE IT AS SUCH                      
*                                                                               
VB90     SR    R1,R1                                                            
         PACK  DUB,SBUYNUM                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,SBUYRNUM       BUY RECORD NUMBER                            
         STCM  R1,3,BUYNUM         BUY RECORD NUMBER                            
         LH    RF,SBUYLEN          RECORD LENGTH                                
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         L'OBJECT FOR TEMPSTR SO FAR                  
         DROP  RE,R6                                                            
*                                                                               
VBX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP THE BUY ELEMENT FOR MINIO & ADD IT                       
***********************************************************************         
SETUPEL  NTR1                                                                   
         USING UBUYD,R6                                                         
         MVI   SVACT,C'A'          NEW BUY DATE -- ADD IT                       
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         CLI   SV1OR2,2                                                         
         BE    SUP10                                                            
         USING SPUPMEL,R3                                                       
         MVI   SPUPEL90,SPUPELQ    X'90' EL CODE                                
         MVI   SPUPEL9L,SPUPEL9Q   ELEMENT LENGTH                               
         MVC   SPUPSTA,SVSTA       STATION (PACKED)                             
         MVC   SPUPSEQ,SEQNUM      SEQUENCE NUMBER (AVOID DUPS)                 
         GOTO1 DATCON,DMCB,(5,0),(0,SPUPUDAT)   UPLOAD DATE                     
         MVC   SPUPUUID,UBUYUID    USER-SUPPLIED UNIQUE ID                      
         MVC   SPUPBNUM,UBUYNUM    BUY LINE NUMBER IN FILE                      
         OI    SPUPUSRC,X'41'      FLAGS: CAME FROM UPLOAD / KEEP IT            
         B     SUP15                                                            
*                                                                               
         USING SPU2MEL,R3                                                       
SUP10    MVI   SPU2EL90,SPU2ELQ    ELEMENT CODE                                 
         MVI   SPU2EL9L,SPU2EL9Q   ELEMENT LENGTH                               
         MVC   SPU2STA,SVSTA       STATION (PACKED)                             
         MVC   SPU2SEQ+1(1),SEQNUM   SEQUENCE NUMBER (AVOID DUPS)               
         OI    SPU2SEQ,X'80'         SO WE KNOW SPU2SLN IS NOT A BYLN           
         GOTO1 DATCON,DMCB,(5,0),(0,SPU2UDAT)   UPLOAD DATE                     
         MVC   SPU2UUID,UBUYUID    USER-SUPPLIED UNIQUE ID                      
         MVC   SPU2BNUM,UBUYNUM    BUY LINE NUMBER IN FILE                      
         OI    SPU2USRC,X'41'      FLAGS: CAME FROM UPLOAD / KEEP IT            
         DROP  R3                                                               
*                                                                               
SUP15    BAS   RE,ADDMIN                                                        
         BNE   SUP20                                                            
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     SUPX                                                             
*                                                                               
SUP20    MVC   UBUYSTA,=C'1AAAA'   ERROR ON MINIO ADD                           
*                                  FORCE STATION ERROR FOR UPLOAD               
SUPX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS A ROTATION OBJECT IN MYOBJECT                               
***********************************************************************         
SETROT   NTR1                                                                   
         L     R6,AIO                                                           
         USING UROTD,R6                                                         
         MVC   UROTTYPE,=C'ROT*'                                                
         LH    R1,=Y(SROTLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING SROTD,R3                                                         
         MVC   SROTLEN,=Y(SROTLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A OPTIONS OBJECT IN MYOBJECT                                
***********************************************************************         
SETOPTS  NTR1                                                                   
         L     R6,AIO                                                           
         USING UOPTD,R6                                                         
         MVC   UOPTTYPE,=C'OPT*'                                                
         LH    R1,=Y(SOPTLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING SOPTD,R3                                                         
         MVC   SOPTLEN,=Y(SOPTLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A SCHEDULE OBJECT IN MYOBJECT                               
***********************************************************************         
SETSKD   NTR1                                                                   
         L     R6,AIO                                                           
         USING USKDD,R6                                                         
         MVC   USKDTYPE,=C'SKD*'                                                
         LH    R1,=Y(SSKDLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING SSKDD,R3                                                         
         MVC   SSKDLEN,=Y(SSKDLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A ORBIT OBJECT IN MYOBJECT                                  
***********************************************************************         
SETORB   NTR1                                                                   
         L     R6,AIO                                                           
         USING UORBD,R6                                                         
         MVC   UORBTYPE,=C'ORB*'                                                
         LH    R1,=Y(SORBLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING SORBD,R3                                                         
         MVC   SORBLEN,=Y(SORBLENQ) LENGTH OF OBJECT                            
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS A COMMENT OBJECT IN MYOBJECT                                
***********************************************************************         
SETCOM   NTR1                                                                   
         L     R6,AIO                                                           
         USING UCOMD,R6                                                         
         MVC   UCOMTYPE,=C'COM*'                                                
         LH    R1,=Y(SCOMLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING SCOMD,R3                                                         
         MVC   SCOMLEN,=Y(SCOMLENQ) L'OBJECT                                    
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A DEMO OBJECT IN MYOBJECT                                   
***********************************************************************         
SETDEM   NTR1                                                                   
         L     R6,AIO                                                           
         USING UDEMD,R6                                                         
         MVC   UDEMTYPE,=C'DEM*'                                                
*                                                                               
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   SDEM10               - NO, REGULAR                               
         LH    R1,=Y(SDM3LENQ)     LONGER LENGTH (BY2)                          
         B     *+8                                                              
SDEM10   LH    R1,=Y(SDEMLENQ)     LENGTH OF OBJECT                             
*                                                                               
         BAS   RE,SETINFO                                                       
*                                                                               
         USING SDEMD,R3                                                         
*** SBY2                                                                        
         TM    MISCFLG1,MF1SBY2    IS IT BY2 (20 DEMOS)?                        
         BNO   SDEM20               - NO, REGULAR                               
         MVC   SDEMLEN,=Y(SDM3LENQ) L'OBJECT  (BY2)                             
         B     SDEMX                                                            
*** SBY2                                    MHC 01/16/03                        
SDEM20   MVC   SDEMLEN,=Y(SDEMLENQ) L'OBJECT                                    
SDEMX    B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MOVES THE INFO TO THE OBJECT                                     
* R1 - L'OBJECT (W/O LENGTH)                                                    
* R6 - OBJECT                                                                   
* RETURN - R3 CURRENT OBJECT                                                    
***********************************************************************         
SETINFO  NTR1                                                                   
*                                                                               
         LA    R3,MYOBJECT         BUILD ADDITIONAL DATA OBJECT HERE            
         AH    R3,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
*                                                                               
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AR    RF,R1               PLUS L'THIS OBJECT                           
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT SO FAR                
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8              CLEAR ENOUGH AT END OF OBJECT                
         BE    *+10                                                             
         XC    0(0,R3),0(R3)                                                    
*                                                                               
         LH    RF,SUBNUM           INCREMENT SUB-RECORD NUMBER                  
         LA    RF,1(RF)                                                         
         STH   RF,SUBNUM                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  5(2,R3),DUB+4(4)                                                 
*                                                                               
         L     RF,DATALEN                                                       
         AH    RF,=H'4'            4 MORE FOR OBJECT IDENTIFIER                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R3),0(R6)                                                   
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
***********************************************************************         
* AT END OF BUY - PUT OBJECT TO TEMPSTR                                         
***********************************************************************         
SETEIN   NTR1                                                                   
         TM    MYSTAT,MYSDEL       IF THIS WAS A DELETE                         
         BZ    SN10                                                             
         LA    RE,MYOBJECT                                                      
         USING SBUYD,RE                                                         
         CLC   SBUYERNO,=H'53'                                                  
         BNE   SNX                 DEL OBJECT                                   
         DROP  RE                                                               
*                                                                               
SN10     LA    RE,MYOBJECT         BUILD END-OF-BUY OBJECT HERE                 
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
         USING SEBYD,RE                                                         
         XC    SEBYD(SEBYLENQ),SEBYD                                            
         MVC   SEBYLEN,=Y(SEBYLENQ) L'RECORD                                    
         MVC   SEBYSTRT(SEBYRLNQ),=C'EBY*'                                      
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,=Y(SEBYLENQ)     PLUS L'EBY                                   
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT                       
         DROP  RE                                                               
*                                                                               
         LH    R3,MYOBJLEN         SEND OBJECT TO TEMPSTR                       
         CH    R3,=Y(MYOBJLNQ)                                                  
         BNH   *+6                                                              
         DC    H'0'                NEED MORE SPACE TO BUILD OBJECT              
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
SNX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AT END OF ESTIMATE - CLEAN UP MINIO RECORD                                    
***********************************************************************         
SETEOE   NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SPUPELQ     X'90' LOOK FOR FIRST BUY ON EST              
         MVI   MINFILTL,1                                                       
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      ANY HISTORY FOR THIS ESTIMATE                
         BE    SE30                NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SE10     LA    R3,ELEMENT                                                       
         USING SPUPMEL,R3                                                       
         CLI   UPLOC,C'Y'          IF UPDATING BY LOCATION                      
         BNE   SE15                                                             
         CLC   SVLOCID,SPUPUUID    & THIS BUY IS NOT BY SAME LOCATION           
         BNE   SE25                IGNORE IT                                    
*                                                                               
SE15     TM    SPUPUSRC,X'40'      IS THIS MARKED FOR KEEPING?                  
         BZ    SE16                                                             
         NI    SPUPUSRC,X'FF'-X'40' TURN IT OFF (IT'S A WORK FLAG)              
         B     SE20                                                             
*                                                                               
SE16     TM    SPUPUSRC,X'80'      IS THIS MARKED FOR DELETION                  
         BO    SE17                                                             
         CLI   SVUPDFLG,C'Y'       UPDATES ONLY?                                
         BE    SE20                                                             
         OI    SPUPUSRC,X'80'      NO -- MARK IT FOR DELETION                   
*                                                                               
SE17     BAS   RE,PROCDEL          PUT DELETE OBJECTS TO TEMPSTR                
*                                                                               
SE20     MVI   WORK,SPUPELQ        SAVE ELEMENT KEY                             
         MVC   WORK+1(3),SPUPSTA                                                
         MVC   WORK+4(1),SPUPSLN                                                
         MVC   WORK+5(1),SPUPSEQ                                                
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(6),WORK     RESTORE ELEMENT KEY                          
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
SE25     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE BUYS FOR THIS ESTIMATE?             
         BE    SE30                NO                                           
         CLI   MINERR,0                                                         
         BE    SE10                                                             
         DC    H'0'                                                             
********                                                                        
* X'92' ELEMENTS                                                                
********                                                                        
SE30     XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SPU2ELQ     LOOK FOR FIRST BUY ON EST                    
         MVI   MINFILTL,1                                                       
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      ANY HISTORY FOR THIS ESTIMATE                
         BE    SE60                NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SE40     LA    R3,ELEMENT                                                       
         USING SPU2MEL,R3                                                       
         CLI   UPLOC,C'Y'          IF UPDATING BY LOCATION                      
         BNE   SE45                                                             
         CLC   SVLOCID,SPU2UUID    & THIS BUY IS NOT BY SAME LOCATION           
         BNE   SE55                IGNORE IT                                    
*                                                                               
SE45     TM    SPU2USRC,X'40'      IS THIS MARKED FOR KEEPING?                  
         BZ    SE46                                                             
         NI    SPU2USRC,X'FF'-X'40' TURN IT OFF (IT'S A WORK FLAG)              
         B     SE50                                                             
*                                                                               
SE46     TM    SPU2USRC,X'80'      IS THIS MARKED FOR DELETION                  
         BO    SE47                                                             
         CLI   SVUPDFLG,C'Y'       UPDATES ONLY?                                
         BE    SE50                                                             
         OI    SPU2USRC,X'80'      NO -- MARK IT FOR DELETION                   
*                                                                               
SE47     BAS   RE,PROCDEL          PUT DELETE OBJECTS TO TEMPSTR                
*                                                                               
SE50     MVI   WORK,SPU2ELQ        SAVE ELEMENT KEY                             
         MVC   WORK+1(3),SPU2STA                                                
         MVC   WORK+4(2),SPU2SLN                                                
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(6),WORK     RESTORE ELEMENT KEY                          
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
SE55     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE BUYS FOR THIS ESTIMATE?             
         BE    SE60                NO                                           
         CLI   MINERR,0                                                         
         BE    SE40                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
SE60     CLI   MINCHG,C'Y'         ANY CHANGE TO THIS MINIO RECORD?             
         BNE   SE80                                                             
         BAS   RE,CLSMIN                                                        
*                                                                               
SE80     LA    RE,MYOBJECT         BUILD END-OF-EST OBJECT HERE                 
         USING SEOEODAT,RE                                                      
         XC    SEOEODAT(SEOELENQ),SEOEODAT                                      
         MVC   SEOETYPE,=C'EOE*'                                                
         DROP  RE                                                               
         LH    R3,=Y(SEOELENQ)     SEND END-OF-EST OBJECT TO TEMPSTR            
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
SEX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PUT DELETE OBJECTS TO TEMPSTR FOR THE SPOT BUY PROGRAM                        
***********************************************************************         
PROCDEL  NTR1                                                                   
*                                                                               
         LA    R3,ELEMENT          MINIO ELEMENT BEING DELETED                  
         CLI   0(R3),SPUPELQ       X'90' ELEM?                                  
         BNE   PD10                                                             
         CLI   SPUPSLN-SPUPMEL(R3),0      IGNORE IF NOT FOR A BUYLINE           
         BE    PDX                                                              
         B     PD20                                                             
*                                                                               
PD10     TM    SPU2SLN-SPU2MEL(R3),X'80'  IGNORE IF NOT FOR A BUYLINE           
         BNZ   PDX                                                              
*                                                                               
PD20     LA    R2,MYOBJECT         BUILD DELETE OBJECT HERE                     
         USING SDELODAT,R2                                                      
         XC    SDELODAT(SDELLENQ),SDELODAT                                      
         MVC   SDELTYPE,=C'DEL*'                                                
*                                                                               
         LA    R1,WORK                                                          
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SVAGY                                                    
         MVC   STAPMED,SVMED                                                    
         MVC   STAPACOM,ACOMFACS                                                
*                                                                               
         MVC   STAPSTA,SPUPSTA-SPUPMEL(R3)                                      
         CLI   0(R3),SPUPELQ       X'90' ELEMENT?                               
         BE    *+10                                                             
         MVC   STAPSTA,SPU2STA-SPU2MEL(R3)  NO, X'92' ELEM                      
         GOTO1 STAPACK,(R1)                                                     
*                                                                               
         MVC   SDELSTA,STAPQSTA                                                 
         CLI   SDELSTA,C'0'        CABLE STATION?                               
         BL    *+8                                                              
         MVI   SDELSTA+4,C'/'      PUT A SLASH BETWEEN SYSTEM AND NETWK         
         DROP  R1                                                               
*                                                                               
         CLI   0(R3),SPUPELQ       X'90' ELEMENT?                               
         BNE   PD30                                                             
         USING SPUPMEL,R3                                                       
         MVC   SDELLIN,SPUPSLN                                                  
         MVC   SDELBNUM,SPUPBNUM   LINE NUMBER IN FILE                          
         B     PD40                                                             
*                                                                               
         USING SPU2MEL,R3                                                       
PD30     MVC   SDELLIN2,SPU2SLN                                                 
         MVC   SDELBNUM,SPU2BNUM   LINE NUMBER IN FILE                          
*                                                                               
PD40     LH    R3,=Y(SDELLENQ)     SEND DELETE OBJECT TO TEMPSTR                
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
PDX      B     XIT                                                              
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS THE BUY PROGRAM WITH THE TRANSFER CONTROL ELEMENT          
* VIA GLOBBER.                                                                  
***********************************************************************         
CALLBUY  NTR1                                                                   
*                                                                               
         XC    BLOCK(24),BLOCK                                                  
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'CON'    FROM THE CONTROL SYSTEM                      
         MVC   GLVXFRPR,=C'MAD'    MAD PROGRAM                                  
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'BUY'    BUY PROGRAM                                  
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         MVC   MDACTION,=Y(ER10XCTL) GLOBBER COULDN'T PUT OUT XCTL ELEM         
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE UPLOADED BUYS TO THE PC.               
***********************************************************************         
CONFIRM  NTR1                                                                   
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   CONFFLAG,X'FF'      IF CONFIRM WAS USED BEFORE                   
         BE    CNF10               THEN PUT OUT CONFIRMATION OBJECT             
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'GTX'),MYOBJLNQ,(3,0)                       
         BNE   EXIT                                                             
*                                                                               
CNFLOOP  GOTO1 GETTMP,DMCB,MYOBJGET GET BUY DATA FROM TEMPSTR                   
         BNE   EXIT                                                             
*                                                                               
         MVC   DATALEN,DMCB+4      COPY LENGTH OF DATA                          
*                                                                               
         CLI   EOTFLAG,C'Y'        DONE IF NO MORE                              
         BNE   CNF10                                                            
*                                                                               
         CLI   ENDOBJCT,C'Y'       END-OF-DATA OBJECT?                          
         BE    CNF05                                                            
*                                  PUT END-OF-BLOCK OBJECT OUT                  
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
*                                                                               
CNF05    MVI   ENDBLOCK,C'N'                                                    
         MVI   CONFFLAG,0                                                       
         GOTO1 TMPCLOSE            DONE WITH READING TEMPFILE                   
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
*                                                                               
         CLI   MINCHG,C'Y'         ANY CHANGE                                   
         BNE   *+8                                                              
         BAS   RE,CLSMIN                                                        
         B     CNFX                EXIT ROUTINE                                 
*                                                                               
CNF10    LA    R4,MYOBJGET         RETURNED OBJECT                              
         CLC   =C'HDR*',8(R4)      HEADER OBJECT?                               
         BNE   CNF20                                                            
         BAS   RE,CNFHDR                                                        
         B     CNFCHK                                                           
*                                                                               
CNF20    CLC   =C'BUY*',10(R4)     BUY OBJECT?                                  
         BNE   CNF30                                                            
         BAS   RE,CFNBUY                                                        
         B     CNFCHK                                                           
*                                                                               
CNF30    CLC   =C'DEL*',8(R4)      DELETE OBJECT?                               
         BNE   CNF40                                                            
         BAS   RE,CNFDEL                                                        
         B     CNFCHK                                                           
*                                                                               
CNF40    CLC   =C'EOE*',8(R4)      END-OF-EST OBJECT?                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MINCHG,C'Y'         ANY CHANGE TO THIS ESTIMATE                  
         BNE   CNFCHK                                                           
         BAS   RE,CLSMIN                                                        
         B     CNFCHK                                                           
*                                                                               
CNFCHK   CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG NOT SET,                
         BNE   CNFLOOP             LOOP BACK UNTIL NO MORE OBJECTS              
         MVI   CONFFLAG,X'FF'      WE HAVE SOMETHING IN OUR BUFFER              
*                                                                               
CNFX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE BUY OBJECT                             
***********************************************************************         
         USING SBUYD,R4                                                         
CFNBUY   NTR1                                                                   
         XC    SUBNUM,SUBNUM       SUB RECORD NUMBER                            
*                                                                               
         OC    SBUYLIN2,SBUYLIN2   DO WE HAVE A 2 BYTE BUYLINE #?               
         BZ    *+14                                                             
         MVC   SVLINE,SBUYLIN2     SAVE LINE NUMBER FROM SPOTPAK                
         B     CBUY05                                                           
         XC    SVLINE,SVLINE                                                    
         MVC   SVLINE+1(1),SBUYLINE   SAVE LINE NUMBER FROM SPOTPAK             
*                                                                               
CBUY05   MVC   BUYNUM,SBUYRNUM     BUY NUMBER                                   
         MVC   SEQNUM,SBUYSEQ      SEQUENCE NUMBER (FROM $MAD)                  
         MVC   SVACT,SBUYACTN      ACTION                                       
         MVC   SVUNIQ,SBUYUID      USER-SUPPLIED UNIQUE ID                      
*                                                                               
         LA    R1,WORK                                                          
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,SVAGY                                                    
         MVC   STAPMED,SVMED                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,SBUYSTA                                                 
*                                                                               
         CLI   STAPQSTA,C'0'       IS THIS CABLE - CONTINUE                     
         BL    *+14                                                             
         MVC   STAPQSTA(L'SBUYSTA),SBUYSTA                                      
         B     CBUY10                                                           
*                                                                               
         CLI   STAPQSTA+4,C' '     ELSE MAKE SURE BAND IS SET                   
         BH    CBUY10                                                           
         MVC   STAPQSTA+4(1),SVMED                                              
*                                                                               
CBUY10   GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0           ANY ERROR FROM STAPACK?                      
         BE    CBUY12              NOT IF SVSTA IS BINARY ZEROS                 
         MVC   SBUYERNO,=H'269'                                                 
         MVI   SBUYERF,1                                                        
         B     CBUYERR                                                          
*                                                                               
CBUY12   MVC   SVSTA,STAPSTA                                                    
         DROP  R1                                                               
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY+1(3),SVSTA  STATION                                      
*                                                                               
         MVI   MINEKEY,SPUPELQ     ALWAYS LOOK FOR X'90' FIRST                  
         CLI   SVACT,C'C'          WAS THIS A CHANGE?                           
         BNE   CBUY14                                                           
         CLI   SVLINE,0            1-BYTE BUYLINE?                              
         BNE   CBUY30              NO, THEN IT HAS TO BE X'92'                  
         MVC   MINEKEY+4(1),SVLINE+1   YES - SET SPOTPAK LINE NUMBER            
         B     CBUY20                                                           
*                                                                               
CBUY14   CLI   SVACT,C'D'          DELETING & RECORD NOT FOUND?                 
         BNE   CBUY16                                                           
         CLC   SBUYERNO,=H'53'                                                  
         BE    CBUYERR             YES, DON'T LOOK FOR IT IN MINIO              
*                                                                               
CBUY16   MVC   MINEKEY+5(1),SEQNUM ELSE NUMBER ASSIGNED BY $MAD OVERLAY         
*                                                                               
CBUY20   BAS   RE,READMIN                                                       
         BE    CBUY45                                                           
********                                                                        
* NO X'90' ELEMENT FOR WHAT WE'RE LOOKING FOR, TRY X'92'                        
********                                                                        
CBUY30   MVI   MINEKEY,SPU2ELQ      X'92'                                       
*                                                                               
         CLI   SVACT,C'C'           WAS THIS A CHANGE?                          
         BNE   CBUY33                                                           
         MVC   MINEKEY+4(2),SVLINE  YES - SET SPOTPAK LINE NUMBER               
         B     CBUY40                                                           
*                                                                               
CBUY33   CLI   SVACT,C'D'          DELETING & RECORD NOT FOUND?                 
         BNE   CBUY36                                                           
         CLC   SBUYERNO,=H'53'                                                  
         BE    CBUYERR             YES, DON'T LOOK FOR IT IN MINIO              
*                                                                               
CBUY36   MVI   MINEKEY+4,X'80'     SEQUENCE # FOR X'92' HAS X'80' BIT           
         MVC   MINEKEY+5(1),SEQNUM   ON                                         
*                                                                               
CBUY40   BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                DIDN'T WE ADD/SEE THIS EARLIER?              
*                                                                               
CBUY45   OC    SBUYERNO,SBUYERNO   ANY ERROR IN BUY* RECORD?                    
         BZ    CBUY60                                                           
*                                                                               
CBUYERR  MVC   ERRNUM,SBUYERNO     SAVE ERROR NUMBER                            
         MVC   ERFLDNUM,SBUYERF    SAVE FIELD NUMBER IN ERROR                   
         CLI   SVACT,C'A'          DID WE ADD THIS EARLIER?                     
         BNE   CBUY90                                                           
         OC    SVLINE,SVLINE       DO WE HAVE A LINE NUMBER                     
         BZ    CBUY50                                                           
         BAS   RE,CNFBADD          YES, 1ST PUT CONFIRM BUY WAS ADDED           
         B     CBUY90              THEN WRITE ERROR OBJECT                      
*                                                                               
CBUY50   BAS   RE,READMIN          CHECK TO MAKE SURE IT IS HERE 1ST            
         BNE   CBUY90                OTHERWISE RECUP WILL DIE                   
         BAS   RE,DELMIN           DELETE MINIO ELEMENT                         
         B     CBUY90              WRITE ERROR OBJECT                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
CBUY60   DS    0H                                                               
         BAS   RE,CNFBADD          PUT OUT CONFIRM THAT BUY WAS ADDED           
         BNE   CBX                                                              
*                                                                               
CBUY70   SR    R0,R0                                                            
         ICM   R0,3,0(R4)          LENGTH OF SUB-OBJECT                         
         AR    R4,R0               BUMP TO NEXT PORTION                         
         LA    R4,2(R4)            BUMP PAST LENGTH                             
*                                                                               
         CLC   =C'ROT*',10(R4)                                                  
         BE    CBUY80                                                           
         CLC   =C'SKD*',10(R4)                                                  
         BE    CBUY80                                                           
         CLC   =C'ORB*',10(R4)                                                  
         BE    CBUY80                                                           
         CLC   =C'COM*',10(R4)                                                  
         BE    CBUY80                                                           
         CLC   =C'DEM*',10(R4)                                                  
         BE    CBUY80                                                           
         CLC   =C'EBY*',10(R4)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SVACT,C'A'                                                       
         BE    CBX                 STATUS FOR ADDED WAS ALREADY SENT            
         BAS   RE,CNFEIN                                                        
         L     R3,=A(ITSBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         B     CBUYPUT                                                          
*                                                                               
CBUY80   OC    2(2,R4),2(R4)       ANY ERROR IN OBJECT?                         
         BZ    CBUY70              NO                                           
*                                                                               
CBUY90   MVC   SUBNUM,5(R4)        SUBRECORD NUMBER                             
         MVC   ERFLDNUM,4(R4)      FIELD NUMBER                                 
         MVC   ERRNUM,2(R4)        ERROR NUMBER                                 
         BAS   RE,SETERR           SET ERROR                                    
         B     CBX                                                              
*                                                                               
CBUYPUT  GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
CBX      B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS A STATUS OBJECT THAT IS IN ERROR                            
***********************************************************************         
SETERR   NTR1                                                                   
         LA    R2,WORK             BUILD ERROR OBJECT                           
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         SR    R0,R0                                                            
         ICM   R0,3,BUYNUM         BUY NUMBER                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTARECN,DUB+4(4)                                                
         MVC   DSTASUBR,SUBNUM                                                  
*                                                                               
         ZIC   R0,ERFLDNUM         FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,ERRNUM         ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
*                                                                               
         L     R3,=A(ITSBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
         B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE DEL OBJECT                             
***********************************************************************         
         USING SDELODAT,R4                                                      
CNFDEL   NTR1                                                                   
         LA    R1,WORK                                                          
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,SVAGY                                                    
         MVC   STAPMED,SVMED                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,SDELSTA                                                 
*                                                                               
         CLI   STAPQSTA,C'0'       IS THIS CABLE?                               
         BL    *+14                                                             
         MVC   STAPQSTA(L'SDELSTA),SDELSTA   THEN NEED THE NETWORK ALSO         
         B     CD10                                                             
*                                                                               
         CLI   STAPQSTA+4,C' '     ELSE MAKE SURE BAND IS SET                   
         BH    CD10                                                             
         MVC   STAPQSTA+4(1),SVMED                                              
*                                                                               
CD10     GOTO1 STAPACK,(R1)                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY+1(3),STAPSTA                                             
         DROP  R1                                                               
*                                                                               
         MVI   MINEKEY,SPUPELQ     X'90'                                        
         MVC   MINEKEY+4(1),SDELLIN                                             
         OC    SDELLIN2,SDELLIN2                                                
         BZ    CD15                                                             
         CLI   SDELLIN2,0          BUYLINE > 255?                               
         BNE   CD20                                                             
CD15     BAS   RE,READMIN                                                       
         BE    CD30                                                             
*                                                                               
CD20     MVI   MINEKEY,SPU2ELQ     X'92'                                        
         MVC   MINEKEY+4(2),SDELLIN2                                            
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                DIDN'T WE ADD THIS EARLIER?                  
*                                                                               
CD30     OC    SDELERNO,SDELERNO   ANY ERROR ON DELETE?                         
         BZ    CD50                                                             
         CLC   SDELERNO,=H'53'     RECORD NOT FOUND ON RECALL OF LINE?          
         BE    CD50                YES, THEN DELETE ELEMENT                     
         LA    R3,ELEMENT                                                       
*                                                                               
         CLI   0(R3),SPUPELQ                                                    
         BNE   CD35                                                             
         USING SPUPMEL,R3                                                       
         NI    SPUPUSRC,X'FF'-X'80' YES -- TAKE OFF DELETE BIT                  
         OI    SPUPUSRC,X'40'      KEEP THIS BUY                                
         B     CD40                                                             
*                                                                               
         USING SPU2MEL,R3                                                       
CD35     NI    SPU2USRC,X'FF'-X'80' YES -- TAKE OFF DELETE BIT                  
         OI    SPU2USRC,X'40'      KEEP THIS BUY                                
         DROP  R3                                                               
*                                                                               
CD40     BAS   RE,WRTMIN                                                        
         BNE   CDELX                                                            
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         PACK  DUB,SDELBNUM        BUY RECORD NUMBER IN FILE                    
         CVB   R1,DUB                                                           
         STCM  R1,3,BUYNUM                                                      
         MVC   ERRNUM,SDELERNO     SAVE ERROR NUMBER                            
         MVC   ERFLDNUM,SDELERF    FIELD NUMBER                                 
         XC    SUBNUM,SUBNUM       SUB RECORD NUMBER                            
         BAS   RE,SETERR           SET ERROR                                    
         B     CDELX                                                            
*                                                                               
CD50     BAS   RE,DELMIN                                                        
*                                                                               
         L     R1,TOTALDEL         INCREMENT DELETE COUNTER                     
         LA    R1,1(R1)                                                         
         ST    R1,TOTALDEL                                                      
*                                                                               
CDELX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE HDR OBJECT                             
***********************************************************************         
         USING SHDRODAT,R4                                                      
CNFHDR   NTR1                                                                   
         OC    SHDRERNO,SHDRERNO   ANY ERROR ON HEADER?                         
         BZ    CHDRX               NO                                           
*                                                                               
         LA    R2,WORK             YES -- BUILD ERROR OBJECT                    
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         MVC   DSTARECN,=C'00000'  RECORD ZERO = HEADER                         
         MVC   DSTASUBR,=C'00'     NO SUB-RECORD                                
         ZIC   R0,SHDRERF          FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
         SR    R0,R0                                                            
         ICM   R0,3,SHDRERNO       ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
         DROP  R2                                                               
                                                                                
         L     R3,=A(ITSBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
                                                                                
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
                                                                                
CHDRX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CONFIRM IF BUY ADDED                                                          
***********************************************************************         
CNFBADD  NTR1                                                                   
         LA    R2,WORK             BUY WAS OK                                   
         USING DSTATUSD,R2                                                      
         CLI   SVACT,C'A'                                                       
         BNE   CADDX                                                            
*                                                                               
         MVC   DSTATYPE,=C'03'     RECORD ADDED STATUS                          
         LA    R1,ELEMENT                                                       
         CLI   0(R1),SPUPELQ       OLD INSERTION ELEM?                          
         BNE   CADD10                                                           
         USING SPUPMEL,R1                                                       
         MVC   SPUPSLN,SVLINE+1    PUT SPOTPAK LINE NUMBER IN ELEMENT           
         MVI   SPUPSEQ,0           REMOVE THE $MAD SEQUENCE NUMBER              
*                                                                               
         CLI   SVLINE,0            2 BYTE LINE NUMBER?                          
         BE    CADD20                                                           
         USING SPU2MEL,R1                                                       
CADD10   MVI   SPU2MEL,SPU2ELQ     YES                                          
         MVC   SPU2SLN,SVLINE                                                   
         DROP  R1                                                               
*                                                                               
CADD20   BAS   RE,WRTMIN                                                        
         BNE   NO                                                               
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BUYNUM         BUY NUMBER                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTARECN,DUB+4(4)                                                
         L     R3,=A(ITSBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
CADDX    B     YES                 RETURN TO THE CALLER                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CONFIRM END OF BUY OBJECT                                                     
***********************************************************************         
CNFEIN   NTR1                                                                   
         LA    R2,WORK             BUY WAS OK                                   
         USING DSTATUSD,R2                                                      
         CLI   SVACT,C'C'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSTATYPE,=C'02'     RECORD MODIFIED STATUS                       
         SR    R0,R0                                                            
         ICM   R0,3,BUYNUM         BUY NUMBER                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTARECN,DUB+4(4)                                                
*                                                                               
CEINX    B     XIT                 RETURN TO THE CALLER                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
GETTITEM NTR1                                                                   
*                                                                               
         SR    R2,R2                                                            
         CLI   TSTDATAF,C'1'                                                    
         BNE   *+12                                                             
         LA    R3,TSTX1                                                         
         B     GT10                                                             
         CLI   TSTDATAF,C'2'                                                    
         BNE   *+12                                                             
         LA    R3,TSTX2                                                         
         B     GT10                                                             
         DC    H'0'                                                             
*                                                                               
GT10     CLI   0(R3),X'FF'                                                      
         BNE   *+14                                                             
         MVC   TYPENUM,=A(ITEOD)                                                
         B     XIT                                                              
*                                                                               
         CLI   0(R3),X'80'                                                      
         BNE   *+16                                                             
         ICM   R2,3,1(R3)                                                       
         LA    R3,3(R2,R3)                                                      
         B     GT10                                                             
*                                                                               
         CLC   =C'NOP*',3(R3)                                                   
         BNE   GT20                                                             
         MVI   0(R3),X'80'                                                      
         ICM   R2,3,1(R3)                                                       
         LA    R3,3(R2,R3)                                                      
         B     GT10                                                             
*                                                                               
GT20     MVC   TYPENUM,=A(ITSBYHDR)                                             
         CLC   =C'HDR*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYBUY)                                             
         CLC   =C'BUY*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYROT)                                             
         CLC   =C'ROT*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYSKD)                                             
         CLC   =C'SKD*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYORB)                                             
         CLC   =C'ORB*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYCOM)                                             
         CLC   =C'COM*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYDEM)                                             
         CLC   =C'DEM*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYEBY)                                             
         CLC   =C'EBY*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITSBYEOE)                                             
         CLC   =C'EOE*',3(R3)                                                   
         BE    GTX                                                              
         DC    H'0'                                                             
*                                                                               
GTX      SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         SH    R0,=H'4'                                                         
         ST    R0,DATALEN                                                       
         MVI   0(R3),X'80'                                                      
         LA    R3,7(R3)                                                         
         ST    R3,ADATA                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        MINIO I/O ROUTINES                                                     
*                                                                               
***********************************************************************         
* THIS ROUTINE READ HIGH FOR A MINIO ELEMENT                                    
***********************************************************************         
         USING MINBLKD,R5                                                       
HIGHMIN  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         B     YES                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL A MINIO ELEMENT                                 
***********************************************************************         
SEQMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     YES                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT                                            
***********************************************************************         
READMIN  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    YES                 YES EXIT                                     
         B     NO                                                               
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT                                             
***********************************************************************         
ADDMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BNE   ERRMIN                                                           
         B     YES                 CONTINUE                                     
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE CLOSES A MINIO RECORD                                            
***********************************************************************         
CLSMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BNE   ERRMIN                                                           
         B     YES                 CONTINUE                                     
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE WRITES A MINIO ELEMENT                                           
***********************************************************************         
WRTMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BNE   ERRMIN                                                           
         B     YES                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT                                          
***********************************************************************         
DELMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BNE   ERRMIN                                                           
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     YES                                                              
*                                                                               
ERRMIN   DS    0H                  ERROR IN MINIO                               
*****    MVC   ERFLDNUM,=AL2(UPLMINER)     MINIO ERROR IN UPLOAD                
         MVI   ERFLDNUM,0          FIELD NOT IN ERR BECAUSE IT'S MINIO          
         MVI   ERRNUM,0            SET MINIO ERROR NUMBER IN ERRNUM             
         MVC   ERRNUM+1(L'MINERR),MINERR                                        
         XC    SUBNUM,SUBNUM       SUB RECORD NUMBER                            
         BAS   RE,SETERR           SET ERROR                                    
         B     NO                                                               
         EJECT                                                                  
EXIT     L     RD,SAVEDRD                                                       
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
XFF      DC    64X'FF'                                                          
SPACES   DC    64C' '                                                           
         SPACE 1                                                                
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPGETBUY - INITIALIZE                                                         
*                                                                               
* ON ENTRY:    AGENCY              SET TO THE 2 CHAR AGENCY ALPHA               
*                                                                               
* ON EXIT:     SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
***********************************************************************         
GBUYINIT NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT      INIT CALL                                    
         MVC   GBYAGY,SVAGY                                                     
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
         MVC   SV1OR2,GBY1OR2                                                   
***************                                                                 
* SPECIAL FOR SJR                                                               
***************                                                                 
         CLC   SVAGY,=C'SJ'                                                     
         BNE   YES                                                              
         CLC   SVCLT,=C'TBL'       ONLY CLT TBL ON SJR HAS 2 BYTES              
         BNE   YES                                                              
         MVI   SV1OR2,2            HARD CODE THIS                               
         J     YES                                                              
***********************************************************************         
* SPGETBUY - READ HIGH                                                          
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*              KEY                 SET TO BUY KEY YOU WANT                      
*                                                                               
* ON EXIT:     KEY                 SET TO THE KEY YOU READ                      
*              KEYSAVE             SAVED OF WHAT YOU WANTED                     
***********************************************************************         
GBUYHIGH NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYHIGH      READ HIGH FOR GBKEST/LIN                     
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    R1,KEY                                                           
         ST    R1,GBYKEYIN                                                      
         MVC   KEYSAVE,KEY         "GOTO1 HIGH" KEY GETS OVERWRITTEN            
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
*                                                                               
         LA    R1,KEY+BUYKDA-BUYKEY                                             
         MVC   SVBUYDA,0(R1)                                                    
         J     YES                                                              
***********************************************************************         
* SPGETBUY - READ SEQ                                                           
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*              KEY                 SET TO BUY KEY YOU WANT                      
*                                                                               
* ON EXIT:     KEY                 SET TO THE KEY YOU READ                      
*              KEYSAVE             SAVED OF WHAT YOU WANTED                     
***********************************************************************         
GBUYRSEQ NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYSEQ       READ SEQUENTIAL                              
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    R1,KEY                                                           
         ST    R1,GBYKEYIN                                                      
         MVC   KEYSAVE,KEY         "GOTO1 SEQ" KEY GETS OVERWRITTEN             
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
*                                                                               
         LA    R1,KEY+BUYKDA-BUYKEY                                             
         MVC   SVBUYDA,0(R1)                                                    
         J     YES                                                              
***********************************************************************         
* SPGETBUY - GETREC                                                             
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*                                                                               
* ON EXIT:                                                                      
***********************************************************************         
GBUYGETR NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYGET                                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    RE,SVBUYDA                                                       
         ST    RE,GBYDA            GBYDA IS THE A(D/A)                          
         L     R1,AIO                                                           
         ST    R1,GBYIOA                                                        
         LA    R1,DMWORK                                                        
         ST    R1,GBYDMWRK                                                      
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
         J     YES                                                              
***********************************************************************         
* SPGETBUY - PUTREC                                                             
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*              SVBUYDA                                                          
* ON EXIT:                                                                      
***********************************************************************         
GBUYPUTR NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYPUT                                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    RE,SVBUYDA                                                       
         ST    RE,GBYDA            GBYDA IS THE A(D/A)                          
         L     R1,AIO                                                           
         ST    R1,GBYIOA                                                        
         LA    R1,DMWORK                                                        
         ST    R1,GBYDMWRK                                                      
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
         J     YES                                                              
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTMAD18TST                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTMADFFD                                                       
         EJECT                                                                  
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPTUPLOADD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENUPL                                                       
         SPACE 3                                                                
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* CTMADEQUS                                                                     
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE CTMADEQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYHDRD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
APRELO   DS    A                                                                
APBASE1  DS    A                                                                
APBASE2  DS    A                                                                
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
GLOBBER  DS    A                   A(GLOBBER)                                   
MINIO    DS    A                   A(MINIO)                                     
RECUP    DS    A                   A(RECUP)                                     
MEDGET   DS    A                   A(MEDGET)                                    
AWORKSTR DS    A                   A(UNSAVED WORKING STORAGE)                   
AMINBLK  DS    A                   A(MINIO PARAMETER BLOCK)                     
AMINBUFF DS    A                   A(MINIO I/O BUFFERS)                         
AMINRECT DS    A                   A(MINIO RECORD TABLE)                        
AGETBUY  DS    A                   A(GETBUY)                                    
*                                                                               
SVBUYDA  DS    A                   SAVED BUY D/A                                
*                                                                               
TOTALDEL DS    F                   TOTAL NUMBER BUYS DELETED                    
*                                                                               
BUYNUM   DS    H                   BUY NUMBER                                   
SUBNUM   DS    H                   SUB-RECORD NUMBER                            
MYOBJLEN DS    H                   LENGTH OF OBJECTS I BUILD                    
ERRNUM   DS    H                   ERROR NUMBER                                 
*                                                                               
CONFFLAG DS    X                                                                
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAG                           
MF1SBY2  EQU   X'80'                - IT'S THE NEW SBY2 TYPE (20 DEMOS)         
MF190ELM EQU   X'40'                - WE STILL HAVE X'90' ELEMENTS              
*                                                                               
TSTDATAF DS    CL1                 WHICH TEST RECORDS TO USE                    
ENDOBJCT DS    CL1                 END OF OBJECTS FLAG                          
ENDBLOCK DS    CL1                 END OF BLOCK FLAG                            
ERFLDNUM DS    X                   FIELD NUMBER IN ERROR                        
TMPOPEND DS    CL1                 'Y' = TEMPSTR IS OPEN                        
MINCHG   DS    CL1                 'Y' = MINIO RECORD CHANGED                   
UPLOC    DS    CL1                 'Y' = UPLOAD BY LOCATION                     
SVLOCID  DS    CL2                 LOCATION ID                                  
**SVLINE   DS    X                   SPOTPAK LINE NUMBER                        
SVLINE   DS    XL2                 SPOTPAK 2-BYTE BUYLINE #                     
SVACT    DS    CL1                 ACTION CODE                                  
SVHNOSP  DS    CL1                 FLAG - NO SPOTS NOT CONSIDERED ERROR         
SENTHDR  DS    CL1                 HEADER WAS SENT                              
*                                                                               
MYSTAT   DS    X                   INTERNAL STATUS BYTE                         
MYSDEL   EQU   X'80'               THIS IS A DELETE                             
*                                                                               
SVSYS    DS    CL1                 SYSTEM                                       
SVTYPE   DS    CL3                 UPLOAD TYPE                                  
SVAGY    DS    CL2                 AGENCY                                       
SV1OR2   DS    XL1                 FROM SPGETBUY WHETHER AGY USE 1 OR 2         
SVMED    DS    CL1                 MEDIA                                        
SVBAM    DS    X                   AGY/MED                                      
SVCLT    DS    CL3                 CLIENT                                       
SVBCLT   DS    XL2                 CLIENT (PACKED)                              
SVPRD    DS    CL3                 PRODUCT                                      
SVPRD2   DS    CL3                 PRODUCT 2                                    
SVBPRD   DS    XL1                 PRODUCT (BINARY)                             
SVEST    DS    XL2                 ESTIMATE                                     
SVSTA    DS    XL3                 LAST STATION FROM INPUT FILE                 
SVUNIQ   DS    CL8                 UNIQUE BUY ID                                
SVWRTFLG DS    CL1                 WRITE ENABLE FLAG                            
SVUPDFLG DS    CL1                 UPDATES ONLY FLAG                            
**LASTSTA  DS    XL3                 LAST STA RETRIEVED FROM INPUT FILE         
SEQNUM   DS    X                   SEQUENCE NUMBER (FOR TIES ON DATE)           
SVAPRF14 DS    C                   AGENCY PROF+14, SAVE BYR/BLR                 
SVHDROBJ DS    CL(SHD2LENQ)        SAVED HEADER OBJECT                          
*                                                                               
MYOBJGET DS    (MYOBJLNQ)CL1       GET A TWA OBJECT HERE                        
*                                                                               
MYOBJECT DS    (MYOBJLNQ)CL1       BUILD A TWA OBJECT HERE                      
MYOBJLNQ EQU   1800                MAXIMUM OBJECT LENGTH                        
*                                                                               
         DS    0F                                                               
GETBLK   DS    0XL(GETBUYL)                                                     
       ++INCLUDE SPGETBUYD                                                      
*                                                                               
         DS    (4096-(*-OVERD))X   $MAD CONTROLLER ONLY SAVES 4K                
         EJECT                                                                  
*                   ELEMENTS                                                    
*                                                                               
UHDRD    DSECT                                                                  
UHDRSYS  DS    CL1                 SYSTEM (ALWAYS C'S')                         
UHDRUTYP DS    CL3                 UPLOAD TYPE (ALWAYS 'BUY')                   
*                                  (NOT ALWAYS C'BUY', COULD BE C'BY2')         
*                                  NEW TYPE FOR 20 DEMOS   MHC 01/16/03         
UHDRAGID DS    CL2                 BUYING AGENCY ID                             
UHDRMED  DS    CL1                 MEDIA CODE                                   
UHDRCLT  DS    CL3                 CLIENT CODE                                  
UHDRPRD  DS    CL3                 PRODUCT CODE                                 
UHDRPRD2 DS    CL3                 PRODUCT 2 CODE                               
UHDREST  DS    CL3                 ESTIMATE NUMBER                              
UHDRSTPD DS    CL6                 PERIOD START                                 
UHDRENPD DS    CL6                 PERIOD END                                   
UHDRDEMO DS    14CL7               DEMOS                                        
UHDRDAIL DS    CL1                 DAILY FLAG                                   
UHDRBUYR DS    CL8                 BUYER ID                                     
UHDRUPDO DS    CL1                 UPDATES ONLY? (Y/N)                          
UHDRUID2 DS    CL2                 LOCATION ID-1ST 2 CHARS UNIQUE ID            
UHDRNOSP DS    CL1                 ALLOW NO SPOTS                               
         DS    CL9                 SPARE                                        
UHDRTSTR DS    CL1                 TEST RUN - DO NOT UPDATE (Y/N)               
UHDRTSTB DS    CL1                 TEST BUYS (Y/N)                              
UHDRRATE DS    CL1                 RATE SOURCE -- NOT USED FOR SILO             
*                                                                               
*        THE FOLLOWING IS FOR SBY2 ONLY!!!                                      
*        NEEDED 20 DEMOS INSTEAD OF 14 FOR THEM CANADIANS                       
*                                             MHC  01/16/03                     
*                                                                               
         ORG   UHDRDEMO                                                         
UHD2DEMO DS    20CL7               DEMOS                                        
UHD2DAIL DS    CL1                 DAILY FLAG                                   
UHD2BUYR DS    CL8                 BUYER ID                                     
UHD2UPDO DS    CL1                 UPDATES ONLY? (Y/N)                          
UHD2UID2 DS    CL2                 LOCATION ID-1ST 2 CHARS UNIQUE ID            
UHD2NOSP DS    CL1                 ALLOW NO SPOTS                               
         DS    CL9                 SPARE                                        
UHD2TSTR DS    CL1                 TEST RUN - DO NOT UPDATE (Y/N)               
UHD2TSTB DS    CL1                 TEST BUYS (Y/N)                              
UHD2RATE DS    CL1                 RATE SOURCE -- NOT USED FOR SILO             
         SPACE 3                                                                
*                                                                               
UBUYD    DSECT                                                                  
UBUYTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'BUY*')                 
UBUYSTA  DS    CL8                 STATION                                      
UBUYROT  DS    CL7                 ROTATION                                     
UBUYRDAY DS    CL1                 ROTATION START DAY                           
UBUYSTIM DS    CL4                 START TIME                                   
UBUYETIM DS    CL4                 END TIME                                     
UBUYDPT  DS    CL1                 DAYPART                                      
UBUYSLEN DS    CL3                 TOTAL SPOT LENGTH                            
UBUYLUNT DS    CL1                 LENGTH UNITS (S - SEC, M - MIN)              
UBUYPROG DS    CL20                PROGRAM NAME                                 
UBUYCOST DS    CL9                 (N - 2 DECIMALS IMPLIED)                     
UBUYCQLF DS    CL1                 COST QUALIFIER - SPECIAL RATE CODE           
UBUYMAS  DS    CL3                 MASTER PRODUCT                               
UBUYMAS2 DS    CL3                 MASTER PRODUCT 2 (PIGGYBACK)                 
UBUYP1SH DS    CL3                 PRD 1 TIME SHARE (N - LEN UNITS) FOR         
*                                  PIGGYBACKS ONLY                              
UBUYP1CS DS    CL5                 PRD 1 COST SHARE (N - % OF COST PD           
*                                  BY PRD 1 - 2 DEC IMPLIED)                    
UBUYUID  DS    CL8                 UNIQUE BUY ID                                
UBUYDEL  DS    CL1                 DELETE FLAG (Y/N|)                          
UBUYNUM  DS    CL5                 BUY NUMBER                                   
         DS    CL14                SPARE                                        
UBUYLENQ EQU   *-UBUYD             L'OBJECT FOR $BUY                            
*                                                                               
*        THE FOLLOWING IS FOR SBY2 ONLY!!!                                      
*        NEEDED A NEW UNIQUE ID FIELD FOR THEM CANADIANS                        
*                                             MHC  01/16/03                     
*                                                                               
         ORG   UBUYNUM+SBYSPARE-SADJCODE   REFER TO SPTUPLOADD!!                
UBY2UID  DS    CL12                UNIQUE ID FOR SBY2 ONLY                      
UBY2LENQ EQU   *-UBUYD             L'OBJECT FOR $BUY (FOR SBY2)                 
*                                                                               
UROTD    DSECT                                                                  
UROTTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'ROT*')                 
UROTSPOT DS    CL2                 NUMBER OF SPOTS                              
UROTCOST DS    CL9                 COST OVERRIDE (2 DEC IMPLIED)                
UROTDATE DS    CL6                 DATE YYMMDD                                  
UROTPRD1 DS    CL3                 PRD 1                                        
UROTPRD2 DS    CL3                 PRD 2 (PIGGYBACK)                            
UROTLEN1 DS    CL3                 LEN 1 (1ST OF PIGGYBACK PAIR)                
UROTDATX EQU   *                                                                
         EJECT                                                                  
*                                                                               
UOPTD    DSECT                                                                  
UOPTTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'OPT*')                 
UOPTOPTN DS    CL8                 OPTION BEING USED                            
UOPTVALU DS    CL10                VALUE OF THAT OPTION                         
UOPTDATX EQU   *                                                                
*                                                                               
USKDD    DSECT                                                                  
USKDTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'DTL*')                 
USKDCOST DS    CL9                 SPOT COST (N - 2 DEC IMPLIED) IF             
*                                  DIFFERENT FROM ABOVE                         
USKDSDT  DS    CL6                 START DATE YYMMDD                            
USKDCNTR DS    14CL2               14 2 BYTE COUNTERS - POSITIONAL              
*                                  FROM THE START DATE - EACH HAS               
*                                  N'SPOTS FOR CORRESPONDING WEEK.              
USKDDATX EQU   *                                                                
         SPACE 3                                                                
*                                                                               
UORBD    DSECT                                                                  
UORBTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'ORB*')                 
UORBPOS  DS    CL7                 POSITION DAYS MTWTFSS                        
UORBPSTM DS    CL4                 POSITION START TIME                          
UORBPETM DS    CL4                 POSITION END TIME                            
UORBPPRG DS    CL7                 POSITION PROGRAM NAME                        
UORBDATX EQU   *                                                                
         SPACE 3                                                                
*                                                                               
UCOMD    DSECT                                                                  
UCOMTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'COM*')                 
UCOMDATA DS    CL70                COMMENT DATA                                 
UCOMDATX EQU   *                                                                
         EJECT                                                                  
*                                                                               
UDEMD    DSECT                                                                  
UDEMTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'DEM*')                 
UDEMDEM  DS    14CL6               DEMO VALUES (1 DEC PLACED IMPLIED)           
*                                                                               
*        THE FOLLOWING IS USED FOR SBY2 ONLY!!!                                 
*             ....YES, FOR THE CANADIANS...                                     
*                                  MHC  01/16/03                                
*                                                                               
         ORG   UDEMDEM                                                          
UDM2DEM  DS    20CL6               DEMO VALUES (1 DEC PLACED IMPLIED)           
UDM2DATX EQU   *                                                                
UDEMDATX EQU   *                                                                
         SPACE 3                                                                
*                                                                               
DSTATUSD DSECT                                                                  
DSTATYPE DS    CL2                 STATUS OBJECT TYPE                           
*                                   C'01' = RECORD IN ERROR                     
*                                   C'02' = RECORD MODIFIED OK                  
*                                   C'03' = RECORD RECORD ADDED OK              
*                                   C'04' = TOTAL NUMBER DELETED                
DSTARECN DS    CL5                 RECORD NUMBER (ONE BASED)                    
*                                   ZERO = ERROR IN HDR* RECORD                 
         ORG   DSTARECN                                                         
DSTATOTD DS    CL5                 TOTAL NUMBER OF BUYS DELETED                 
*                                   ONLY WHEN TYPE = C'04'                      
DSTAOKLQ EQU   *-DSTATUSD          L'OBJECT WHEN TYPE = C'02','03','04'         
DSTASUBR DS    CL2                 SUB-RECORD NUMBER (ZERO-BASED)               
DSTAFLDN DS    CL2                 FIELD NUMBER IN ERROR                        
DSTAMSGN DS    CL5                 ERROR MESSAGE NUMBER                         
DSTAERLQ EQU   *-DSTATUSD          L'OBJECT WHEN TYPE = C'01'                   
         EJECT                                                                  
*              EQUATES                                                          
*                                                                               
LENMINRC EQU   1976                LENGTH OF A SPOT MINIO RECORD                
NUMMNBUF EQU   2                   NUMBER OF MINIO RECORD BUFFERS               
MINBUFSZ EQU   NUMMNBUF*LENMINRC   TOTAL SPACE FOR MINIO BUFFERS                
MINRCTBL EQU   14336               MINIO RECORD TABLE SIZE                      
         SPACE 3                                                                
WORKD    DSECT                                                                  
*                                                                               
MINBLK   DS    (MINBLKL)X          MINIO PARAMETER BLOCK                        
*                                                                               
MINRECTB DS    (MINRCTBL)X         MINIO RECORD TABLE                           
MINBUFFS DS    (MINBUFSZ)X         MINIO I/O BUFFERS                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117CTMAD18   04/13/09'                                      
         END                                                                    
