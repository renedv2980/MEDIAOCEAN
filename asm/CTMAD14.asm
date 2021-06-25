*          DATA SET CTMAD14    AT LEVEL 020 AS OF 05/15/19                      
*PHASE TA0C14A                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE SIXPACK                                                                
TA0C14   TITLE 'CTMAD14 - $MAD UPLOAD PRINT INSERTIONS'                         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*     KWAN   09/2015  OBJECT VALIDATION - READ HIGH AND THEN SEQ                
*                                                                               
*     BOBY   7/06     UP LOAD CUSTOM COLUMNS                                    
*                                                                               
*     BOBY   2/06     STOP DUMPS WHEN INSERTION DATE CHANGED                    
*                                                                               
*     BPLA   6/00     REMOVE  "OI   GLVXFLG1,GLV1GOTO" STATEMENT                
*                     BIT NEEDED FOR SOMETHING ELSE                             
*                                                                               
TA0C14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,TA0C14,RA,R8,RR=R2,CLEAR=YES                         
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
         LA    RF,MINBUFFS         MINIO BUFFERS                                
         ST    RF,AMINBUFF                                                      
         LA    RF,MINRECTB         MINIO RECORD TABLE                           
         ST    RF,AMINRECT                                                      
         LA    RF,BUYREC           PRINTPAK BUY RECORD I/O AREA                 
         ST    RF,ABUYREC                                                       
         DROP  RE                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   GLOBBER,CGLOBBER    A(GLOBBER)                                   
         MVC   MINIO,CMINIO        A(MINIO)                                     
         MVC   PERVAL,CPERVAL      A(PERVAL)                                    
         DROP  RE                                                               
*                                                                               
         L     RE,=V(PUBVAL)                                                    
         AR    RE,R2                                                            
         ST    RE,PUBVAL           A(PUBVAL)                                    
         L     RE,=V(RECUP)                                                     
         AR    RE,R2                                                            
         ST    RE,RECUP            A(RECUP)                                     
         L     RE,=V(SIXPACK)                                                   
         AR    RE,R2                                                            
         ST    RE,SIXPACK          A(SIXPACK)                                   
*                                                                               
         GOTO1 SWITCH,DMCB,=C'PRINT',0                                          
         CLI   DMCB+4,0                                                         
         BE    *+14                                                             
         MVC   MDACTION,=Y(ERSWITCH)  ERROR SWITCHING TO PRINT SYSTEM           
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LR    RE,R5               CLEAR MINIO PARAMETER BLOCK                  
         LH    RF,=Y(MINBLKL)                                                   
         XCEFL                                                                  
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=C'PRTFIL  ' FILE NAME                                    
         MVC   MINDIR,=C'PRTDIR  ' DIR NAME                                     
         MVI   MINFKLEN,25         KEY LENGTH                                   
         MVI   MINNCTL,2           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=Y(LENMINRC)  MAXIMUM RECORD LENGTH                     
         MVI   MINEKLEN,L'PEUPEKEY ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,PEUPEKEY-PEUPREC  DISPLACEMENT TO ELEMENT KEY           
         MVC   MINBUFF,AMINBUFF    A(FIRST BUFFER)                              
         MVC   MINRTAB,AMINRECT    A(AREA FOR RECORD TABLE)                     
         LA    RF,ELEMENT                                                       
         ST    RF,MINELEM          A(AREA FOR MINIO ELEMENT)                    
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   MINRECUP,RECUP      A(RECUP)                                     
         MVC   MINRTABL,=Y(MINRCTBL) LENGTH OF RECORD TABLE                     
         MVC   MINMAXEL,=H'250'    MAX LENGTH OF ELEMENT                        
         MVI   MINNBUF,NUMMNBUF    NUMBER OF AVAILABLE BUFFERS                  
         MVI   MINFILTL,1          FILTER ON ELEMENT CODE ONLY                  
         LA    R3,MINMKEY          SET MASTER KEY                               
         USING PEUPKEY,R3                                                       
         MVC   PEUPAGY,SVAGY       AGENCY                                       
         MVC   PEUPMED,SVMED       MEDIA                                        
         MVI   PEUPTYPE,X'90'      RECORD TYPE                                  
         MVC   PEUPCLT,SVCLT       CLIENT                                       
         MVC   PEUPPRD,SVPRD       PRODUCT                                      
         MVC   PEUPEST,SVEST       ESTIMATE                                     
         MVC   PEUPPUB,SVPUB       PUBCODE                                      
         DROP  R3                                                               
         DROP  R5                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT GETS THE FILE HEADER               
* OBJECT, INITIALIZES MINIO, THEN EXITS AND WAITS FOR INSERTIONS.               
***********************************************************************         
PROCSTRT NTR1                                                                   
*                                                                               
         MVI   INTERUPT,C'N'       UPLOAD HAS NOT BEEN INTERRUPTED              
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         MVI   MDLAST,C'N'         NOT YET LAST OUTPUT DATA FRAME               
         MVI   CONFFLAG,0          HAVEN'T CONFIRMED YET                        
         MVI   ENDOBJCT,C'N'       NOT END OF OBJECTS                           
         XC    INSNUM,INSNUM       NO INSERTIONS YET                            
         XC    LASTPUB,LASTPUB     NO PUBS YET                                  
         XC    TOTALDEL,TOTALDEL   NO INSERTIONS DELETED YET                    
         MVI   SVERONLY,C'N'       ASSUME NOT ERRORS-ONLY UPLOAD                
         MVI   PUBLSTST,C'N'       PUBLIST BUILD NOT YET STARTED                
         ZAP   BYSERCNT,=P'0'      INIT BUY SERIAL# COUNTER                     
*                                                                               
PS5      MVI   TSTDATAF,0                                                       
         L     R1,ATWA             USE INTERNAL TABLE ENTRY?                    
         LA    R1,MADDATA-TA0CFFD(R1)                                           
         CLI   0(R1),C'X'          ONE OF OURS?                                 
         BNE   PS10                                                             
         MVC   TSTDATAF,1(R1)                                                   
         CLI   1(R1),C'1'                                                       
         BE    *+12                                                             
         CLI   1(R1),C'2'                                                       
         BNE   PS10                                                             
*                                                                               
         GOTO1 GETTITEM            GET TEST TABLE ITEM                          
         B     PS20                                                             
*                                                                               
PS10     GOTO1 GETITEM             GET FIRST OBJECT                             
         BNE   EXIT                                                             
*                                                                               
PS20     CLC   TYPENUM,=A(ITPINSIG) IS THERE A SIGNATURE OBJECT?                
         BNE   PS25                                                             
         L     R2,ADATA            OBJECT AS GIVEN TO ME                        
         USING USIGD,R2                                                         
         CLI   USIGVER,C'0'        DELIBERATE FAILURE (FOR DEBUGGING)?          
         BE    *+20                YES                                          
         MVC   SVVER,USIGVER       SAVE VERSION NUMBER                          
         MVC   SVERONLY,USIGERR    SAVE "ERRORS ONLY" FLAG                      
         B     PS5                 GET THE NEXT OBJECT (HEADER)                 
         DROP  R2                                                               
*                                                                               
         LA    RF,WORK                                                          
         USING DSTATUSD,RF                                                      
         MVC   DSTATYPE,=C'05'     INITIALIZATION ERROR                         
         MVC   DSTAINIE,=C'00001'  PC PROGRAM EXPIRED                           
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         B     PS60                                                             
         DROP  RF                                                               
*                                                                               
PS25     CLC   TYPENUM,=A(ITPINHDR) THIS MUST BE FILE HEADER OBJECT             
         BE    *+14                                                             
         MVC   MDACTION,=Y(ER11OBCD)  INVALID OBJECT CODE                       
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         L     R2,ADATA            OBJECT AS GIVEN TO ME                        
         USING UHDRD,R2                                                         
         MVC   SVAGY,UHDRAGID      BUYING AGENCY ID                             
         MVC   SVMED,UHDRMED       MEDIA CODE                                   
         MVC   SVCLT,UHDRCLT       CLIENT CODE                                  
         MVC   SVPRD,UHDRPRD       PRODUCT CODE                                 
         MVC   SVPESTFL,UHDRPEST   PARTIAL ESTIMATE FLAG                        
         MVC   SVPESTST,UHDRSTDT   PARTIAL ESTIMATE START DATE                  
         MVC   SVPESTEN,UHDRENDT   PARTIAL ESTIMATE END DATE                    
*                                                                               
         CLC   UHDREST,SPACES                                                   
         BNE   *+10                                                             
         SR    R1,R1               NO ESTIMATE GIVEN -- ERROR                   
         B     PS40                                                             
         CLI   UHDREST+2,C' '                                                   
         BE    *+14                                                             
         PACK  DUB,UHDREST(3)                                                   
         B     PS30                                                             
         CLI   UHDREST+1,C' '                                                   
         BE    *+14                                                             
         PACK  DUB,UHDREST(2)                                                   
         B     *+10                                                             
         PACK  DUB,UHDREST(1)                                                   
PS30     CVB   R1,DUB                                                           
PS40     STCM  R1,3,SVEST          ESTIMATE NUMBER                              
*                                                                               
         MVI   SVUPDFLG,C'N'       ASSUME ENTIRE ESTIMATE IS BEING SENT         
         CLI   UHDRUPDO,C'Y'       UPDATES ONLY?                                
         BNE   *+8                                                              
         MVI   SVUPDFLG,C'Y'       CORRECT                                      
         CLI   UHDRUPDO,C'D'       DELETES ONLY?                                
         BNE   *+8                                                              
         MVI   SVUPDFLG,C'D'       CORRECT                                      
*                                                                               
         MVC   SVUPTYPE,UHDRTYPE   UPLOAD TYPE (STANDARD, ETC.)                 
         DROP  R2                                                               
*                                                                               
         CLI   SVUPTYPE,C'E'       EDR UPLOAD?                                  
         BNE   *+6                                                              
         DC    H'0'                NO LONGER SUPPORTED                          
*                                                                               
         BAS   RE,VALHEADR         VALIDATE HEADER RECORD FIELDS                
*                                                                               
         CLI   ERFLDNUM,0          ANY ERROR ON HEADER?                         
         BNE   PS50                YES                                          
*                                                                               
PS48     LA    RE,SVHDROBJ         BUILD HEADER OBJECT HERE                     
         USING PHDRODAT,RE                                                      
         L     RF,ADATA            OBJECT AS GIVEN TO ME                        
         XC    PHDRODAT(PHDRLENQ),PHDRODAT                                      
         MVC   PHDRTYPE,=C'HDR*'                                                
         MVC   PHDRSTRT(PHDRRLNQ),0(RF)   OBJECT DATA                           
         DROP  RE                                                               
*                                                                               
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
*                                                                               
         CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BE    *+8                                                              
         BAS   RE,PROCMID          YES                                          
         B     XIT                                                              
*                                                                               
PS50     LA    RF,WORK                                                          
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
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         DROP  RF                                                               
*                                                                               
PS60     GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE HEADER RECORD FIELDS, SO WE DON'T UPLOAD A SHITLOAD OF           
* BOGUS INSERTIONS FOR NOTHING.                                                 
***********************************************************************         
VALHEADR NTR1                                                                   
*                                                                               
         MVI   ERFLDNUM,2          AGENCY                                       
         CLC   SVAGY,SPACES                                                     
         BNE   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
*                                                                               
         MVI   ERFLDNUM,3          MEDIA                                        
         CLI   SVMED,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
         LA    R5,KEY                                                           
         USING PAGYREC,R5                                                       
         XC    PAGYKEY,PAGYKEY     BUILD AGENCY HEADER KEY                      
         MVC   PAGYKAGY,SVAGY                                                   
         MVC   PAGYKMED,SVMED                                                   
         MVI   PAGYKRCD,PAGYKIDQ                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'PRTDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   ERRNUM,=H'2'        INVALID INPUT FIELD                          
         B     VHX                                                              
         DROP  R5                                                               
*                                                                               
         MVI   ERFLDNUM,5          CLIENT                                       
         CLC   SVCLT,SPACES                                                     
         BNE   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
         LA    R5,KEY                                                           
         USING PCLTREC,R5                                                       
         XC    PCLTKEY,PCLTKEY     BUILD CLIENT HEADER KEY                      
         MVC   PCLTKAGY,SVAGY                                                   
         MVC   PCLTKMED,SVMED                                                   
         MVI   PCLTKRCD,PCLTKIDQ                                                
         MVC   PCLTKCLT,SVCLT                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'PRTDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   ERRNUM,=H'53'       RECORD NOT FOUND                             
         B     VHX                                                              
         DROP  R5                                                               
*                                                                               
         BRAS  RE,TSTLOK           IF CLIENT LOCKED                             
         BNZ   VHX                    END PROCESSING                            
*                                                                               
         MVI   ERFLDNUM,6          PRODUCT                                      
         CLC   SVPRD,SPACES                                                     
         BNE   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
         LA    R5,KEY                                                           
         USING PPRDREC,R5                                                       
         XC    PPRDKEY,PPRDKEY     BUILD PRODUCT HEADER KEY                     
         MVC   PPRDKAGY,SVAGY                                                   
         MVC   PPRDKMED,SVMED                                                   
         MVI   PPRDKRCD,PPRDKIDQ                                                
         MVC   PPRDKCLT,SVCLT                                                   
         MVC   PPRDKPRD,SVPRD                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'PRTDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   ERRNUM,=H'53'       RECORD NOT FOUND                             
         B     VHX                                                              
         DROP  R5                                                               
*                                                                               
         MVI   ERFLDNUM,7          ESTIMATE                                     
         OC    SVEST,SVEST                                                      
         BNZ   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
         LA    R5,KEY                                                           
         USING PESTREC,R5                                                       
         XC    PESTKEY,PESTKEY     BUILD ESTIMATE HEADER KEY                    
         MVC   PESTKAGY,SVAGY                                                   
         MVC   PESTKMED,SVMED                                                   
         MVI   PESTKRCD,PESTKIDQ                                                
         MVC   PESTKCLT,SVCLT                                                   
         MVC   PESTKPRD,SVPRD                                                   
         MVC   PESTKEST,SVEST                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'PRTDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   ERRNUM,=H'16'       INVALID ESTIMATE                             
         B     VHX                                                              
         DROP  R5                                                               
*                                                                               
         XC    SVPESTS3,SVPESTS3   SET START DATE TO LOW VALUES                 
         MVC   SVPESTE3,XFF        SET END DATE TO HIGH VALUES                  
         CLI   SVPESTFL,C'Y'       PARTIAL ESTIMATE UPLOAD?                     
         BNE   VH10                NO                                           
*                                                                               
         MVI   ERFLDNUM,9          START DATE                                   
         CLC   SVPESTST,SPACES     START DATE GIVEN?                            
         BNE   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
         GOTO1 PERVAL,DMCB,(8,SVPESTST),('PVINSGLO',WORK)                       
         TM    DMCB+4,X'FF'-PVRCONE  ANY ERRORS?                                
         BZ    *+14                NO                                           
         MVC   ERRNUM,=H'2'        INVALID FIELD                                
         B     VHX                                                              
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   SVPESTS3,PVALBSTA-PERVALD(RF)  BINARY DATE                       
*                                                                               
         MVI   ERFLDNUM,10         END DATE                                     
         CLC   SVPESTEN,SPACES     END DATE GIVEN?                              
         BNE   *+14                                                             
         MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
         GOTO1 PERVAL,DMCB,(8,SVPESTEN),('PVINSGLO',WORK)                       
         TM    DMCB+4,X'FF'-PVRCONE  ANY ERRORS?                                
         BZ    *+14                NO                                           
         MVC   ERRNUM,=H'2'        INVALID FIELD                                
         B     VHX                                                              
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   SVPESTE3,PVALBSTA-PERVALD(RF)  BINARY DATE                       
*                                                                               
         CLC   SVPESTS3,SVPESTE3                                                
         BNH   *+14                                                             
         MVC   ERRNUM,=H'2'        START DATE AFTER END DATE                    
         B     VHX                                                              
*                                                                               
VH10     MVI   ERFLDNUM,0          ALL FIELDS ARE OK                            
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         LA    R3,MINMKEY                                                       
         USING PEUPKEY,R3                                                       
         MVC   PEUPAGY,SVAGY       AGENCY                                       
         MVC   PEUPMED,SVMED       MEDIA                                        
         MVI   PEUPTYPE,X'90'      RECORD TYPE                                  
         MVC   PEUPCLT,SVCLT       CLIENT                                       
         MVC   PEUPPRD,SVPRD       PRODUCT                                      
         MVC   PEUPEST,SVEST       ESTIMATE                                     
         DROP  R3                                                               
*                                                                               
         L     R4,ATIA                                                          
         CLI   PUBLSTST,C'Y'       HAVE WE STARTED BUILDING PUBLIST?            
         BNE   VH12                                                             
*                                                                               
         L     R2,10(R4)           NUMBER OF ENTRIES IN PUBLIST                 
         LA    R4,14(R4)           A(FIRST ENTRY IN PUBLIST)                    
         LR    R1,R2                                                            
         MH    R1,=H'8'            DISPLACEMENT TO END OF LIST                  
         AR    R4,R1               R4 = A(END OF LIST)                          
         B     VH15                                                             
*                                                                               
VH12     SR    R2,R2               NO ENTRIES IN PUBLIST YET                    
         MVC   2(8,R4),=C'PUBLIST*'  FIRST OBJECT IS PUBLIST                    
         LA    R4,14(R4)           A(FIRST ENTRY IN PUBLIST)                    
         MVI   PUBLSTST,C'Y'                                                    
*                                                                               
VH15     LA    R3,KEY                                                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',MINMKEY,KEY               
VH20     CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(PEUPPUB-PEUPKEY),MINMKEY                                     
         BNE   VH40                NOT SAME AGY/MED/TYPE/CLT/EST                
         CLC   PEUPEKEY-PEUPKEY(,R3),XFF   LAST RECORD IN MINIO SET?            
         BNE   VH30                                                             
         MVC   0(1,R4),PEUPMED-PEUPKEY(R3)  SAVE MEDIA IN LIST                  
         MVC   1(6,R4),PEUPPUB-PEUPKEY(R3)  SAVE PUBCODE IN LIST                
         MVI   7(R4),C'N'          NOT SEEN IN UPLOAD FILE YET                  
         LA    R2,1(R2)            INCREMENT NUMBER OF LIST ENTRIES             
         LA    R4,8(R4)            BUMP TO NEXT SLOT IN PUBLST                  
*                                                                               
VH30     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',MINMKEY,KEY               
         B     VH20                                                             
         DROP  R5                                                               
*                                                                               
VH40     L     R1,ATIA                                                          
         ST    R2,10(R1)           NUMBER OF ENTRIES IN PUBLIST                 
         SR    R4,R1                                                            
         STH   R4,0(R1)            TOTAL LENGTH OF PUBLIST                      
         GOTO1 WRTTWA,DMCB,ATIA,2  STORE PUBLIST IN TWA PAGE 2                  
*                                                                               
VHX      B     XIT                 HEADER RECORD FIELDS ARE VALID               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT GETS ALL INSERTIONS         
* FOR ALL PUBS AND PUTS THEM IN THE TEMPSTR BUFFERS.                            
***********************************************************************         
PROCMID  NTR1                                                                   
*                                                                               
         GOTO1 READTWA,DMCB,ATIA,2 READ PUBLIST FROM TWA PAGE 2                 
*                                                                               
         CLI   ENDOBJCT,C'Y'       IF END OF OBJECTS                            
         BE    PM30                THEN WE CAN CONFIRM                          
         CLI   ENDBLOCK,C'Y'       IF END OF BLOCK                              
         BE    PM30                THEN WE CAN CONFIRM                          
*                                                                               
         CLI   TMPOPEND,C'Y'       IS TEMPSTR ALREADY OPEN?                     
         BE    PM10                                                             
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'PTX'),,(3,0)                               
         BNE   EXIT                                                             
*                                                                               
         MVI   TMPOPEND,C'Y'       TEMPSTR IS OPEN                              
         LH    R3,=Y(PHDRLENQ)     WRITE FILE HEADER OBJECT TO TEMPSTR          
         GOTO1 PUTTMP,DMCB,SVHDROBJ,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
PM10     BAS   RE,GETOBJCT                                                      
*                                                                               
         CLI   EIFFLAG,C'Y'        ANY MORE DATA FOR THIS INPUT FRAME?          
         BE    PM20                NO MORE                                      
*                                                                               
         CLI   ENDOBJCT,C'Y'       ANY MORE OBJECTS COMING?                     
         BNE   *+12                                                             
         BAS   RE,DELPUBS          NO -- DELETE REMAINING PUBS                  
         B     *+12                                                             
         CLI   ENDBLOCK,C'Y'       IF END OF BLOCK                              
         BNE   PM20                                                             
*                                                                               
         GOTO1 TMPCLOSE            DONE WITH PUTTING INTO TEMPFILE              
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
*                                                                               
         BAS   RE,CALLBUY          CALL PRINT BUY PROGRAM                       
*                                                                               
PM20     GOTO1 WRTTWA,DMCB,ATIA,2  STORE PUBLIST IN TWA PAGE 2                  
*                                                                               
         CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BE    *+8                                                              
         MVI   OVERMODE,C'M'       YES                                          
         B     PMX                                                              
*                                                                               
PM30     CLI   CONFFLAG,X'FF'      CONFIRM USED BEFORE?                         
         BE    PM40                YES                                          
*                                                                               
*                                  DID WE GET CONTROL BACK FROM BUY?            
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   PM40                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVXCTL                                  
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   MDACTION,=Y(ER10GDEL)  PROBLEM WITH GLOBBER DELETE               
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY,=C'PRI'    FROM PRINT BUY?                              
         BNE   PM40                                                             
         CLC   GLVXFRPR,=C'BUY'                                                 
         BNE   PM40                                                             
         TM    GLVXFLG1,GLV1RETN   YES, IS IT A RETURN CALL?                    
         BZ    PM40                                                             
         DROP  R1                                                               
*                                                                               
         MVI   CONFFLAG,0          NOTHING IN OUR BUFFER YET                    
*                                                                               
PM40     BAS   RE,CONFIRM          SEND OUT CONFIRMATIONS                       
*                                                                               
         CLI   ENDOBJCT,C'Y'       DID WE SEE THE END-OF-DATA OBJECT?           
         BNE   PMX                                                              
*                                                                               
         OC    TOTALDEL,TOTALDEL   ANY INSERTIONS DELETED?                      
         BZ    PM50                                                             
         LA    R2,WORK             BUILD RECORD DELETED TOTAL OBJECT            
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'04'     RECORDS DELETED OBJECT                       
         L     R0,TOTALDEL         TOTAL NUMBER OF INSERTIONS DELETED           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTATOTD,DUB+4(4)                                                
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
         DROP  R2                                                               
*                                                                               
PM50     GOTO1 PUTITEM,DMCB,ITEOD,0   THAT'S ALL, FOLKS!                        
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'         LAST OUTPUT DATA FRAME                       
*                                                                               
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
* THIS ROUTINE GETS THE INSERTIONS FOR A PUB FROM THE INPUT FRAMES.             
***********************************************************************         
GETOBJCT NTR1                                                                   
*                                                                               
GOBJLOOP L     R1,ATWA             USE INTERNAL TABLE ENTRY?                    
         LA    R1,MADDATA-TA0CFFD(R1)                                           
         MVI   TSTDATAF,0                                                       
         CLI   0(R1),C'X'          ONE OF OURS?                                 
         BNE   GOBJ10                                                           
         MVC   TSTDATAF,1(R1)                                                   
         CLI   1(R1),C'1'                                                       
         BE    *+12                                                             
         CLI   1(R1),C'2'                                                       
         BNE   GOBJ10                                                           
*                                                                               
         GOTO1 GETTITEM            GET TEST TABLE ITEM                          
         B     GOBJ20                                                           
*                                                                               
GOBJ10   GOTO1 GETITEM             GET AN OBJECT                                
         BNE   EXIT                                                             
*                                                                               
GOBJ20   CLI   EIFFLAG,C'Y'        END OF FRAME?                                
         BNE   GOBJ30              NO                                           
         CLI   MINCHG,C'Y'         ANY CHANGE TO THIS MINIO RECORD?             
         BNE   GOBJX                                                            
*                                                                               
         L     R5,AMINBLK          YES -- SAVE IT AT END OF FRAME               
         USING MINBLKD,R5                                                       
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    GOBJX                                                            
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
GOBJ30   CLC   TYPENUM,=A(ITEOD)   END-OF-DATA?                                 
         BNE   *+12                                                             
         MVI   ENDOBJCT,C'Y'                                                    
         B     GOBJX                                                            
*                                                                               
         CLC   TYPENUM,=A(ITEOB)   END-OF-BLOCK?                                
         BNE   *+12                                                             
         MVI   ENDBLOCK,C'Y'                                                    
         B     GOBJX                                                            
*                                                                               
         L     R1,AIO              COPY THE OBJECT TO LOCAL STORAGE             
         LA    R1,4(R1)            LEAVE ROOM FOR OBJECT IDENTIFIER             
         MVI   0(R1),C' '                                                       
         MVC   1(255,R1),0(R1)     PRE-FILL WITH BLANKS                         
         LR    R0,R1                                                            
         L     R1,DATALEN                                                       
*                                                                               
         CHI   R1,4000             LIMIT SIZE OF OBJECT                         
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ADATA                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,VALOBJCT         VALIDATE THE OBJECT                          
*                                                                               
         B     GOBJLOOP                                                         
*                                                                               
GOBJX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE OBJECT WE GOT                                      
***********************************************************************         
VALOBJCT NTR1                                                                   
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         CLC   TYPENUM,=A(ITPININT) USER INTERRUPT OBJECT?                      
         BE    VOBJ600             YES                                          
         CLI   INTERUPT,C'Y'                                                    
         BNE   *+14                                                             
         MVC   MDACTION,=Y(ER11OBCD)  INVALID OBJECT CODE                       
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         CLC   TYPENUM,=A(ITPININS) INSERTION OBJECT?                           
         BE    VOBJ100                                                          
         CLC   TYPENUM,=A(ITPINADD) ADDITIONAL DATA OBJECT?                     
         BE    VOBJ200                                                          
         CLC   TYPENUM,=A(ITPINCCL) CUSTOM COLUMN   OBJECT?                     
         BE    VOBJ250                                                          
         CLC   TYPENUM,=A(ITPINACH) ADDITONAL CHARGE OBJECT?                    
         BE    VOBJ260                                                          
         CLC   TYPENUM,=A(ITPINZZZ) CORPORATE BUY OBJECT?                       
         BE    VOBJ300                                                          
         CLC   TYPENUM,=A(ITPINEIN) END OF INSERTION OBJECT?                    
         BE    VOBJ400                                                          
         CLC   TYPENUM,=A(ITPINEPB) END OF PUB OBJECT?                          
         BE    VOBJ500                                                          
         MVC   MDACTION,=Y(ER11OBCD)  INVALID OBJECT CODE                       
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
VOBJ100  L     R6,AIO              INSERTION OBJECT                             
         USING UINSD,R6                                                         
         MVC   UINSTYPE,=C'INS*'                                                
         LH    RF,INSNUM           INCREMENT INSERTION NUMBER                   
         LA    RF,1(RF)                                                         
         CLC   UINSINUM,SPACES     INSERTION NUMBER GIVEN EXPLICITLY?           
         BE    *+14                NO -- JUST INCREMENT OLD ONE                 
         PACK  DUB,UINSINUM        YES -- USE IT                                
         CVB   RF,DUB                                                           
         STH   RF,INSNUM                                                        
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
         MVI   INSCHG,C'N'         ASSUME INSERTION WON'T CHANGE                
*                                                                               
         MVC   WORK,SPACES         NO                                           
         MVC   WORK(L'UINSPUB),UINSPUB                                          
         MVC   WORK+L'UINSPUB+1(L'UINSZONE),UINSZONE                            
         MVC   WORK+L'UINSPUB+L'UINSZONE+2(L'UINSEDTN),UINSEDTN                 
         LA    R2,L'UINSPUB+L'UINSZONE+L'UINSEDTN+2                             
         GOTO1 SQUASHER,DMCB,WORK,(C',',(R2))                                   
         L     R2,DMCB+4           L'SQUASHED STRING                            
         GOTO1 PUBVAL,DMCB,((R2),WORK),SVPUB                                    
         CLI   DMCB,X'FF'                                                       
         BE    *+14                INVALID PUBCODE                              
         MVC   PPAKPUB,SVPUB                                                    
         B     VOBJ108                                                          
*                                                                               
         XC    SVPUB,SVPUB                                                      
         MVC   PPAKPUB,XFF                                                      
         MVI   SVACT,C'A'          INVALID PUBCODE, SO TELL PRT/BUY...          
         MVI   INSCHG,C'Y'         ...TO ADD, KNOWING WE'LL GET ERROR           
         B     VOBJ180                                                          
*                                                                               
VOBJ108  XC    SVDATE,SVDATE                                                    
         MVC   DUB,UINSDATE        INSERTION DATE IN FORMAT YYYYMMDD            
         CLC   UINSDAY,SPACES      ANY DAY GIVEN?                               
         BNE   *+10                                                             
         MVC   DUB+6(2),=C'01'     NO - FORCE DAY 1 JUST FOR NOW                
         GOTO1 PERVAL,DMCB,(8,DUB),('PVINSGLO',WORK)                            
         TM    DMCB+4,X'FF'-PVRCONE  ANY ERRORS?                                
         BZ    VOBJ110             NO                                           
         MVC   UINSDATE,XFF                                                     
         MVI   SVACT,C'A'          INVALID INS DATE, SO TELL PRT/BUY...         
         MVI   INSCHG,C'Y'         ...TO ADD WITH XFF IN DATE FIELD,...         
         B     VOBJ112             ...SO HE KNOWS IT'S AN ERROR                 
*                                                                               
VOBJ110  LA    RF,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,RF                                                       
         MVC   SVDATE(2),PVALBSTA  BINARY YEAR/MONTH                            
         CLC   UINSDAY,SPACES      ANY DAY GIVEN?                               
         BE    VOBJ112                                                          
         MVC   SVDATE+2(1),PVALBSTA+2   BINARY DAY                              
         DROP  RF                                                               
*                                                                               
VOBJ112  CLC   LASTPUB,SVPUB       CHANGE OF PUB?                               
         BE    VOBJ170             NO, SAME AS LAST TIME                        
*                                                                               
         MVC   LASTPUB,SVPUB       REMEMBER THIS PUBCODE                        
         MVI   ENDBLOCK,C'N'       NOT END OF BLOCK                             
         LA    RF,MINMKEY          PUT MEDIA/PUBCODE INTO MASTER KEY            
         MVC   PEUPMED-PEUPKEY(,RF),SVMED                                       
         MVC   PEUPPUB-PEUPKEY(,RF),SVPUB                                       
         MVI   MINCHG,C'N'         NO CHANGES TO MINIO RECORD YET               
         MVI   PUBCHG,C'N'         NO CHANGES TO PUB DATA YET                   
*                                                                               
         L     R3,ATIA                                                          
         CLC   =C'PUBLIST*',2(R3)                                               
         BE    *+6                                                              
         DC    H'0'                FIRST OBJECT MUST BE PUBLIST                 
         L     R2,10(R3)           NUMBER OF ENTRIES IN PUBLIST                 
         LA    R3,14(R3)           POINT TO FIRST PUB IN LIST                   
         MVC   DUB(1),SVMED        BUILD KEY FOR BINSRCH                        
         MVC   DUB+1(6),SVPUB                                                   
         GOTO1 BINSRCH,DMCB,DUB,(R3),(R2),8,7,0                                 
         CLI   DMCB,X'01'                                                       
         BNE   VOBJ115             PUB EXISTS ON PRINTPAK                       
*                                                                               
* STATUS ELEMENT                                                                
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING PEUPEL01,R3                                                      
         MVI   PEUPEL01,X'01'      ELEMENT CODE                                 
         MVI   PEUPEL1L,PEUPEL1Q   ELEMENT LENGTH                               
         GOTO1 DATCON,DMCB,(5,0),(3,PEUPUDT)   UPLOAD DATE                      
         MVI   PEUPSEQN,1          FIRST UPLOAD OF THIS PUB                     
         DROP  R3                                                               
*                                                                               
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    VOBJ170             PROCESS THE INSERTION                        
         DC    H'0'                                                             
*                                                                               
VOBJ115  L     R1,DMCB             A(PUB ENTRY)                                 
         MVI   7(R1),C'Y'          MARK PUB AS HAVING BEEN UPLOADED             
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ELEMENT                                                       
         USING PEUPEL01,R3                                                      
         MVC   PEUPLDT,PEUPUDT     SAVE PREVIOUS UPLOAD DATE                    
         GOTO1 DATCON,DMCB,(5,0),(3,PEUPUDT)   NEW UPLOAD DATE                  
         CLC   PEUPLDT,PEUPUDT     SAME DAY AS PREVIOUS UPLOAD?                 
         BNE   *+18                                                             
         SR    R1,R1               YES -- INCREMENT COUNTER                     
         IC    R1,PEUPSEQN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,PEUPSEQN                                                      
*                                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
* THERE SHOULD BE NO ELEMENTS WHICH ARE MARKED FOR DELETION.  IF THERE          
* ARE, IT'S BECAUSE A PREVIOUS UPLOAD DIED ON THE MAINFRAME.  IF IT             
* DIED AFTER THE BUY WAS DELETED BUT BEFORE THE ELEMENT WAS DELETED, WE         
* MUST DELETE IT NOW.  IF IT DIED IN THE BUY PROGRAM, THEN THE ELEMENT          
* MUST REMAIN, BUT THE DELETE BIT MUST GET TURNED OFF.                          
*                                                                               
VOBJ120  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'       LOOK FOR FIRST INSERTION ON PUB              
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         B     VOBJ127                                                          
*                                                                               
VOBJ125  GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
*                                                                               
VOBJ127  CLI   MINERR,MINEEOF      ANY MORE INSERTIONS FOR THIS PUB?            
         BE    VOBJ140             NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         TM    PEUPUSRC,X'80'      IS THIS MARKED FOR DELETION?                 
         BZ    VOBJ125             NO                                           
*                                                                               
         LA    R4,KEY                                                           
         USING PBUYREC,R4                                                       
         XC    PBUYKEY,PBUYKEY     BUILD BUY RECORD KEY                         
         MVC   PBUYKAGY,SVAGY                                                   
         MVC   PBUYKMED,SVMED                                                   
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,SVCLT                                                   
         MVC   PBUYKPRD,SVPRD                                                   
         MVC   PBUYKPUB(6),PPAKPUB                                              
         MVC   PBUYKDAT,PEUPPDT                                                 
*                                                                               
         CLI   PBUYKDAT+2,0        IF DAY IS ZERO I.E. MONTHLY                  
         BNE   *+8                                                              
         MVI   PBUYKDAT+2,1           FORCE TO FIRST OF MONTH                   
*                                                                               
         MVC   PBUYKEST,SVEST                                                   
         MVC   PBUYKLIN,PEUPPLIN                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'PRTDIR',KEY,KEY                   
         CLI   DMCB+8,X'10'        RECORD NOT FOUND?                            
         BE    *+14                CORRECT -- INSERTION WAS DELETED             
         CLI   DMCB+8,0                                                         
         BE    VOBJ130             KEEP THE INSERTION                           
         DC    H'0'                BAD RETURN CODE FROM DATAMGR                 
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         B     VOBJ135                                                          
*                                                                               
VOBJ130  DS    0H                                                               
*                                                                               
         TM    PEUPUSRC,X'80'      IS THIS MARKED FOR DELETION?                 
         BZ    VOBJ137             NO                                           
*                                                                               
         MVC   WORK,MINEKEY        SAVE CURRENT MINIO ELEM KEY                  
         NI    PEUPUSRC,X'FF'-X'80' KEEP THIS ELEMENT                           
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
*                                                                               
VOBJ135  CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY,WORK        RESTORE SEQUENCE                             
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
*                                                                               
         B     VOBJ125             CONTINE WITH NEXT                            
*                                                                               
VOBJ137  DS    0H                                                               
*                                                                               
         B     VOBJ125             CONTINUE WITH NEXT                           
*                                                                               
         DROP  R3                                                               
*                                                                               
* THERE SHOULD BE NO ELEMENTS WITH A LINE NUMBER OF ZERO.  IF THERE             
* ARE, IT'S BECAUSE A PREVIOUS UPLOAD DIED ON THE MAINFRAME.  IF IT             
* DIED AFTER THE BUY WAS ADDED BUT BEFORE THE ELEMENT WAS UPDATED WITH          
* THE LINE NUMBER, WE MUST ADD THE LINE NUMBER NOW.  IF IT DIED BEFORE          
* THE BUY WAS ADDED, WE MUST DELETE THE ELEMENT.                                
*                                                                               
VOBJ140  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'       LOOK FOR FIRST INSERTION ON PUB              
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         B     VOBJ147                                                          
*                                                                               
VOBJ145  GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
*                                                                               
VOBJ147  CLI   MINERR,MINEEOF      ANY MORE INSERTIONS FOR THIS PUB?            
         BE    VOBJ170             NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         CLI   PEUPPLIN,0          IS THE PRINTPAK LINE NUMBER ZERO?            
         BNE   VOBJ145                                                          
*                                                                               
         LA    R4,KEY              YES -- MAYBE IT WASN'T ADDED                 
         USING PBUYREC,R4                                                       
         XC    PBUYKEY,PBUYKEY     BUILD BUY RECORD KEY                         
         MVC   PBUYKAGY,SVAGY                                                   
         MVC   PBUYKMED,SVMED                                                   
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,SVCLT                                                   
         MVC   PBUYKPRD,SVPRD                                                   
         MVC   PBUYKPUB(6),PPAKPUB                                              
         MVC   PBUYKDAT,PEUPPDT                                                 
*                                                                               
         CLI   PBUYKDAT+2,0        IF DAY IS ZERO I.E. MONTHLY                  
         BNE   *+8                                                              
         MVI   PBUYKDAT+2,1           FORCE TO FIRST OF MONTH                   
*                                                                               
         MVC   PBUYKEST,SVEST                                                   
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',KEYSAVE,KEY               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VOBJ150  CLC   KEY(24),KEYSAVE     SAME A/M/CLT/PRD/PUB/DATE/EST?               
         BNE   VOBJ160             NO -- BUY ISN'T THERE                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFIL',KEY+27,          +        
               ABUYREC,DMWORK                                                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,ABUYREC                                                       
         MVC   BYTE,(PBUYKLIN-PBUYKEY)(R4) SAVE LINE NUMBER                     
         MVI   ELCODE,PBIUPELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VOBJ152             BUY WASN'T UPLOADED -- SKIP IT               
         CLC   PEUPUNIQ,(PIUPUSEQ-PBIUPELD)(R4)  SAME INSERTION?                
         JNE   VOBJ152                                                          
         CLI   (PIUPELLN-PBIUPELD)(R4),PIUPELXQ                                 
         JNE   VOBJ155                                                          
         CLC   PEUPUIQX,(PIUPUQXT-PBIUPELD)(R4)  SAME INSERTION?                
         JNE   VOBJ152                                                          
         J     VOBJ155             YES -- IT IS THERE                           
*                                                                               
VOBJ152  GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',KEYSAVE,KEY               
         CLI   DMCB+8,0                                                         
         BE    VOBJ150                                                          
         DC    H'0'                                                             
*                                                                               
VOBJ155  MVC   PEUPPLIN,BYTE       PUT PRINTPAK LINE NUMBER IN ELEMENT          
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         B     VOBJ165                                                          
*                                                                               
VOBJ160  GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
*                                                                               
VOBJ165  CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     VOBJ140             START FROM FIRST ELEMENT AGAIN               
         DROP  R3                                                               
*                                                                               
* TELL $BUY TO ADD/CHANGE THE INSERTION                                         
*                                                                               
VOBJ170  CLC   UINSDATE(2),XFF     DID WE PUT BAD DATE IN DELIBERATELY?         
         BE    VOBJ180             YES -- THIS WILL GENERATE AN ERROR           
*                                                                               
         CLI   SVUPDFLG,C'D'       DELETES ONLY?                                
         BNE   VOBJ173             NO                                           
*                                                                               
         LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         XC    MINEKEY,MINEKEY     LOOK FOR THIS UNIQUE ID                      
         MVI   MINEKEY,X'90'                                                    
         CLC   UINSUNIQ+8(7),SPACES                                             
         JNH   VOBJ170K                                                         
         MVC   SVXUPIDC,UINSUNIQ                                                
         BRAS  RE,GETXUPEL         EXTENDED UPLOAD ELEMENT FOUND?               
         J     VOBJ170M                                                         
*                                                                               
VOBJ170K GOTO1 SIXPACK,DMCB,UINSUNIQ,MINEKEY+1,8                                
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
VOBJ170M CLI   MINERR,0                                                         
         BE    VOBJ172             THE BUY IS THERE, SO DELETE IT               
*                                                                               
VOBJ171  LA    RE,MYOBJECT         BUILD DELETE OBJECT HERE                     
         USING PDELODAT,RE                                                      
         XC    PDELODAT(PDELLENQ),PDELODAT                                      
         MVC   PDELRNUM,INSNUM     INSERTION RECORD NUMBER                      
         MVC   PDELTYPE,=C'DEL*'   INSERTION NOT FOUND, WE NEED ERROR           
         MVC   PDELPUB,SVPUB       PUBCODE                                      
         MVC   PDELDATE,SVDATE     INSERTION DATE                               
         MVI   PDELLINE,0          TELL $BUY IT'S IN ERROR                      
         MVC   PDELUNIQ,UINSUNIQ   UNIQUE ID                                    
         MVC   PDELMED,SVMED       MEDIA                                        
         MVC   PDELSER#,UINSSER#   UPLOAD SERIAL# FOR EXTENDED UPID             
         DROP  RE                                                               
*                                                                               
         LH    R2,=Y(PDELLENQ)     SEND DELETE OBJECT TO TEMPSTR                
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R2)                                        
         BNE   EXIT                                                             
         B     VOBJX                                                            
*                                                                               
VOBJ172  DS    0H                                                               
*                                                                               
*        VERIFY CORRECT INSERTION BEING DELETED                                 
*                                                                               
         CLC   PEUPPDT,SVDATE      MATCH DATE TO PBU FILE                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    PEUPUSRC,X'80'      MARK ELEMENT FOR DELETION                    
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         MVI   PUBCHG,C'Y'         PUB DATA CHANGED                             
         BAS   RE,PROCDEL          PUT DELETE OBJECT TO TEMPSTR                 
         B     VOBJX                                                            
         DROP  R3                                                               
*                                                                               
VOBJ173  XC    MINEKEY,MINEKEY     LOOK FOR THIS UNIQUE ID                      
         MVI   MINEKEY,X'90'                                                    
         CLC   UINSUNIQ+8(7),SPACES                                             
         JNH   VOBJ173K                                                         
         MVC   SVXUPIDC,UINSUNIQ                                                
         BRAS  RE,GETXUPEL         EXTENDED UPLOAD ELEMENT FOUND?               
         JNE   *+14                                                             
         MVC   UINSSER#,SVXUPIDC                                                
         J     VOBJ175                                                          
         BRAS  RE,GETXUPID                                                      
         MVC   UINSSER#,SVBYSER#                                                
         MVC   MINEKEY+L'PEUPMKEY+L'PEUPMKSP(L'PEUPMKS#),UINSSER#               
         J     VOBJ173M                                                         
VOBJ173K GOTO1 SIXPACK,DMCB,UINSUNIQ,MINEKEY+1,8                                
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,MINERNF                                                   
         BE    *+14                IT'S NEW -- ADD THIS INSERTION               
         CLI   MINERR,0                                                         
         BE    VOBJ175             IT EXISTS -- ACTION CHANGE                   
         DC    H'0'                                                             
*                                                                               
VOBJ173M MVI   SVACT,C'A'          NEW INSERTION DATE -- ADD IT                 
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         MVI   PEUPEL90,X'90'      ELEMENT CODE                                 
         MVI   PEUPEL9L,PEUP90LQ   ELEMENT LENGTH                               
*                                                                               
         CLC   UINSUNIQ+8(7),SPACES                                             
         JNH   VOBJ173N                                                         
         OC    UINSSER#,UINSSER#                                                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ZAP   PEUPUNID+1(L'PEUPELS#),UINSSER#                                  
         J     VOBJ173P                                                         
*                                                                               
VOBJ173N GOTO1 SIXPACK,DMCB,UINSUNIQ,PEUPUNID,8                                 
VOBJ173P MVC   PEUPPDT,SVDATE      INSERTION DATE (BINARY)                      
         OI    PEUPUSRC,X'41'      FLAGS: CAME FROM UPLOAD / KEEP IT            
         MVC   PEUPUNIQ,UINSUNIQ   USER-SUPPLIED SEQUENCE NUMBER                
         MVC   PEUPUIQX,UINSUNIQ+L'PEUPUNIQ                                     
         DROP  R3                                                               
*                                                                               
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         MVI   INSCHG,C'Y'         INSERTION HAS CHANGED                        
         B     VOBJ180                                                          
*                                                                               
VOBJ175  MVI   SVACT,C'C'          POSSIBLE CHANGE OF INSERTION                 
         LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         CLC   PEUPPDT,SVDATE      SAME INSERTION DATE?                         
         BE    VOBJ176                                                          
*                                                                               
         MVC   UINSDATE,=X'FFFFFEFEFEFE' USER CHANGING INSERTION DATE           
         MVC   UINSDATE+2(L'PEUPPDT),PEUPPDT   OLD DATE                         
         MVC   UINSDATE+5(L'SVDATE),SVDATE     NEW DATE                         
         L     RE,ATWA             USE INTERNAL TABLE ENTRY?                    
         USING TA0CFFD,RE                                                       
         DROP  RE                                                               
         MVC   PEUPPDT,SVDATE      UPDATE WITH NEW INSERTION DATE               
         MVI   SVACT,C'C'          INVALID INS DATE, SO TELL PRT/BUY...         
******   MVI   INSCHG,C'Y'         ...TO ADD WITH XFF IN DATE FIELD,...         
******   OI    PEUPUSRC,X'40'      KEEP THIS INSERTION                          
******   B     VOBJ180             FORCE BAD DATE TO CREATE ERROR               
*                                                                               
VOBJ176  DS    0H                                                               
*                                                                               
******   BE    *+6                                                              
******   DC    H'0'                *** NOTE: A DEATH HERE MEANS THAT            
*                                   1) USER HAS CHANGED INSERTION DATE          
*                                      (THAT'S A NO-NO) OR                      
*                                   2) USER HAS FUCKED UP UNIQUE IDS SO         
*                                      WE THINK HE'S CHANGING AN OLD            
*                                      INSERTION, EVEN THOUGH IT'S NEW          
*                                  WE CAN'T TELL THE DIFFERENCE FOR             
*                                  NOW, SO JUST CALL CLIENT AND TELL            
*                                  HIM TO FIX HIS FILE                          
*                                                                               
         MVI   INSCHG,C'Y'         INSERTION HAS CHANGED                        
         OI    PEUPUSRC,X'40'      KEEP THIS INSERTION                          
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         MVC   SVLINE,PEUPPLIN     PRINTPAK LINE NUMBER                         
         DROP  R3                                                               
*                                                                               
VOBJ180  CLC   UINSCLOS,SPACES     IS THERE A CLOSING DATE?                     
         BE    VOBJ185             NO                                           
         GOTO1 PERVAL,DMCB,(8,UINSCLOS),('PVINSGLO',WORK)                       
         TM    DMCB+4,X'FF'-PVRCONE  ANY ERRORS?                                
         BZ    VOBJ185             NO                                           
         MVC   UINSCLOS,XFF        BAD CLOSING DATE -- TELL PRT/BUY             
*                                                                               
VOBJ185  CLC   UINSSALE,SPACES     IS THERE A SALE DATE?                        
         BE    VOBJ190             NO                                           
         GOTO1 PERVAL,DMCB,(8,UINSSALE),('PVINSGLO',WORK)                       
         TM    DMCB+4,X'FF'-PVRCONE  ANY ERRORS?                                
         BZ    VOBJ190             NO                                           
         MVC   UINSSALE,XFF        BAD SALE DATE -- TELL PRT/BUY                
*                                                                               
VOBJ190  CLC   UINSMATC,SPACES     IS THERE A MATERIAL CLOSE DATE?              
         BE    VOBJ195             NO                                           
         GOTO1 PERVAL,DMCB,(8,UINSMATC),('PVINSGLO',WORK)                       
         TM    DMCB+4,X'FF'-PVRCONE  ANY ERRORS?                                
         BZ    VOBJ195             NO                                           
         MVC   UINSMATC,XFF        BAD MATERIAL CLOSE DATE -- TELL $BUY         
*                                                                               
VOBJ195  LA    RE,MYOBJECT         BUILD INSERTION OBJECT HERE                  
         USING PINSD,RE                                                         
         XC    PINSD(PINSLENQ),PINSD                                            
         MVC   PINSLEN,=Y(PINSLENQ) L'RECORD                                    
         MVC   PINSRNUM,INSNUM     INSERTION RECORD NUMBER                      
         CLI   SVACT,C'C'                                                       
         BNE   *+10                                                             
         MVC   PINSLINE,SVLINE     PRINTPAK LINE NUMBER (FOR CHANGE)            
         MVC   PINSACTN,SVACT      ACTION                                       
         MVC   PINSTYPE,UINSTYPE   PUT ALL INSERTION FIELDS IN OBJECT           
         MVC   PINSPUB,UINSPUB                                                  
         MVC   PINSZONE,UINSZONE                                                
         MVC   PINSEDTN,UINSEDTN                                                
         MVC   PINSDATE,UINSDATE                                                
         MVC   PINSADCD,UINSADCD                                                
         MVC   PINSSPAC,UINSSPAC                                                
         MVC   PINSSHOW,UINSSHOW                                                
         MVC   PINSREG,UINSREG                                                  
         MVC   PINSILL,UINSILL                                                  
         MVC   PINSCOST,UINSCOST                                                
         MVC   PINSRATE,UINSRATE                                                
         MVC   PINSPDSC,UINSPDSC                                                
         MVC   PINSPCST,UINSPCST                                                
         MVC   PINSCLOS,UINSCLOS                                                
         MVC   PINSSALE,UINSSALE                                                
         MVC   PINSMATC,UINSMATC                                                
         MVC   PINSUNIQ,UINSUNIQ                                                
         MVC   PINSMED,SVMED                                                    
         MVC   PINSPUBP,PPAKPUB                                                 
         MVC   PINSSPRD,UINSSPRD                                                
         MVC   PINSSER#,UINSSER#                                                
         LH    RF,PINSLEN          RECORD LENGTH                                
         STH   RF,MYOBJLEN         L'OBJECT FOR TEMPSTR SO FAR                  
         DROP  RE,R6                                                            
         B     VOBJX                                                            
*                                                                               
VOBJ200  CLI   SVUPDFLG,C'D'       FILE CONTAINS DELETES ONLY?                  
         BE    VOBJX               IGNORE OPT* RECORDS                          
*                                                                               
         L     R6,AIO              ADDITIONAL DATA OBJECT                       
         USING UOPTD,R6                                                         
         MVC   UOPTTYPE,=C'OPT*'                                                
*                                                                               
         LH    RF,SUBNUM           INCREMENT SUB-RECORD NUMBER                  
         LA    RF,1(RF)                                                         
         STH   RF,SUBNUM                                                        
*                                                                               
         MVI   INSCHG,C'Y'         INSERTION HAS CHANGED                        
         LA    RE,MYOBJECT         BUILD ADDITIONAL DATA OBJECT HERE            
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
         USING POPTD,RE                                                         
         XC    POPTD(POPTLENQ),POPTD                                            
         LH    RF,SUBNUM                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  POPTRNUM,DUB+4(4)                                                
         LA    RF,UOPTDATX         A(END OF DATA)                               
         BCTR  RF,0                BACK UP TO LAST NON-BLANK CHARACTER          
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         LA    RF,1(RF)            POINT JUST BEYOND LAST BLANK                 
         SR    RF,R6               COMPUTED LENGTH OF OBJECT. . .               
         AH    RF,=Y(POPTLENQ)     . . . PLUS LENGTH OF OVERHEAD. . .           
         STH   RF,POPTLEN          . . . = L'OBJECT FOR TEMPSTR                 
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,POPTLEN          PLUS L'OPT                                   
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT SO FAR                
         L     RF,DATALEN                                                       
         AH    RF,=H'4'            4 MORE FOR OBJECT IDENTIFIER                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    *+10                                                             
         MVC   POPTSTRT(0),UOPTD                                                
         B     VOBJX                                                            
         DROP  R6,RE                                                            
*                                                                               
*        CUSTOM COLUMN OBJECT                                                   
*                                                                               
VOBJ250  CLI   SVUPDFLG,C'D'       FILE CONTAINS DELETES ONLY?                  
         BE    VOBJX               IGNORE CCL* RECORDS                          
*                                                                               
         L     R6,AIO              CUSTOM COLUMN OBJECT                         
         USING UCCLD,R6                                                         
         MVC   UCCLTYPE,=C'CCL*'                                                
*                                                                               
         LH    RF,SUBNUM           INCREMENT SUB-RECORD NUMBER                  
         LA    RF,1(RF)                                                         
         STH   RF,SUBNUM                                                        
*                                                                               
         MVI   INSCHG,C'Y'         INSERTION HAS CHANGED                        
*                                                                               
         LA    RE,MYOBJECT         BUILD ADDITIONAL DATA OBJECT HERE            
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
*                                                                               
         USING PCCLD,RE                                                         
         XC    PCCLD(PCCLLENQ),PCCLD                                            
*                                                                               
         LH    RF,SUBNUM                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PCCLRNUM,DUB+4(4)                                                
*                                                                               
         LA    RF,UCCLDATX-1       A(END OF DATA)                               
         LA    R0,L'UCCLDATA       MAX DATA LENGTH                              
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                BACK UP TO LAST NON-BLANK CHARACTER          
         BCT   R0,*-10                                                          
         LA    RF,1(RF)            FORCES AT LEAST 1 CH EVEN IF BLANK           
*                                                                               
         LA    RF,1(RF)            POINT JUST BEYOND LAST BLANK                 
*                                                                               
         SR    RF,R6               COMPUTED LENGTH OF OBJECT. . .               
         AHI   RF,PCCLLENQ         . . . PLUS LENGTH OF OVERHEAD. . .           
         STH   RF,PCCLLEN          . . . = L'OBJECT FOR TEMPSTR                 
*                                                                               
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,PCCLLEN          PLUS L'CCL                                   
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT SO FAR                
*                                                                               
         L     RF,DATALEN                                                       
         AH    RF,=H'4'            4 MORE FOR OBJECT IDENTIFIER                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         BE    *+10                                                             
         MVC   PCCLSTRT(0),UCCLD   ADD DATA TO OUTPUT                           
*                                                                               
         B     VOBJX                                                            
*                                                                               
         DROP  R6,RE                                                            
*                                                                               
*        ADDITIONAL CHARGE OBJECT                                               
*                                                                               
VOBJ260  CLI   SVUPDFLG,C'D'       FILE CONTAINS DELETES ONLY?                  
         BE    VOBJX               IGNORE ACH* RECORDS                          
*                                                                               
         L     R6,AIO              CUSTOM COLUMN OBJECT                         
         USING UACHD,R6                                                         
         MVC   UACHTYPE,=C'ACH*'                                                
*                                                                               
         LH    RF,SUBNUM           INCREMENT SUB-RECORD NUMBER                  
         LA    RF,1(RF)                                                         
         STH   RF,SUBNUM                                                        
*                                                                               
         MVI   INSCHG,C'Y'         INSERTION HAS CHANGED                        
*                                                                               
         LA    RE,MYOBJECT         BUILD ADDITIONAL DATA OBJECT HERE            
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
*                                                                               
         USING PACHD,RE                                                         
         XC    PACHD(PACHLENQ),PACHD                                            
*                                                                               
         LH    RF,SUBNUM                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PACHRNUM,DUB+4(4)                                                
*                                                                               
         LHI   RF,PACHLENQ         . . . LENGTH OF ELEMENT . . .                
         STH   RF,PACHLEN          . . . = L'OBJECT FOR TEMPSTR                 
*                                                                               
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,PACHLEN          PLUS L'ACH                                   
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT SO FAR                
*                                                                               
         L     RF,DATALEN                                                       
         AH    RF,=H'4'            4 MORE FOR OBJECT IDENTIFIER                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         BE    *+10                                                             
         MVC   PACHSTRT(0),UACHD   ADD DATA TO OUTPUT                           
*                                                                               
         B     VOBJX                                                            
*                                                                               
         DROP  R6,RE                                                            
*                                                                               
VOBJ300  CLI   SVUPDFLG,C'D'       FILE CONTAINS DELETES ONLY?                  
         BE    VOBJX               IGNORE ZZZ* RECORDS                          
*                                                                               
         L     R6,AIO              CORPORATE BUY OBJECT                         
         USING UZZZD,R6                                                         
         MVC   UZZZTYPE,=C'ZZZ*'                                                
*                                                                               
         LH    RF,SUBNUM           INCREMENT SUB-RECORD NUMBER                  
         LA    RF,1(RF)                                                         
         STH   RF,SUBNUM                                                        
*                                                                               
         MVI   INSCHG,C'Y'         INSERTION HAS CHANGED                        
         LA    RE,MYOBJECT         BUILD CORPORATE BUY OBJECT HERE              
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
         USING PZZZD,RE                                                         
         XC    PZZZD(PZZZLENQ),PZZZD                                            
         LH    RF,SUBNUM                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PZZZRNUM,DUB+4(4)                                                
         LA    RF,UZZZDATX         A(END OF DATA)                               
         BCTR  RF,0                BACK UP TO LAST NON-BLANK CHARACTER          
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         LA    RF,1(RF)            POINT JUST BEYOND LAST BLANK                 
         SR    RF,R6               COMPUTED LENGTH OF OBJECT. . .               
         AH    RF,=Y(PZZZLENQ)     . . . PLUS LENGTH OF OVERHEAD. . .           
         STH   RF,PZZZLEN          . . . = L'OBJECT FOR TEMPSTR                 
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,PZZZLEN          PLUS L'ZZZ                                   
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT SO FAR                
         L     RF,DATALEN                                                       
         AH    RF,=H'4'            4 MORE FOR OBJECT IDENTIFIER                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    *+10                                                             
         MVC   PZZZSTRT(0),UZZZD                                                
         B     VOBJX                                                            
         DROP  RE,R6                                                            
*                                                                               
VOBJ400  CLI   INSCHG,C'Y'         DO WE NEED TO WRITE THE OBJECT?              
         BNE   VOBJX               NO                                           
         MVI   PUBCHG,C'Y'         PUB DATA CHANGED                             
         LA    RE,MYOBJECT         BUILD END-OF-INSERTION OBJECT HERE           
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
         USING PEIND,RE                                                         
         XC    PEIND(PEINLENQ),PEIND                                            
         MVC   PEINLEN,=Y(PEINLENQ) L'RECORD                                    
         MVC   PEINSTRT(PEINRLNQ),=C'EIN*'                                      
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,=Y(PEINLENQ)     PLUS L'EIN                                   
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT                       
         DROP  RE                                                               
*                                                                               
         LH    R3,MYOBJLEN         SEND OBJECT TO TEMPSTR                       
         CH    R3,=Y(MYOBJLNQ)                                                  
         BNH   *+6                                                              
         DC    H'0'                NEED MORE SPACE TO BUILD OBJECT              
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
         B     VOBJX                                                            
*                                                                               
VOBJ500  OC    SVPUB,SVPUB         SEE IF INVALID PUB                           
         BZ    VOBJX                                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'       LOOK FOR FIRST INSERTION ON PUB              
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,MINEEOF      ANY HISTORY FOR THIS PUB?                    
         BE    VOBJ530             NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VOBJ510  LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         TM    PEUPUSRC,X'40'      IS THIS MARKED FOR KEEPING?                  
         BZ    *+12                NO                                           
         NI    PEUPUSRC,X'FF'-X'40' TURN IT OFF (IT'S A WORK FLAG)              
         B     VOBJ520                                                          
*                                                                               
         CLC   PEUPPDT,SVPESTS3    WITHIN PARTIAL ESTIMATE DATE RANGE?          
         BL    VOBJ525                                                          
         CLC   PEUPPDT,SVPESTE3                                                 
         BH    VOBJ525             NO                                           
         CLI   SVERONLY,C'E'       ERRORS-ONLY UPLOAD?                          
         BE    VOBJ525             YES - DON'T PURGE ANYTHING                   
         CLI   SVUPDFLG,C'N'       SAFE TO PURGE THE REST?                      
         BNE   VOBJ525             NO - DON'T PURGE ANYTHING                    
*                                                                               
         OI    PEUPUSRC,X'80'      MARK IT FOR DELETION                         
         BAS   RE,PROCDEL          PUT DELETE OBJECTS TO TEMPSTR                
         MVI   PUBCHG,C'Y'         PUB DATA CHANGED                             
*                                                                               
VOBJ520  MVI   WORK,X'90'          SAVE ELEMENT KEY                             
         MVC   WORK+1(6),PEUPUNID                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(7),WORK     RESTORE ELEMENT KEY                          
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
VOBJ525  GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,MINEEOF      ANY MORE INSERTIONS FOR THIS PUB?            
         BE    VOBJ530             NO                                           
         CLI   MINERR,0                                                         
         BE    VOBJ510                                                          
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
VOBJ530  CLI   MINCHG,C'Y'         ANY CHANGE TO THIS MINIO RECORD?             
         BNE   VOBJ540                                                          
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VOBJ540  CLI   PUBCHG,C'Y'         ANY CHANGE TO THIS PUB'S DATA?               
         BNE   VOBJX                                                            
         LA    RE,MYOBJECT         BUILD END-OF-PUB OBJECT HERE                 
         USING PEOPODAT,RE                                                      
         XC    PEOPODAT(PEOPLENQ),PEOPODAT                                      
         MVC   PEOPTYPE,=C'EOP*'                                                
         DROP  RE                                                               
         LH    R3,=Y(PEOPLENQ)     SEND END-OF-PUB OBJECT TO TEMPSTR            
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
         B     VOBJX                                                            
         DROP  R5                                                               
*                                                                               
VOBJ600  MVI   INTERUPT,C'Y'       USER INTERRUPTED UPLOAD                      
*                                                                               
VOBJX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* PUT DELETE OBJECTS TO TEMPSTR FOR THE PRINT BUY PROGRAM.                      
***********************************************************************         
PROCDEL  NTR1                                                                   
*                                                                               
         LA    R3,ELEMENT          MINIO ELEMENT BEING DELETED                  
         USING PEUPMEL,R3                                                       
         LA    RE,MYOBJECT         BUILD DELETE OBJECT HERE                     
         USING PDELODAT,RE                                                      
         XC    PDELODAT(PDELLENQ),PDELODAT                                      
         MVC   PDELTYPE,=C'DEL*'                                                
         MVC   PDELPUB,SVPUB       PUBCODE                                      
         MVC   PDELDATE,PEUPPDT    INSERTION DATE                               
         MVC   PDELLINE,PEUPPLIN   PRINTPAK LINE NUMBER                         
         MVC   PDELUNIQ(L'PEUPUNIQ),PEUPUNIQ                                    
         MVC   PDELUNIQ+L'PEUPUNIQ(L'PEUPUIQX),PEUPUIQX                         
         MVC   PDELMED,SVMED       MEDIA                                        
         DROP  R3,RE                                                            
*                                                                               
         LH    R3,=Y(PDELLENQ)     SEND DELETE OBJECT TO TEMPSTR                
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE ALL INSERTIONS FOR PUBS WHICH WERE NOT UPLOADED                        
***********************************************************************         
DELPUBS  NTR1                                                                   
*                                                                               
         CLI   SVUPDFLG,C'N'       SAFE TO DELETE PUBS NOW?                     
         BNE   DPX                 NO -- DON'T DELETE LEFTOVER PUBS             
         CLI   INTERUPT,C'Y'       USER INTERRUPTED UPLOAD?                     
         BE    DPX                 YES -- DON'T DELETE LEFTOVER PUBS            
         CLI   SVERONLY,C'E'       ERRORS-ONLY UPLOAD?                          
         BE    DPX                 YES -- DON'T DELETE LEFTOVER PUBS            
*                                                                               
         L     R4,ATIA                                                          
         CLC   =C'PUBLIST*',2(R4)                                               
         BE    *+6                                                              
         DC    H'0'                FIRST OBJECT MUST BE PUBLIST                 
*                                                                               
         ICM   R2,15,10(R4)        NUMBER OF ENTRIES IN PUBLIST                 
         BZ    DPX                 NOTHING TO DELETE                            
*                                                                               
         LA    R4,14(R4)           POINT TO FIRST PUB IN LIST                   
         L     R5,AMINBLK          MINIO PARAMETER BLOCK                        
         USING MINBLKD,R5                                                       
*                                                                               
DP10     CLI   7(R4),C'N'          WAS THIS PUB SEEN IN UPLOAD FILE?            
         BNE   DP40                YES -- LEAVE IT ALONE                        
*                                                                               
         LA    RF,MINMKEY          PUT MEDIA, PUBCODE INTO MASTER KEY           
         MVC   PEUPMED-PEUPKEY(,RF),0(R4)                                       
         MVC   PEUPPUB-PEUPKEY(,RF),1(R4)                                       
         MVC   SVMED,0(R4)         FOR PROCDEL SUBROUTINE                       
         MVC   SVPUB,1(R4)         FOR PROCDEL SUBROUTINE                       
         MVI   MINCHG,C'N'         MINIO RECORD NOT CHANGED (YET)               
*                                                                               
         XC    MINEKEY,MINEKEY     DELETE ALL INSERTIONS                        
         MVI   MINEKEY,X'90'                                                    
DP15     GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,MINEEOF      ANY MORE INSERTIONS FOR THIS PUB?            
         BE    DP30                NO -- NOTHING ELSE TO DELETE                 
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DP20     LA    R3,ELEMENT          THE ELEMENT TO BE DELETED                    
         USING PEUPMEL,R3                                                       
         CLI   PEUPPLIN,0          IS THERE A PRINTPAK LINE NUMBER?             
         BNE   DP25                YES                                          
*                                                                               
         MVI   WORK,X'90'          NO -- DELETE THE ELEMENT                     
         MVC   WORK+1(6),PEUPUNID  SAVE ELEMENT KEY                             
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         MVC   MINEKEY(7),WORK     RESTORE ELEMENT KEY                          
         B     DP15                                                             
*                                                                               
DP25     CLC   PEUPPDT,SVPESTS3    WITHIN PARTIAL ESTIMATE DATE RANGE?          
         BL    DP27                                                             
         CLC   PEUPPDT,SVPESTE3                                                 
         BH    DP27                NO                                           
*                                                                               
         BAS   RE,PROCDEL          PUT DELETE OBJECTS TO TEMPSTR                
*                                                                               
         OI    PEUPUSRC,X'80'                                                   
         MVI   WORK,X'90'          SAVE ELEMENT KEY                             
         MVC   WORK+1(6),PEUPUNID                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(7),WORK     RESTORE ELEMENT KEY                          
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
DP27     GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,MINEEOF      ANY MORE INSERTIONS FOR THIS PUB?            
         BE    DP30                                                             
         CLI   MINERR,0                                                         
         BE    DP20                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
DP30     CLI   MINCHG,C'Y'         ANY CHANGE TO THIS PUB'S DATA?               
         BNE   DP40                                                             
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         LA    RE,MYOBJECT         BUILD END-OF-PUB OBJECT HERE                 
         USING PEOPODAT,RE                                                      
         XC    PEOPODAT(PEOPLENQ),PEOPODAT                                      
         MVC   PEOPTYPE,=C'EOP*'                                                
         DROP  RE                                                               
         LH    R3,=Y(PEOPLENQ)     SEND END-OF-PUB OBJECT TO TEMPSTR            
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R3)                                        
         BNE   EXIT                                                             
*                                                                               
DP40     LA    R4,8(R4)            BUMP TO NEXT PUB IN TABLE                    
         BCT   R2,DP10                                                          
*                                                                               
DPX      B     XIT                                                              
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
         MVC   GLVXTOSY,=C'PRI'    TO THE PRINT SYSTEM                          
         MVC   GLVXTOPR,=C'BUY'    BUY PROGRAM                                  
********                                                                        
** NO-OPED 6/20/00 AS PER MEL H.                                                
******** OI    GLVXFLG1,GLV1GOTO   CALL BASE PROGRAM ON TRANSFER                
********                                                                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   MDACTION,=Y(ER10XCTL) GLOBBER COULDN'T PUT OUT XCTL ELEM         
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',SVMED,1,GLVPRMD                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',SVCLT,3,GLVPRCLT                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',SVPRD,3,GLVPRPRD                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+5(3)                                                  
         GOTO1 GLOBBER,DMCB,=C'PUTD',DUB,3,GLVPREST                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
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
         XC    LASTPUB,LASTPUB                                                  
         GOTO1 TMPOPEN,DMCB,=C'GTX',MYOBJLNQ,(3,0)                              
         BNE   EXIT                                                             
*                                                                               
CNFLOOP  GOTO1 GETTMP,DMCB,MYOBJECT GET INSERTION DATA FROM TEMPSTR             
         BNE   EXIT                                                             
*                                                                               
         MVC   DATALEN,DMCB+4      COPY LENGTH OF DATA                          
*                                                                               
         CLI   EOTFLAG,C'Y'        DONE IF NO MORE                              
         BNE   CNF10                                                            
*                                                                               
         CLI   ENDOBJCT,C'Y'       END-OF-DATA OBJECT?                          
         BE    CNF05                                                            
*                                                                               
*                                  PUT END-OF-BLOCK OBJECT OUT                  
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
*                                                                               
CNF05    MVI   ENDBLOCK,C'N'                                                    
         MVI   CONFFLAG,0                                                       
         GOTO1 TMPCLOSE            DONE WITH READING TEMPFILE                   
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         B     CNFX                EXIT ROUTINE                                 
*                                                                               
CNF10    LA    R4,MYOBJECT         RETURNED OBJECT                              
         CLC   =C'INS*',10(R4)     INSERTION OBJECT?                            
         BE    CNF100                                                           
         CLC   =C'EOP*',8(R4)      END-OF-PUB OBJECT?                           
         BE    CNF400                                                           
         CLC   =C'DEL*',8(R4)      DELETE OBJECT?                               
         BE    CNF500                                                           
         CLC   =C'HDR*',8(R4)      HEADER OBJECT?                               
         BE    CNF600                                                           
         DC    H'0'                                                             
*                                                                               
         USING PINSD,R4                                                         
CNF100   MVC   SVLINE,PINSLINE     SAVE LINE NUMBER FROM PRINTPAK               
         MVC   INSNUM,PINSRNUM     INSERTION NUMBER                             
         MVC   SVACT,PINSACTN      ACTION                                       
*                                                                               
         MVC   SVPUB,PINSPUBP      PRINTPAK PUBCODE                             
         CLC   SVPUB,XFF                                                        
         BNE   CNF110              IT'S VALID                                   
         MVC   SUBRECNO,=C'00'     NO SUB-RECORD                                
         MVC   ERRNUM,PINSERNO     SAVE ERROR NUMBER                            
         MVC   ERFLDNUM,PINSERF    SAVE FIELD NUMBER IN ERROR                   
         B     CNF310              WRITE ERROR OBJECT                           
*                                                                               
CNF110   XC    SVDATE,SVDATE                                                    
         CLC   PINSDATE(2),XFF     SKIP IF DELIBERATE BAD DATE                  
         BE    CNF120                                                           
*                                                                               
         MVC   DUB,PINSDATE        INSERTION DATE                               
         CLC   DUB+6(2),SPACES     ANY DAY GIVEN? (PINSDAY)                     
         BNE   *+10                                                             
         MVC   DUB+6(2),=C'01'     FORCE TO 1ST DAY (FOR DATCON)                
         GOTO1 DATCON,DMCB,(9,DUB),(3,SVDATE)                                   
         CLC   PINSDATE+6(2),SPACES     ANY DAY GIVEN? (PINSDAY)                
         BNE   CNF120                                                           
         MVI   SVDATE+2,0          NO -- CHANGE X'01' TO NULL                   
*                                                                               
CNF120   CLC   LASTPUB,SVPUB       CHANGE OF PUB?                               
         BE    *+14                NO, SAME AS LAST TIME                        
         MVC   LASTPUB,SVPUB       REMEMBER THIS PUBCODE                        
         MVI   MINCHG,C'N'         NO CHANGES TO MINIO RECORD YET               
*                                                                               
         LA    RF,MINMKEY          PUT MEDIA,PUBCODE IN MASTER KEY              
         MVC   PEUPMED-PEUPKEY(,RF),SVMED                                       
         MVC   PEUPPUB-PEUPKEY(,RF),SVPUB                                       
*                                                                               
         OC    SVDATE,SVDATE                                                    
         BZ    CNF180                                                           
*                                                                               
         CLI   SVACT,C'A'          WAS THIS AN ADD?                             
         BNE   CNF150              NO                                           
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
         CLC   PINSUNIQ+8(7),SPACES                                             
         JNH   CNF130K                                                          
         MVC   SVXUPIDC,PINSUNIQ                                                
         BRAS  RE,GETXUPEL         EXTENDED UPLOAD ELEMENT FOUND?               
         J     CNF130M                                                          
CNF130K  GOTO1 SIXPACK,DMCB,PINSUNIQ,MINEKEY+1,8                                
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
CNF130M  CLI   MINERR,0                                                         
         BE    CNF160                                                           
         DC    H'0'                DIDN'T WE ADD THIS EARLIER?                  
*                                                                               
CNF150   CLI   SVACT,C'C'          WAS THIS A CHANGE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    PINSERNO,PINSERNO   ANY ERROR IN INS* RECORD?                    
         BNZ   CNF180              YES                                          
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
         CLC   PINSUNIQ+8(7),SPACES                                             
         JNH   CNF156K                                                          
         MVC   SVXUPIDC,PINSUNIQ                                                
         BRAS  RE,GETXUPEL         EXTENDED UPLOAD ELEMENT FOUND?               
         J     CNF156M                                                          
CNF156K  GOTO1 SIXPACK,DMCB,PINSUNIQ,MINEKEY+1,8                                
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
CNF156M  CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                BUT WE TOLD PRT/BUY TO CHANGE IT!            
*                                                                               
CNF160   OC    PINSERNO,PINSERNO   ANY ERROR IN INS* RECORD?                    
         BZ    CNF200              NO                                           
*                                                                               
CNF180   MVC   SUBRECNO,=C'00'     NO SUB-RECORD                                
         MVC   ERRNUM,PINSERNO     SAVE ERROR NUMBER                            
*                                                                               
         CLC   ERRNUM,=H'20'       IF INVALID DATE ERROR                        
         BNE   *+8                                                              
         CLI   PINSDATE+2,X'FE'    AND CHANGING INSERTION DATE                  
         BNE   *+10                                                             
         MVC   ERRNUM,=H'99'          CHANGE ERROR MESSAGE                      
*                                                                               
         MVC   ERFLDNUM,PINSERF    SAVE FIELD NUMBER IN ERROR                   
         B     CNF300              WRITE ERROR OBJECT                           
*                                                                               
CNF200   SR    R0,R0                                                            
         ICM   R0,3,0(R4)          LENGTH OF SUB-OBJECT                         
         AR    R4,R0               BUMP TO NEXT PORTION                         
         DROP  R4                                                               
*                                                                               
         CLC   =C'OPT*',10(R4)                                                  
         BE    CNF250                                                           
         CLC   =C'CCL*',10(R4)                                                  
         BE    CNF260                                                           
         CLC   =C'ACH*',10(R4)                                                  
         BE    CNF265                                                           
         CLC   =C'ZZZ*',10(R4)                                                  
         BE    CNF270                                                           
         CLC   =C'EIN*',10(R4)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,WORK             INSERTION WAS OK                             
         USING DSTATUSD,R2                                                      
         CLI   SVACT,C'A'                                                       
         BNE   CNF210                                                           
*                                                                               
         MVC   DSTATYPE,=C'03'     RECORD ADDED STATUS                          
         LA    R1,ELEMENT                                                       
         USING PEUPMEL,R1                                                       
         MVC   PEUPPLIN,SVLINE     PUT PRINTPAK LINE NUMBER IN ELEMENT          
         DROP  R1                                                               
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     CNF220                                                           
*                                                                               
CNF210   CLI   SVACT,C'C'                                                       
         BE    *+6                                                              
         DC    H'0'                WHAT IS THIS ACTION CODE?                    
         MVC   DSTATYPE,=C'02'     RECORD MODIFIED STATUS                       
*                                                                               
CNF220   SR    R0,R0                                                            
         ICM   R0,3,INSNUM         INSERTION NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTARECN,DUB+4(4)                                                
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         B     CNFPUT                                                           
         DROP  R2                                                               
*                                                                               
         USING POPTD,R4                                                         
CNF250   OC    POPTERNO,POPTERNO   ANY ERROR IN OPT* RECORD?                    
         BZ    CNF200              NO                                           
         MVC   ERRNUM,POPTERNO     SAVE ERROR NUMBER                            
         MVC   SUBRECNO,POPTRNUM   SAVE SUB-RECORD NUMBER                       
         MVI   ERFLDNUM,0          NO FIELD NUMBER ON THESE                     
         B     CNF300              WRITE ERROR OBJECT                           
         DROP  R4                                                               
*                                                                               
         USING PCCLD,R4                                                         
CNF260   OC    PCCLERNO,PCCLERNO   ANY ERROR IN CCL* RECORD?                    
         BZ    CNF200              NO                                           
*                                                                               
         MVC   ERRNUM,PCCLERNO     SAVE ERROR NUMBER                            
         MVC   SUBRECNO,PCCLRNUM   SAVE SUB-RECORD NUMBER                       
         MVI   ERFLDNUM,0          NO FIELD NUMBER ON THESE                     
         B     CNF310              WRITE ERROR OBJECT                           
*                                  NEED TO SKIP THE DELETION OF BUY             
*                                  FROM MINIO DATASET                           
*                                  BUY MAKES IT TO FILE BUT                     
*                                  CUST COL DATA DOESN'T                        
         DROP  R4                                                               
*                                                                               
         USING PACHD,R4                                                         
CNF265   OC    PACHERNO,PACHERNO   ANY ERROR IN ACH* RECORD?                    
         BZ    CNF200              NO                                           
*                                                                               
         MVC   ERRNUM,PACHERNO     SAVE ERROR NUMBER                            
         MVC   SUBRECNO,PACHRNUM   SAVE SUB-RECORD NUMBER                       
         MVI   ERFLDNUM,0          NO FIELD NUMBER ON THESE                     
         B     CNF310              WRITE ERROR OBJECT                           
*                                  NEED TO SKIP THE DELETION OF BUY             
*                                  FROM MINIO DATASET                           
*                                  BUY MAKES IT TO FILE BUT                     
*                                  ADDITIONAL CHARGE DATA DOESN'T               
         DROP  R4                                                               
*                                                                               
         USING PZZZD,R4                                                         
CNF270   OC    PZZZERNO,PZZZERNO   ANY ERROR IN ZZZ* RECORD?                    
         BZ    CNF200              NO                                           
         MVC   ERRNUM,PZZZERNO     SAVE ERROR NUMBER                            
         MVC   SUBRECNO,PZZZRNUM   SAVE SUB-RECORD NUMBER                       
         MVI   ERFLDNUM,0          NO FIELD NUMBER ON THESE                     
         DROP  R4                                                               
*                                                                               
CNF300   CLI   SVACT,C'A'          DID WE ADD THIS EARLIER?                     
         BNE   CNF310                                                           
         OC    SVDATE,SVDATE       DON'T DELETE IF BAD INSERTION DATE           
         BZ    CNF310                                                           
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
CNF310   LA    R2,WORK             BUILD ERROR OBJECT                           
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         SR    R0,R0                                                            
         ICM   R0,3,INSNUM         INSERTION NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTARECN,DUB+4(4)                                                
         MVC   DSTASUBR,SUBRECNO   SUB-RECORD NUMBER                            
         ZIC   R0,ERFLDNUM         FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
         SR    R0,R0                                                            
         ICM   R0,3,ERRNUM         ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         B     CNFPUT                                                           
         DROP  R2                                                               
*                                                                               
CNF400   CLI   MINCHG,C'Y'         ANY CHANGE TO THIS PUB'S DATA?               
         BNE   CNFCHK                                                           
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    CNFCHK                                                           
         DC    H'0'                                                             
*                                                                               
         USING PDELODAT,R4                                                      
CNF500   OC    PDELERNO,PDELERNO   ANY ERROR ON DELETE?                         
         BZ    CNF550              NO -- DELETE WAS SUCCESSFUL                  
         LA    R2,WORK             BUILD ERROR OBJECT                           
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         SR    R0,R0                                                            
         ICM   R0,3,PDELRNUM       INSERTION NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTARECN,DUB+4(4)                                                
         MVC   DSTASUBR,=C'00'     NO SUB-RECORD NUMBER                         
         MVC   DSTAFLDN,=C'15'     LOCK RECORD ON UNIQUE INSERTION ID           
         SR    R0,R0                                                            
         ICM   R0,3,PDELERNO       ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         B     CNFPUT                                                           
         DROP  R2                                                               
*                                                                               
CNF550   LA    RF,MINMKEY                                                       
         MVC   PEUPMED-PEUPKEY(,RF),PDELMED                                     
         MVC   PEUPPUB-PEUPKEY(,RF),PDELPUB                                     
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
         CLC   PDELUNIQ+8(7),SPACES                                             
         JNH   CNF550K                                                          
         MVC   SVXUPIDC,PDELUNIQ                                                
         BRAS  RE,GETXUPEL         EXTENDED UPLOAD ELEMENT FOUND?               
         J     CNF550M                                                          
CNF550K  GOTO1 SIXPACK,DMCB,PDELUNIQ,MINEKEY+1,8                                
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
CNF550M  CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIDN'T WE ADD THIS EARLIER?                  
         DROP  R4                                                               
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         L     R1,TOTALDEL         INCREMENT DELETE COUNTER                     
         LA    R1,1(R1)                                                         
         ST    R1,TOTALDEL                                                      
         B     CNFCHK                                                           
*                                                                               
         USING PHDRODAT,R4                                                      
CNF600   OC    PHDRERNO,PHDRERNO   ANY ERROR ON HEADER?                         
         BZ    CNFCHK              NO                                           
*                                                                               
         LA    R2,WORK             YES -- BUILD ERROR OBJECT                    
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         MVC   DSTARECN,=C'00000'  RECORD ZERO = HEADER                         
         MVC   DSTASUBR,=C'00'     NO SUB-RECORD                                
         ZIC   R0,PHDRERF          FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
         SR    R0,R0                                                            
         ICM   R0,3,PHDRERNO       ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
         DROP  R2                                                               
*                                                                               
         L     R3,=A(ITPINSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOB,0  NOTHING ELSE AFTER THIS                    
         BNE   EXIT                                                             
         MVI   ENDBLOCK,C'N'                                                    
         MVI   CONFFLAG,0                                                       
         GOTO1 TMPCLOSE            DONE WITH READING TEMPFILE                   
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         B     CNFX                                                             
         DROP  R4                                                               
*                                                                               
CNFPUT   GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
CNFCHK   CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG NOT SET,                
         BNE   CNFLOOP             LOOP BACK UNTIL NO MORE OBJECTS              
*                                                                               
         USING MINBLKD,R5                                                       
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         MVI   CONFFLAG,X'FF'      WE HAVE SOMETHING IN OUR BUFFER              
*                                                                               
CNFX     B     XIT                 RETURN TO THE CALLER                         
         EJECT                                                                  
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
GT20     MVC   TYPENUM,=A(ITPINHDR)                                             
         CLC   =C'HDR*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITPININS)                                             
         CLC   =C'INS*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITPINADD)                                             
         CLC   =C'OPT*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITPINZZZ)                                             
         CLC   =C'ZZZ*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITPINEIN)                                             
         CLC   =C'EIN*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITPINEPB)                                             
         CLC   =C'EOP*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITPINSIG)                                             
         CLC   =C'SIG*',3(R3)                                                   
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
EXIT     L     RD,SAVEDRD                                                       
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
XFF      DC    64X'FF'                                                          
SPACES   DC    64C' '                                                           
         SPACE 3                                                                
         GETEL R4,33,ELCODE                                                     
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'PPPAY02 - TEST FOR OFFLINE LOCKING  - TSTLOK'                   
*=====================================================================*         
*                                                                     *         
*     TEST DATA LOCKED BY ANOTHER APPLICATION                         *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
*EXIT    CC        ZERO     - FILE NOT LOCKED                         *         
*                  NON ZERO - FILE LOCKED                             *         
*                                                                     *         
*=====================================================================*         
         SPACE 1                                                                
         DS    0D                                                               
TSTLOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         XC    LKUPKEY,LKUPKEY     ESTABLISH KEY FOR LOCK                       
         LA    R2,LKUPKEY                                                       
         USING LKKEYD,R2                                                        
*                                                                               
*        TEST FOR LOCKED CLIENT                                                 
*                                                                               
         MVC   LOCKAGY,SVAGY       AGENCY                                       
         MVC   LOCKRTY,=C'BC'      CLIENT LOCK                                  
         MVC   LOCKMED,SVMED       MEDIA                                        
         MVC   LOCKCLT,SVCLT       CLIENT                                       
*                                                                               
TSTLOKCL DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
*                                                                               
         USING LOCKUPD,R1          ESTABLISH LOCKET PARAMETER LIST              
*                                                                               
         CLI   LKERR,1             IF FILES LOCKED FOR CLIENT                   
         BE    TSTLOKER               SEND LOCKED MESSAGE                       
         CLI   LKERR,2             IF TABLE BUSY                                
         BE    TSTLOKCL               ASK SAME QUESTION AGAIN                   
*                                                                               
TSTLOKMX DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQUAL CC                                 
         B     TSTLOKX                                                          
*                                                                               
TSTLOKER DS    0H                                                               
         LHI   RF,PPELOCKD     FILE LOCKED                                      
*                                                                               
         STCM  RF,3,ERRNUM     SET ERROR MESSAGE CODE                           
*                                                                               
         LTR   RB,RB               SET NOT EQUAL CC                             
         B     TSTLOKX                                                          
*                                                                               
TSTLOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETXUPEL NTR1  BASE=*,LABEL=*      GET EXTENDED UPLOAD ELEMENT                  
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         XC    SVXUPIDP,SVXUPIDP                                                
*                                                                               
GXUPEL20 XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         J     GXUPEL40                                                         
*                                                                               
GXUPEL30 GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
*                                                                               
GXUPEL40 CLI   MINERR,MINEEOF      ANY MORE INSERTIONS FOR THIS PUB?            
         JE    GXUPEL80            NO                                           
         CLI   MINERR,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ELEMENT                                                       
         USING PEUPMEL,R3                                                       
         CLC   SVXUPIDC(L'PEUPUNIQ),PEUPUNIQ                                    
         JNE   GXUPEL30                                                         
         CLC   SVXUPIDC+L'PEUPUNIQ(L'PEUPUIQX),PEUPUIQX                         
         JNE   GXUPEL30                                                         
         MVC   SVXUPIDP,PEUPUNID                                                
         J     SETCCEQ                                                          
*                                                                               
GXUPEL80 J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETXUPID NTR1  BASE=*,LABEL=*      GET EXTENDED UPLOAD ID                       
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     R6,AIO                                                           
         USING UINSD,R6                                                         
         CLC   UINSTYPE,=C'INS*'                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         XC    SVBYSER#,SVBYSER#                                                
*                                                                               
         LA    R4,KEY                                                           
         USING PBUYREC,R4                                                       
         XC    PBUYKEY,PBUYKEY     BUILD BUY RECORD KEY                         
         MVC   PBUYKAGY,SVAGY                                                   
         MVC   PBUYKMED,SVMED                                                   
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,SVCLT                                                   
         MVC   PBUYKPRD,SVPRD                                                   
         MVC   PBUYKPUB(6),SVPUB                                                
         MVC   PBUYKDAT,SVDATE                                                  
         CLI   PBUYKDAT+2,0        MONTH ONLY?                                  
         JNE   *+8                                                              
         MVI   PBUYKDAT+2,1        FORCE TO FIRST OF MONTH                      
         MVC   PBUYKEST,SVEST                                                   
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',KEYSAVE,KEY               
         CLI   DMCB+8,0                                                         
         JE    GETBS#36                                                         
         DC    H'0'                                                             
*                                                                               
GETBS#34 GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',KEYSAVE,KEY               
         CLI   DMCB+8,0                                                         
         JE    GETBS#36                                                         
         DC    H'0'                                                             
*                                                                               
GETBS#36 CLC   KEY(24),KEYSAVE     FOUND BUY (UP TO LINE#)?                     
         JNE   GETBS#60                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFIL',KEY+27,          +        
               ABUYREC,DMWORK                                                   
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,ABUYREC                                                       
         USING PIUPEL,R4                                                        
         MVI   ELCODE,PIUPELCQ                                                  
         BRAS  RE,NXTELEM                                                       
         JNE   GETBS#34                                                         
         CLI   PIUPELLN,PIUPELXQ   EXTENDED UPLOAD ELEMENT?                     
         JNE   GETBS#34                                                         
         CLC   UINSUNIQ(L'PIUPUSEQ),PIUPUSEQ                                    
         JNE   GETBS#34                                                         
         CLC   UINSUNIQ+L'PIUPUSEQ(L'PIUPUQXT),PIUPUQXT                         
         JNE   GETBS#34                                                         
         DROP  R4                                                               
*                                                                               
         L     R4,ABUYREC                                                       
         USING PSERELEM,R4                                                      
         MVI   ELCODE,PSERELEQ                                                  
         BRAS  RE,NXTELEM                                                       
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVBYSER#,PSERNUM                                                 
         DROP  R4                                                               
*                                                                               
GETBS#60 OC    SVBYSER#,SVBYSER#   FOUND BUY SERIAL#?                           
         JNZ   GETBS#80                                                         
         CP    BYSERCNT,=P'0'      ALREADY CALCULATED BEFORE?                   
         JNH   GETBS#62                                                         
         AP    BYSERCNT,=P'1'      BUY SERIAL# COUNTER BUMPED UP BY 1           
         ZAP   SVBYSER#,BYSERCNT                                                
         J     GETBS#80                                                         
GETBS#62 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PSERKEY,R4                                                       
         MVC   PSERKAGY,SVAGY                                                   
         MVC   PSERKMED,SVMED                                                   
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKCLT,SVCLT                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'PRTDIR',KEYSAVE,KEY           
         CLC   KEY(7),KEYSAVE      HAVE SERIAL# FOR MEDIA/CLIENT?               
         JE    *+14                                                             
         ZAP   DUB,=P'1'           FIRST TIME                                   
         J     GETBS#64                                                         
         ZAP   DUB,=P'1000000000'                                               
         SP    DUB,PSERKNUM                                                     
         AP    DUB,=P'1'           NEXT BUY SERIAL#                             
GETBS#64 ZAP   SVBYSER#,DUB                                                     
         ZAP   BYSERCNT,DUB        COPY SERIAL# INTO COUNTER                    
         DROP  R4                                                               
*                                                                               
GETBS#80 OC    SVBYSER#,SVBYSER#   HAVE BUY SERIAL#?                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CP    SVBYSER#,=P'0'      ZERO?                                        
         JH    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
NXTELEM  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLC   ELCODE,0(R4)                                                     
         JE    NXTELEMX            CC IS EQUAL                                  
         CLI   0(R4),0                                                          
         JNE   NEXTEL                                                           
         LTR   R4,R4               CC IS NOT EQUAL                              
NXTELEMX BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTMAD14TST                                                     
         PRINT ON                                                               
         SPACE 3                                                                
       ++INCLUDE CTMADFFD                                                       
         EJECT                                                                  
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE PRTUPLOADD                                                     
         EJECT                                                                  
       ++INCLUDE PEUPLREC                                                       
         SPACE 3                                                                
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* CTMADEQUS                                                                     
* DDGLVXCTLD                                                                    
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE CTMADEQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE PRGENFILE                                                      
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
PERVAL   DS    A                   A(PERVAL)                                    
PUBVAL   DS    A                   A(PUBVAL)                                    
RECUP    DS    A                   A(RECUP)                                     
SIXPACK  DS    A                   A(SIXPACK)                                   
AWORKSTR DS    A                   A(UNSAVED WORKING STORAGE)                   
AMINBLK  DS    A                   A(MINIO PARAMETER BLOCK)                     
AMINBUFF DS    A                   A(MINIO I/O BUFFERS)                         
AMINRECT DS    A                   A(MINIO RECORD TABLE)                        
ABUYREC  DS    A                   A(PRINTPAK BUY RECORD I/O AREA)              
*                                                                               
TOTALDEL DS    F                   TOTAL NUMBER INSERTIONS DELETED              
INSNUM   DS    H                   INSERTION NUMBER                             
SUBNUM   DS    H                   SUB-RECORD NUMBER                            
MYOBJLEN DS    H                   LENGTH OF OBJECTS I BUILD                    
ERRNUM   DS    H                   ERROR NUMBER                                 
ELCODE   DS    X                   FOR GETEL MACRO                              
CONFFLAG DS    X                   X'FF'= CONFIRM OBJECTS ARE > 1 FRAME         
TSTDATAF DS    C                   WHICH TEST RECORDS TO USE                    
ENDOBJCT DS    C                   END OF OBJECTS FLAG                          
ENDBLOCK DS    C                   END OF BLOCK FLAG                            
ERFLDNUM DS    X                   FIELD NUMBER IN ERROR                        
TMPOPEND DS    C                   'Y' = TEMPSTR IS OPEN                        
MINCHG   DS    C                   'Y' = MINIO RECORD CHANGED                   
PUBCHG   DS    C                   'Y' = PUB DATA OBJECTS WRITTEN               
INSCHG   DS    C                   'Y' = INSERTION CHANGED                      
INTERUPT DS    C                   'Y' = USER HIT INTERRUPT BUTTON              
SVLINE   DS    X                   PRINTPAK LINE NUMBER                         
SVACT    DS    C                   ACTION CODE                                  
SUBRECNO DS    CL2                 SUB-RECORD WITHIN INSERTION                  
PUBLSTST DS    C                   'Y' = PUBLIST BUILD WAS STARTED              
PPAKPUB  DS    XL6                 PRINTPAK PUBCODE                             
*                                                                               
SVAGY    DS    CL2                 AGENCY                                       
SVMED    DS    C                   MEDIA                                        
SVCLT    DS    CL3                 CLIENT                                       
SVPRD    DS    CL3                 PRODUCT                                      
SVEST    DS    XL2                 ESTIMATE                                     
SVPUB    DS    XL6                 PUB CODE                                     
SVDATE   DS    XL3                 LAST INSERTION DATE FROM INPUT FILE          
SVBYSER# DS    PL(L'PSERNUM)       BUY SERIAL#                                  
SVUPDFLG DS    C                   UPDATES ONLY FLAG                            
SVERONLY DS    C                   ERRORS ONLY FLAG                             
SVVER    DS    CL3                 PC PROGRAM VERSION                           
SVUPTYPE DS    CL3                 UPLOAD TYPE (STANDARD, ETC.)                 
SVPESTFL DS    C                   PARTIAL ESTIMATE FLAG                        
SVPESTST DS    CL8                 PARTIAL ESTIMATE START DATE                  
SVPESTEN DS    CL8                 PARTIAL ESTIMATE END DATE                    
SVPESTS3 DS    XL3                 PARTIAL ESTIMATE START DATE (BINARY)         
SVPESTE3 DS    XL3                 PARTIAL ESTIMATE END DATE (BINARY)           
LASTPUB  DS    XL6                 LAST PUB RETRIEVED FROM INPUT FILE           
SVHDROBJ DS    CL(PHDRLENQ)        SAVED HEADER OBJECT                          
*                                                                               
MYOBJECT DS    (MYOBJLNQ)C         BUILD A TWA OBJECT HERE                      
MYOBJLNQ EQU   2048                MAXIMUM OBJECT LENGTH                        
*                                                                               
LKUPKEY  DS    XL16                KEY FOR LOCK TESTING                         
BYSERCNT DS    PL(L'SVBYSER#)      BUY SERIAL# COUNTER FOR EXTENED UPID         
SVXUPIDC DS    CL15                EXTENDED UPLOAD ID - CHAR FORMAT             
SVXUPIDP DS    PL6                 EXTENDED UPLOAD ID - SERIALIZED PACK         
*                                                                               
         DS    (4096-(*-OVERD))X   $MAD CONTROLLER ONLY SAVES 4K                
         EJECT                                                                  
*                   ELEMENTS                                                    
*                                                                               
USIGD    DSECT                                                                  
USIGVER  DS    CL3                 VERSION NUMBER                               
USIGERR  DS    C                   ERRORS ONLY?                                 
         SPACE 3                                                                
UHDRD    DSECT                                                                  
UHDRSYS  DS    C                   SYSTEM (ALWAYS C'P')                         
UHDRAGID DS    CL2                 BUYING AGENCY ID                             
UHDRMED  DS    C                   MEDIA CODE                                   
UHDRBUYR DS    CL3                 BUYER ID                                     
UHDRCLT  DS    CL3                 CLIENT CODE                                  
UHDRPRD  DS    CL3                 PRODUCT CODE                                 
UHDREST  DS    CL3                 ESTIMATE NUMBER                              
UHDRUPDO DS    C                   UPDATES ONLY? (Y/N)                          
UHDRTYPE DS    CL3                 SOURCE TYPE                                  
*                                   'S  ' = STANDARD                            
         DS    CL10                SPARE                                        
UHDRTSTB DS    C                   TEST BUYS (Y/N)                              
UHDRPEST DS    C                   PARTIAL ESTIMATE UPLOAD?                     
UHDRSTDT DS    CL8                 PARTIAL ESTIMATE START DATE                  
UHDRENDT DS    CL8                 PARTIAL ESTIMATE END DATE                    
         DS    CL100                                                            
         SPACE 3                                                                
UINSD    DSECT                                                                  
UINSTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'INS*')                 
UINSPUB  DS    CL8                 PUBCODE                                      
UINSZNED DS    0CL5                                                             
UINSZONE DS    CL2                 ZONE                                         
UINSEDTN DS    CL3                 EDITION                                      
UINSDATE DS    0CL8                INSERTION DATE                               
UINSYEAR DS    CL4                                                              
UINSMNTH DS    CL2                                                              
UINSDAY  DS    CL2                                                              
UINSADCD DS    CL6                 AD CODE                                      
UINSSPAC DS    CL17                SPACE DESCRIPTION                            
UINSSHOW DS    CL3                 SHOWING                                      
UINSREG  DS    CL4                 REGULAR                                      
UINSILL  DS    CL4                 ILLUMINATED                                  
UINSCOST DS    CL11                INSERTION COST                               
UINSRATE DS    CL10                UNIT RATE                                    
UINSPDSC DS    CL2                 PREMIUM DESCRIPTION                          
UINSPCST DS    CL8                 PREMIUM COST                                 
UINSCLOS DS    CL8                 CLOSING DATE                                 
UINSSALE DS    CL8                 ON-SALE DATE                                 
UINSMATC DS    CL8                 MATERIAL CLOSE DATE                          
UINSUNIQ DS    CL15                UNIQUE INSERTION ID                          
         DS    CL15                                                             
UINSSPRD DS    C                   SPREAD INDICATOR (Y/N)                       
         DS    CL77                SPARE                                        
UINSINUM DS    CL5                 INSERTION NUMBER                             
UINSSER# DS    PL5                 UPLOAD SERIAL#                               
         SPACE 3                                                                
UOPTD    DSECT                                                                  
UOPTTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'OPT*')                 
         DS    CL47                DATA                                         
UOPTDATX EQU   *                                                                
         SPACE 3                                                                
UCCLD    DSECT                                                                  
UCCLTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'CCL*')                 
UCCLCODE DS    CL12                CUSTOM COLUMN CODE                           
UCCLDATA DS    CL60                CUSTOM COLUMN DATA                           
UCCLDATX EQU   *                                                                
         SPACE 3                                                                
UACHD    DSECT                                                                  
UACHTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'ACH*')                 
UACHCODE DS    CL2                 ADDITIONAL CHARGE CODE                       
UACHCHG  DS    CL12                ADDITIONAL CHARGE                            
UACHCOM  DS    CL1                 ADDITIONAL CHARGE - COMMISION - Y/N          
UACHCOMP DS    CL6                 ADDITIOANL CHARGE COMMISION %                
UACHCD   DS    CL1                 ADDITIOANL CHARGE - CD - Y/N                 
UACHDATX EQU   *                                                                
         SPACE 3                                                                
UZZZD    DSECT                                                                  
UZZZTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'ZZZ*')                 
         DS    CL47                DATA                                         
UZZZDATX EQU   *                                                                
         SPACE 3                                                                
DSTATUSD DSECT                                                                  
DSTATYPE DS    CL2                 STATUS OBJECT TYPE                           
*                                   C'01' = RECORD IN ERROR                     
*                                   C'02' = RECORD MODIFIED OK                  
*                                   C'03' = RECORD ADDED OK                     
*                                   C'04' = TOTAL NUMBER DELETED                
*                                   C'05' = INITIALIZATION ERROR                
DSTARECN DS    CL5                 RECORD NUMBER (ONE BASED)                    
*                                   ZERO = ERROR IN HDR* RECORD                 
         ORG   DSTARECN                                                         
DSTATOTD DS    CL5                 TOTAL NUMBER OF INSERTIONS DELETED           
*                                   ONLY WHEN TYPE = C'04'                      
         ORG   DSTARECN                                                         
DSTAINIE DS    CL5                 INITIALIZATION REASON CODE                   
*                                   (ONLY WHEN TYPE = C'05')                    
*                                   C'01' = PC PROGRAM EXPIRED                  
DSTAOKLQ EQU   *-DSTATUSD          L'OBJECT                                     
*                                     (WHEN TYPE = C'02',03','04','05')         
DSTASUBR DS    CL2                 SUB-RECORD NUMBER (ZERO-BASED)               
DSTAFLDN DS    CL2                 FIELD NUMBER IN ERROR                        
DSTAMSGN DS    CL5                 ERROR MESSAGE NUMBER                         
DSTAERLQ EQU   *-DSTATUSD          L'OBJECT WHEN TYPE = C'01'                   
         EJECT                                                                  
*              EQUATES                                                          
*                                                                               
LENMINRC EQU   1000                LENGTH OF A PRINT MINIO RECORD               
NUMMNBUF EQU   2                   NUMBER OF MINIO RECORD BUFFERS               
MINBUFSZ EQU   NUMMNBUF*LENMINRC   TOTAL SPACE FOR MINIO BUFFERS                
MINRCTBL EQU   1000                MINIO RECORD TABLE SIZE                      
         SPACE 3                                                                
WORKD    DSECT                                                                  
*                                                                               
MINBLK   DS    (MINBLKL)X          MINIO PARAMETER BLOCK                        
*                                                                               
MINRECTB DS    (MINRCTBL)X         MINIO RECORD TABLE                           
MINBUFFS DS    (MINBUFSZ)X         MINIO I/O BUFFERS                            
*                                                                               
BUYREC   DS    3000X               PRINTPAK BUY RECORD BUFFER                   
*                                                                               
WORKX    EQU   *                                                                
         SPACE 3                                                                
** FALOCKUPD                                                                    
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                 BASE PUB ONLY                                
         DS    XL2                                                              
*                                                                               
         PRINT ON                                                               
*PPERREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTMAD14   05/15/19'                                      
         END                                                                    
