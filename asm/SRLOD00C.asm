*          DATA SET SRLOD00C   AT LEVEL 024 AS OF 10/09/01                      
*PHASE T11700A                                                                  
         TITLE '$LOAD - LOAD PHASES TO DATASPACE FROM LOADLIB'                  
LOAD     CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,*$LOAD**,RA,CLEAR=YES,RR=RE                                
         USING WORKD,RC            RC=A(W/S)                                    
DS       USING PROGSPCD,DKEY                                                    
LL       USING PROGSPCD,LKEY                                                    
         USING PROGSPCD,PSREC                                                   
*                                                                               
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         ST    R1,SAVER1                                                        
         MVC   IPARMS,0(R1)                                                     
         MVI   HDRN,1              SET DEFAULT MESSAGE                          
*                                                                               
         L     R9,ATWA                                                          
         USING SRLODFFD,R9         R9=A(TWA)                                    
*                                                                               
         BRAS  RE,INIT             INITIALISE                                   
         BNE   MAINX                                                            
         BRAS  RE,VPASSWD          VALIDATE ID/PASSWD/REASON                    
         BNE   MAINX                                                            
*                                                                               
         LA    R8,SRVL1H           R8=A(CURRENT SCREEN LINE)                    
         USING SRVPD,R8                                                         
*                                                                               
MAIN02   BRAS  RE,VLINE            VALIDATE INFORMATION ON LOAD LINE            
         BL    MAINX               ERROR ON LINE                                
*                                                                               
         LA    R8,SRVNEXT(R8)      BUMP TO NEXT INPUT LINE                      
         LA    R1,SRVL6H                                                        
         CR    R8,R1                                                            
         BNH   MAIN02                                                           
         LA    R2,SRVIDH                                                        
         XC    FERN,FERN                                                        
*                                                                               
MAINX    MVC   OPERPARM,SPACES     CLEAR PARMS                                  
         MVC   OPERP1,PROFSID      SET PROFID AND REASON                        
         MVC   OPERRCD,REASON                                                   
         XC    LOGID,LOGID         CLEAR LOGREC                                 
         CLI   OPERP1,C' '         ANYTHING WORTH LOGGING                       
         BE    *+8                                                              
         BRAS  RE,WTO              WRITE IT                                     
*                                                                               
         CLI   FERN,0              ANY ERRORS?                                  
         BE    *+12                NO                                           
         BRAS  RE,DISERR                                                        
         B     XMOD                                                             
*                                                                               
         BRAS  RE,DISOK                                                         
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE IBM LOAD LIB LANGUAGE & PHASE & LEVEL (TSPPOOL) AT SRVP1   *         
***********************************************************************         
         SPACE 1                                                                
VLINE    NTR1  ,                                                                
         XC    DKEY,DKEY                                                        
         XC    LKEY,LKEY                                                        
         XC    PSREC,PSREC                                                      
*                                                                               
         BRAS  RE,VPHASE           VALIDATE PHASE NAME                          
         BNE   EXITL                                                            
         BRAS  RE,VLANG            VALIDATE LANGUAGE                            
         BNE   EXITL                                                            
         BRAS  RE,VACT             VALIDATE ACTIONS (AND NODES)                 
         BNE   EXITL                                                            
*                                                                               
         CLI   ACTN,AADD           WANT TO DO AN ADD?                           
         BNE   VLIN02              NO                                           
         BRAS  RE,ADDIT                                                         
         BNE   EXITL                                                            
         MVI   FERN,0                                                           
         MVI   HDRN,3                                                           
         B     VLIN06                                                           
*                                                                               
VLIN02   CLI   ACTN,ADEL           WANT TO DO A DELETE?                         
         BNE   VLIN04              NO                                           
         BRAS  RE,DELIT                                                         
         BNE   EXITL                                                            
         MVI   FERN,0                                                           
         MVI   HDRN,5                                                           
         B     VLIN06                                                           
*                                                                               
VLIN04   BRAS  RE,LOADIT           THEN YOU MUST BE LOADING                     
         BNE   EXITL                                                            
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
*                                                                               
* THE FOLLOWING CALL TO CALLOV REBUILDS THE CRNDX (T00AFF)                      
* IN AN ATTEMPT TO ENSURE THAT EVERYONE GETS A NEWLY LOADED PHASE               
*                                                                               
VLIN06   GOTO1 ACALLOV,DMCB,0,(C'C',0),0                                        
*                                                                               
         MVC   LOGID,=C'$LD.'      SET UP LOGREC                                
         MVC   LOGLUID,SAVELUID                                                 
         MVC   LOGTIME,SAVETIME                                                 
         MVC   LOGPHS,SRVP1        P1,2,3 LOGREC                                
         MVC   LOGLEV,SRVP2                                                     
         MVC   LOGACT,SRVP3                                                     
         GOTO1 ALOGGER,LOGREC      LOG TO ADRFILE                               
*                                                                               
         MVC   OPERMSG,SPACES                                                   
         MVC   OPERID,=CL9'+LOAD+' SET UP OPER MSG                              
         MVC   OPERFAC,=C'(FACPAK)'                                             
         MVC   OPERFAC+4(3),SYSSHRT                                             
         MVC   OPERP1,SRVP1        SHOW P1 TO P4 OPER MSG                       
         MVC   OPERP2,SRVP2                                                     
         MVC   OPERP3,SRVP3                                                     
         MVC   OPERP4,SRVP4                                                     
         OC    OPERMSG,SPACES                                                   
         BRAS  RE,WTO              WRITE PARMS TO OPER                          
*                                                                               
         MVC   SRVP4,SPACES        REDISPLAY NODES                              
         CLI   NODES,0                                                          
         BE    EXITOK                                                           
*                                                                               
         LA    R2,SRVP4H                                                        
         USING FHD,R2                                                           
         GOTO1 AHEXOUT,DMCB,NODES,SRVP4,1,=C'TOG'                               
         MVI   FHOL,2                                                           
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PHASE NAME INPUT FIELD AT SRVP1                            *         
***********************************************************************         
         SPACE 1                                                                
VPHASE   NTR1  ,                                                                
         LA    R2,SRVP1H           R2=A(PHASE NAME INPUT FIELD)                 
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         MVI   FERRDSP,0                                                        
         MVI   FERN,0                                                           
*                                                                               
         CLI   FHIL,0              INPUT?                                       
         BE    EXITH               NO - SKIP THIS LINE                          
*                                                                               
         CLI   FHIL,5              FORMAT IS XSPPOO(L)                          
         BH    VPH02                                                            
         CLC   =C'FIX',FHDA                                                     
         BE    *+12                                                             
         MVI   FERN,23             TOO LITTLE INPUT                             
         B     EXITL                                                            
*                                                                               
         BRAS  RE,FIXHOLE          FIX HOLE THEN EXIT                           
         BNE   EXITL                                                            
         MVI   HDRN,4                                                           
         MVI   FERN,0                                                           
         B     EXITL                                                            
*                                                                               
VPH02    CLI   FHIL,7                                                           
         BL    VPH04               NO LEVEL INPUT                               
         BE    *+12                                                             
         MVI   FERN,23             TOO MUCH INPUT                               
         B     EXITL                                                            
*                                                                               
         MVC   LL.PSLVL,FHDA+6                                                  
         CLI   FHDA+6,C'A'         LEVEL A,B OR C ONLY VALID INPUTS             
         BE    VPH04                                                            
         CLI   FHDA+6,C'B'                                                      
         BE    VPH04                                                            
         CLI   FHDA+6,C'C'                                                      
         BE    VPH04                                                            
*&&US*&& CLI   FHDA+6,C'S'         S LOADS TO LIVE IN DATASPACE (US)            
*&&UK*&& CLI   FHDA+6,C'X'         X LOADS TO LIVE IN DATASPACE (UK)            
         BE    VPH04                                                            
*                                                                               
         MVI   FERRDSP,6           INVALID LEVEL STAMP                          
         MVI   FERN,32                                                          
         B     EXITL                                                            
*                                                                               
VPH04    MVC   LL.PSLANG,FHDA      SAVE FIRST LETTER OF PHASE NAME              
         MVC   DS.PSLANG,FHDA                                                   
*                                                                               
         L     R1,ALANGTAB                                                      
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1                                                      
         MVC   LANGDEF,LANGOVL1                                                 
VPH06    CLC   LANGOVL1,LL.PSLANG  TEST LANGUAGE OVERLAY LETTER                 
         BE    VPH08                                                            
         BXLE  R1,RE,VPH06                                                      
         MVI   FERRDSP,0                                                        
         MVI   FERN,29             INVALID LANGUAGE OVERLAY                     
         B     EXITL                                                            
*                                                                               
VPH08    MVC   DUB(6),FHDA         MOVE PHASE NAME TO CALLOV PARMLIST           
         MVI   DUB,C'0'                                                         
         GOTO1 AHEXIN,DMCB,DUB,LL.PSNAME,6                                      
         OC    12(4,R1),12(R1)                                                  
         BNZ   VPH10                                                            
         MVI   FERRDSP,1                                                        
         MVI   FERN,28                                                          
         B     EXITL                                                            
*                                                                               
VPH10    MVC   DS.PSNAME,LL.PSNAME                                              
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DDS PHASE LIST LEVEL & LANGAGE & CORERES OPTION AT SRVP2   *         
***********************************************************************         
         SPACE 1                                                                
VLANG    NTR1  ,                                                                
         XC    CORERES,CORERES                                                  
         LA    R2,SRVP2H           R2=A(PHASE NAME INPUT FIELD)                 
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 ASCANNER,DMCB,FHD,(X'83',WORK)                                   
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   FERN,21             FORMAT INVALID FOR SCANNER                   
         B     EXITL                                                            
         CLI   4(R1),4                                                          
         BL    *+12                                                             
         MVI   FERN,21             FORMAT INVALID FOR SCANNER                   
         B     EXITL                                                            
*                                                                               
         XR    R0,R0                                                            
         IC    R0,4(R1)            SAVE NUMBER OF FIELDS INPUT                  
*                                                                               
         LA    R3,WORK                                                          
         USING SCANBLKD,R3                                                      
         CLI   SC2NDLEN,0          TRY FOR L=X (PHASE LIST LANGUAGE)            
         BE    VLA06               NO                                           
*                                                                               
         MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,2                                                           
         CLI   SC1STLEN,1                                                       
         BNE   EXITL                                                            
         CLI   SC1STFLD,C'L'                                                    
         BNE   EXITL                                                            
*                                                                               
         L     R1,ALANGTAB         SEARCH LANG TABLE FOR OLAY LETTER            
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING LANGTABD,R1                                                      
VLA02    CLC   LANGOVL1,SC2NDFLD   TEST LANGUAGE OVERLAY LETTER                 
         BE    VLA04                                                            
         BXLE  R1,RE,VLA02                                                      
         MVC   FERRDSP,SC2NDNUM                                                 
         MVI   FERN,25             INVALID LANGUAGE OVERRIDE LETTER             
         B     EXITL                                                            
*                                                                               
VLA04    MVC   DS.PSLANG,SC2NDFLD  SET DSPACE LANGUAGE CODE                     
         AHI   R3,SCBLKLQ          NEXT FIELD IN LIST                           
         BCT   R0,VLA06                                                         
         B     EXITOK                                                           
*                                                                               
VLA06    CLI   SC2NDLEN,0          VALIDATE TEST LEVEL                          
         BE    VLA08                                                            
         MVC   FERRDSP,SC2NDNUM                                                 
         MVI   FERN,22             NO SECOND VALUE ALLOWED                      
         B     EXITL                                                            
*                                                                               
VLA08    CLI   SC1STLEN,1          ONLY ONE CHAR ALLOWED FOR LEVEL              
         BNE   VLA16                                                            
         CLI   SC1STFLD,C'A'       LEVEL MUST BE A/B/C                          
         BL    VLA12                                                            
         CLI   SC1STFLD,C'C'                                                    
         BH    VLA12                                                            
         MVC   DS.PSLVL,SC1STFLD   SET DDS PHASE LIST LEVEL                     
         B     VLA14                                                            
*                                                                               
VLA12    MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,26             INVALID LEVEL                                
         B     EXITL                                                            
*                                                                               
VLA14    AHI   R3,SCBLKLQ                                                       
         BCT   R0,*+8                                                           
         B     EXITOK                                                           
*                                                                               
         CLI   SC2NDLEN,0          VALIDATE OPTION                              
         BE    VLA16                                                            
         MVC   FERRDSP,SC2NDNUM                                                 
         MVI   FERN,22             NO SECOND VALUE ALLOWED                      
         B     EXITL                                                            
*                                                                               
VLA16    CLI   SC1STLEN,8          1-7 CHARACTERS ALLOWED                       
         BL    VLA18                                                            
         MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,23             INPUT VALUE IS TOO LONG                      
         B     EXITL                                                            
*                                                                               
VLA18    LA    RF,P2OPTS           LOAD OPTIONS                                 
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
*                                                                               
VLA20    CLI   0(RF),255                                                        
         BE    VLA22                                                            
         EX    R1,*+8                                                           
         BE    VLA24                                                            
         CLC   SC1STFLD(0),0(RF)                                                
         AHI   RF,L'P2OPTS                                                      
         B     VLA20                                                            
*                                                                               
VLA22    MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,2              INVALID INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
VLA24    MVC   CORERES,7(RF)       SET OPTION FROM TABLE                        
         B     EXITOK                                                           
         DROP  R2,R3                                                            
*                                                                               
P2OPTS   DS    0CL8                TABLE OF VALID OPTIONS FOR P2                
         DC    CL7'CORERES',C'Y'                                                
         DC    CL7'DUMMY  ',C'Q'                                                
         DC    CL7'DISKRES',C'D'                                                
         DC    CL7'REPLACE',C'R'                                                
         DC    CL7'REMOVE ',C'X'                                                
         DC    AL1(255)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE LOAD ACTION AT SRVP3                                       *         
***********************************************************************         
         SPACE 1                                                                
VACT     NTR1  ,                                                                
         LA    R2,SRVP3H           POINT TO THIRD PARAMETER                     
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         XC    ACTN,ACTN                                                        
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,FHIL                                                        
         BZ    EXITOK                                                           
         BCTR  R1,0                R1=L'COMPARE                                 
         LA    RF,P3ACTS                                                        
*                                  SEARCH VALID ACTION TABLE                    
VAC02    CLI   0(RF),X'FF'                                                      
         BNE   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         EX    R1,*+8                                                           
         BE    VAC04                                                            
         CLC   FHDA(0),0(RF)       MATCH INPUT WITH TABLE                       
         AHI   RF,L'P3ACTS                                                      
         B     VAC02                                                            
*                                                                               
VAC04    MVC   ACTN,7(RF)          SET ACTION FROM TABLE                        
*                                                                               
         CLI   ACTN,ADEL           DELETE ACTION?                               
         BNE   VAC06               NO                                           
         CLC   DS.PSLANG,LANGDEF   CAN DELETE ANY FOREIGN PHASE                 
         BNE   VAC06                                                            
         CLI   DS.PSLVL,C' '       CANNOT DELETE ENGLISH PROD PHASE             
         BH    VAC06                                                            
*                                                                               
         LA    R0,SRVP2H           SET CANNOT DELETE PRODUCTION PHASE           
         ST    R0,FADRH                                                         
         MVI   FERN,13                                                          
         MVI   FERRDSP,0                                                        
         B     EXITL                                                            
*                                                                               
VAC06    CLI   ACTN,AADD           SET DEFAULT NODES IF ACTN=ADD                
         BNE   VAC08                                                            
         CLI   DS.PSOVR,SCRNQ      SEE IF SCREEN ASSUMPTION MET                 
         BH    VAC08                                                            
*                                                                               
         MVI   NODES,X'01'                                                      
         CLI   DS.PSOVR,X'00'      BASE PROGRAM?                                
         BE    *+8                                                              
         MVI   NODES,X'10'                                                      
*                                                                               
VAC08    BRAS  RE,VNODES           VALIDATE NODES                               
         BNE   EXITL                                                            
         B     EXITOK                                                           
*                                                                               
P3ACTS   DS    0CL8                TABLE OF VALID ACTIONS FOR P3                
         DC    CL7'LOAD   ',AL1(ALOAD)                                          
*??      DC    CL7'FORCE  ',AL1(AFORCE)                                         
         DC    CL7'ADD    ',AL1(AADD)                                           
         DC    CL7'DELETE ',AL1(ADEL)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE NODES AT SRVP4                                             *         
***********************************************************************         
         SPACE 1                                                                
VNODES   NTR1  ,                                                                
         LA    R2,SRVP4H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0              ANY INPUT?                                   
         BNE   VNO02               YES                                          
         CLI   DS.PSLVL,0          ADDING LIVE PHASE?                           
         BNE   EXITOK              NO                                           
         MVI   FERN,16             YOU MUST SPECIFY THE NODES                   
         B     EXITL                                                            
*                                                                               
VNO02    CLI   FHIL,2              INPUT MUST BE LENGTH 2                       
         BE    *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         TM    FHII,FHIIHE         INPUT MUST BE VALID HEX                      
         BO    *+12                                                             
         MVI   FERN,27                                                          
         B     EXITL                                                            
*                                                                               
         GOTO1 AHEXIN,DMCB,FHDA,NODES,2                                         
         OC    12(4,R1),12(R1)                                                  
         BNZ   EXITOK                                                           
         MVI   FERN,2                                                           
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET PHASE LIST MAP FOR PROGRAM LANG/LEVEL USING DIRECTORY READ      *         
***********************************************************************         
         SPACE 1                                                                
GETDIR   NTR1  ,                                                                
         L     R2,AUTL                                                          
         USING UTLD,R2                                                          
         MVC   BYTE,DS.PSLVL                                                    
         NI    BYTE,TTESTLVL                                                    
         NI    TTEST,255-TTESTLVL                                               
         OI    TTEST,SRTTEST       RESET READ INDICATOR                         
         OC    TTEST,BYTE          SET PHASE LIST LEVEL                         
*                                                                               
         L     R1,ALANGTAB                                                      
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1                                                      
GDI02    CLC   LANGOVL1,DS.PSLANG  TEST LANGUAGE OVERLAY LETTER                 
         BE    GDI04                                                            
         BXLE  R1,RE,GDI02                                                      
         DC    H'0'                                                             
*                                                                               
GDI04    MVC   TLANG,LANGCODE      SET LANGUAGE CODE                            
         CLI   TLANG,2                                                          
         BH    *+6                                                              
         MVI   TLANG,0                                                          
         DROP  R1                                                               
*                                                                               
         ICM   R0,7,DS.PSNAME                                                   
         ICM   R0,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(R0),(C'D',0)                                 
*                                                                               
         MVI   INPHLST,C'Y'        SET SOMETHING MATCHED                        
         CLI   4(R1),255                                                        
         BNE   *+8                                                              
         MVI   INPHLST,C'N'        SET NOTHING MATCHED                          
*                                                                               
         MVI   TTEST,SRTTEST       RESET READ INDICATOR                         
         MVI   TLANG,0             RESET LANGUAGE CODE                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ENTRY TO PHASE LIST                                             *         
***********************************************************************         
         SPACE 1                                                                
ADDIT    NTR1  ,                                                                
         LA    R2,SRVP1H           POINT TO FIRST INPUT FIELD                   
         ST    R2,FADRH                                                         
*                                                                               
         BRAS  RE,GETDIR           GET CURRENT BEST MATCH                       
         CLC   PROGSPCD(PSKEYL),DS.PROGSPCD                                     
         BNE   *+12                                                             
         MVI   FERN,8              PHASE ALREADY EXISTS                         
         B     EXITL                                                            
*                                                                               
         CLI   DS.PSLVL,0          ADDING 'LIVE' PHASE TO LIST?                 
         BNE   *+10                NO                                           
         XC    PSREC,PSREC         RESET EVERYTHING                             
         MVC   PSREC(PSKEYL),DS.PROGSPCD                                        
*                                                                               
         CLI   DS.PSLVL,0          ADDING 'LIVE' PHASE TO LIST?                 
         BNE   *+10                NO                                           
         MVC   PSNODES,NODES       SPECIFY NODES                                
*                                                                               
         MVC   NODES,PSNODES                                                    
         CLI   PSNODES,0           ZERO NODE PROGRAMS ARE CORE-RESIDENT         
         BE    *+8                 ANYTHING ELSE MUST BE LOADED TO CORE         
         NI    PSFLAG1,255-CTPHSCRQ                                             
*                                                                               
         CLI   CORERES,C'Y'        ADD CORE-RESIDENT?                           
         BNE   ADD02                                                            
         OI    PSFLAG1,CTPHSCRQ                                                 
         CLI   PSSYS,0             NO SCREENS FOR SYSTEM 0                      
         BE    ADD04                                                            
         CLI   DS.PSOVR,SCRNQ      CORE RESIDENT SCREEN?                        
         BL    *+8                                                              
         OI    PSFLAG1,CTPHSCSQ                                                 
         B     ADD04                                                            
*                                                                               
ADD02    CLI   CORERES,C'Q'        DUMMY PHASE                                  
         BNE   ADD04                                                            
         OI    PSFLAG1,CTPHSDMQ+CTPHSCRQ                                        
         MVC   PSADR,=AL4(PSADRD)                                               
         CLI   DS.PSLVL,0                                                       
         BNE   ADD06                                                            
         MVI   FERN,30             CAN'T ADD LIVE DUMMY PHASE                   
         B     EXITL                                                            
*                                                                               
ADD04    MVC   PSADR,=AL4(PSADRL)  SET WANT TO DO LOAD                          
         XC    PSLENH,PSLENH       RESET ALL LENGTHS                            
         XC    PSLEN,PSLEN                                                      
*                                                                               
ADD06    ICM   R0,7,PSNAME                                                      
         ICM   R0,8,=C'W'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(R0),(C'D',0)                                 
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,LOADIT           NOW GO LOAD ENTRY                            
         BE    EXITOK              LOADED OK                                    
*                                                                               
         BRAS  RE,DELIT            DELETE BAD DIRECTORY RECORD                  
         MVI   FERN,7                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* LOAD A PHASE TO THE DATASPACE FROM LOADLIB                          *         
***********************************************************************         
         SPACE 1                                                                
LOADIT   NTR1  ,                                                                
         BRAS  RE,GETDIR           GET DSPACE RECORD THAT BEST MATCHES          
*                                                                               
         LA    R2,SRVP1H           POINT TO FIRST INPUT FIELD                   
         ST    R2,FADRH                                                         
         CLI   INPHLST,C'Y'        SPPOO MUST EXIST IN PHASE LIST               
         BE    *+12                                                             
         MVI   FERN,5                                                           
         B     EXITL                                                            
*                                                                               
         MVC   SPSREC,PSREC                                                     
*                                                                               
         CLC   PSLVL,DS.PSLVL      MAKE SURE LEVEL AGREES                       
         BE    *+12                                                             
         MVI   FERN,11             NO ENTRY FOR LEVEL                           
         B     EXITL                                                            
*                                                                               
         CLC   PSLANG,DS.PSLANG    MAKE SURE LANGUAGE AGREES                    
         BE    *+12                                                             
         MVI   FERN,10             NO ENTRY FOR LANGUAGE                        
         B     EXITL                                                            
*                                                                               
         TM    PSFLAG1,CTPHSDMQ    DUMMY PHASE?                                 
         BZ    LIT02               NO                                           
         OI    LL.PSFLAG1,CTPHSDMQ+CTPHSCRQ                                     
         CLI   CORERES,C'R'        DUMMY PHASES MUST BE REPLACE LOADED          
         BE    LIT02                                                            
         MVI   FERN,19                                                          
         B     EXITL                                                            
*                                                                               
LIT02    CLI   CORERES,C'Y'        ADD CORE-RESIDENT?                           
         BNE   *+8                                                              
         OI    LL.PSFLAG1,CTPHSCRQ                                              
         MVC   NODES,PSNODES                                                    
*                                                                               
         L     R2,AUTL                                                          
         USING UTLD,R2                                                          
         OI    TTEST,TTESTCIL                                                   
         GOTO1 ACALLOV,DMCB,DKEY,(C'W',LKEY),0                                  
         NI    TTEST,255-TTESTCIL                                               
         CLI   4(R1),0                                                          
         BE    EXITOK                                                           
         MVI   FERN,7                                                           
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE A PHASE ENTRY                                                *         
***********************************************************************         
         SPACE 1                                                                
DELIT    NTR1  ,                                                                
         BRAS  RE,GETDIR           GET DSPACE RECORD THAT BEST MATCHES          
*                                                                               
         LA    R2,SRVP1H           POINT TO FIRST INPUT FIELD                   
         ST    R2,FADRH                                                         
         CLI   INPHLST,C'Y'        SPPOO MUST EXIST IN PHASE LIST               
         BE    *+12                                                             
         MVI   FERN,5                                                           
         B     EXITL                                                            
*                                                                               
         MVC   SPSREC,PSREC                                                     
*                                                                               
         CLC   PSLVL,DS.PSLVL      MAKE SURE LEVEL AGREES                       
         BE    *+12                                                             
         MVI   FERN,11             NO ENTRY FOR LEVEL                           
         B     EXITL                                                            
*                                                                               
         CLC   PSLANG,DS.PSLANG    MAKE SURE LANGUAGE AGREES                    
         BE    *+12                                                             
         MVI   FERN,10             NO ENTRY FOR LANGUAGE                        
         B     EXITL                                                            
*                                                                               
         MVI   PSSTAT,255          SET DELETED FLAG                             
         ICM   R0,7,PSNAME                                                      
         ICM   R0,8,=C'W'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(R0),(C'D',0)                                 
         CLI   4(R1),255                                                        
         BNE   EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL TO CLEAN UP THE HOLE (IF YOU NEED TO)                          *         
***********************************************************************         
         SPACE 1                                                                
FIXHOLE  NTR1  ,                                                                
         XR    R0,R0               FIX UP HOLE                                  
         ICM   R0,8,=C'H'                                                       
         GOTO1 ACALLOV,DMCB,0,(R0),0                                            
         CLI   4(R1),255                                                        
         BNE   EXITOK                                                           
         MVI   FERN,31                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         ST    RF,AERRMSGS                                                      
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         ST    RF,AOKMSGS                                                       
*                                                                               
         L     R2,ASYSFACS                                                      
         USING SYSFACD,R2                                                       
         MVC   ASSB,VSSB                                                        
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ADMOD000,VDMOD000                                                
         MVC   AWCTYPE,VWCTYPE                                                  
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ALOGGER,VLOGGER                                                  
*                                                                               
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         DROP  R2                                                               
*                                                                               
         L     R2,AUTL                                                          
         USING UTLD,R2                                                          
         MVC   SAVETEST,TTEST                                                   
         MVC   SAVELANG,TLANG      SAVE USER LANGUAGE CODE                      
         MVI   TLANG,0                                                          
         MVC   SAVELUID,TSYM       SAVE LUID                                    
         MVI   TTEST,SRTTEST       SET CALLOV VALUE FOR TTEST                   
*                                                                               
         L     R2,ASSB             SAVE ADDR & LEN OF C/R TEST BUFFER           
         USING SSBD,R2                                                          
         MVC   ALANGTAB,SSBALANG   SAVE ADDR OF LANGUAGE TABLE                  
         MVC   SYSID,SSBSYSID      SAVE FACPAK SYSTEM ID                        
         MVC   SYSCH,SSBSYSCH      SAVE FACPAK SYSTEM MESSAGE CHR               
         MVC   SYSNAME,SSBSYSN4    SAVE FACPAK SYSTEM NAME                      
         MVC   SYSSHRT,SSBSYSNA    SAVE SHORT SYSTEM NAME                       
*                                                                               
         MVI   PRODSYS,C'Y'        THIS IS A PRODUCTION SYSTEM                  
         TM    SSBSYSFL,X'80'      X'80' SET FOR TEST SYSTEMS                   
         BNO   *+8                                                              
         MVI   PRODSYS,C'N'        NOT A PRODUCTION SYSTEM                      
*                                                                               
         GOTO1 ATICTOC,DMCB,C'SGET'                                             
         MVC   SAVETIME,DMCB       SAVE TIME FOR LOGREC                         
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* IF PRODUCTION SYSTEM, GET ID/PASSWORD AND REASON FOR LOAD           *         
***********************************************************************         
         SPACE 1                                                                
VPASSWD  NTR1  ,                                                                
         MVC   PROFSID,SPACES      P3=PROFSID(,PASSWORD)                        
         MVC   PASSWORD,SPACES                                                  
         CLI   PRODSYS,C'Y'        DONT DO THIS FOR TEST SYSTEMS                
         BNE   EXITOK                                                           
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         CLI   FHIL,0              ANY INPUT TO LOAD?                           
         BE    EXITL               NO - DON'T BOTHER TO CHECK THEN              
*                                                                               
         LA    R2,SRVID1H          GET ID/PASSWORD                              
         CLI   FHIL,0              INPUT?                                       
         BNE   VPW02               YES                                          
         ST    R2,FADRH                                                         
         MVI   FERN,20                                                          
         B     EXITL                                                            
*                                                                               
VPW02    GOTO1 ASCANNER,DMCB,SRVID1H,(X'82',WORK)                               
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   FERN,21             FORMAT INVALID FOR SCANNER                   
         B     EXITL                                                            
         CLI   4(R1),3                                                          
         BL    *+12                                                             
         MVI   FERN,21             FORMAT INVALID FOR SCANNER                   
         B     EXITL                                                            
*                                                                               
         MVC   BYTE,4(R1)          SAVE NUMBER OF FIELDS INPUT                  
*                                                                               
         LA    R3,WORK                                                          
         USING SCANBLKD,R3                                                      
         MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC2NDLEN,0                                                       
         BE    *+12                                                             
         MVI   FERN,22             NO SECOND PARAMETER ALLOWED                  
         B     EXITL                                                            
*                                                                               
         CLI   SC1STLEN,L'PROFSID                                               
         BNH   *+12                                                             
         MVI   FERN,23             TOO MUCH INPUT                               
         B     EXITL                                                            
*                                                                               
         MVC   PROFSID,SC1STFLD    SAVE ID VALUE INPUT                          
*                                                                               
         CLI   BYTE,2              TWO FIELDS INPUT?                            
         BNE   VPW04               NO                                           
         AHI   R3,SCBLKLQ                                                       
         MVC   FERRDSP,SC1STNUM                                                 
         CLI   SC2NDLEN,0                                                       
         BE    *+12                                                             
         MVI   FERN,22             NO SECOND PARAMETER ALLOWED                  
         B     EXITL                                                            
*                                                                               
         CLI   SC1STLEN,L'PASSWORD                                              
         BNH   *+12                                                             
         MVI   FERN,23             TOO MUCH INPUT                               
         B     EXITL                                                            
*                                                                               
         MVC   PASSWORD,SC1STFLD   SAVE PASSWORD INPUT                          
         DROP  R3                                                               
*                                                                               
VPW04    MVC   REASON,SPACES                                                    
         XC    FERRDSP,FERRDSP                                                  
*                                                                               
         LA    R2,SRVID2H          NOW SAVE REASON FOR LOAD                     
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    EXITOK                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   REASON(0),FHDA                                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'E#'        SET ERROR                                  
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,FERN                                                          
         CHI   R0,255                                                           
         BNE   *+6                                                              
         XR    R0,R0                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         LTR   R0,R0               SPECIAL ERROR MESSAGE?                       
         BNE   DERR02              NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,FERNA                                                       
         MVC   0(30,R4),0(RF)                                                   
         B     DERR04                                                           
*                                                                               
DERR02   AHI   R0,-1                                                            
         MHI   R0,EMSGL                                                         
         L     RF,AERRMSGS                                                      
         AR    RF,R0                                                            
         MVC   0(EMSGL,R4),0(RF)                                                
*                                                                               
DERR04   ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOK    NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'I#'      SET OK                                       
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,HDRN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         AHI   R0,-1                                                            
         MHI   R0,IMSGL                                                         
         L     RF,AOKMSGS                                                       
         AR    RF,R0                                                            
         MVC   0(IMSGL,R4),0(RF)                                                
*                                                                               
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,SRVL1H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
* NTRY: R8     = SRV LINE FOR THIS LOAD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING SRVPD,R8                                                         
WTO      NTR1  ,                                                                
         CLI   PRODSYS,C'N'        ONLY FOR PROD SYSTEMS                        
         BE    EXITOK                                                           
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPERMSG,OPERMSGL,C'LVL1'                   
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     EXITOK                                                           
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND HANDY ROUTINES                                      *         
***********************************************************************         
         SPACE 1                                                                
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     L     RD,SAVERD                                                        
         L     RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   TTEST,SAVETEST                                                   
         XMOD1 ,                                                                
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND EQUATES                                                *         
***********************************************************************         
         SPACE 1                                                                
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
*                                                                               
ALOAD    EQU   0                                                                
AFORCE   EQU   1                                                                
AADD     EQU   2                                                                
ADEL     EQU   3                                                                
*                                                                               
*&&UK                                                                           
SCRNQ    EQU   X'9F'               ASSUME SCREEN IF GT X'9F'                    
*&&                                                                             
*&&US                                                                           
SCRNQ    EQU   X'AA'               ASSUME SCREEN IF GT X'AA'                    
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
SPACES   DC    CL80' '                                                          
DUMMYPHS DC    X'FFFFFF'           D/A OF DUMMY C/R PHASES                      
SRTTEST  EQU   X'04'               SPECIAL VALUE FOR CALLOV                     
         EJECT                                                                  
***********************************************************************         
* OK MESSAGES                                                         *         
***********************************************************************         
         SPACE 1                                                                
OKMSGS   DS    0CL(IMSGL)                                                       
OK1      DC    CL(IMSGL)'Enter phases to load'                                  
OK2      DC    CL(IMSGL)'Action completed - system updated'                     
OK3      DC    CL(IMSGL)'Action completed - phase added to system'              
OK4      DC    CL(IMSGL)'Hole reset ok - enter next request'                    
OK5      DC    CL(IMSGL)'Action completed - phase deleted'                      
         SPACE 1                                                                
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Missing input field'                                   
ERR02    DC    CL(EMSGL)'Invalid input field'                                   
ERR03    DC    CL(EMSGL)'Invalid level indicator'                               
ERR04    DC    CL(EMSGL)'Invalid phase name format'                             
ERR05    DC    CL(EMSGL)'Phase name is not in phase list'                       
ERR06    DC    CL(EMSGL)'NO MORE SPARE AVAILABLE IN PHASE LIST'                 
ERR07    DC    CL(EMSGL)'Phase is not in LOADLIB'                               
ERR08    DC    CL(EMSGL)'Phase is already in phase list'                        
ERR09    DC    CL(EMSGL)'Phase has been deleted'                                
ERR10    DC    CL(EMSGL)'Language entry not found for phase'                    
ERR11    DC    CL(EMSGL)'Level entry not found for phase'                       
ERR12    DC    CL(EMSGL)'NOT ENOUGH CORE TO LOAD CORE RESIDENT PHASE'           
ERR13    DC    CL(EMSGL)'Cannot delete a production phase'                      
ERR14    DC    CL(EMSGL)'CANNOT REPLACE A LARGER CORE RESIDENT PHASE'           
ERR15    DC    CL(EMSGL)'CANNOT REMOVE A PRODUCTION VERSION'                    
ERR16    DC    CL(EMSGL)'You must specify nodes if adding a live phase'         
ERR17    DC    CL(EMSGL)'REQUEST NOT VALID ON THIS SYSTEM'                      
ERR18    DC    CL(EMSGL)'WARNING REFRESH ERROR - #NNN - 1ST=XXXXXX'             
ERR19    DC    CL(EMSGL)'Dummy phases require ''REPLACE'' option'               
ERR20    DC    CL(EMSGL)'Id and Reason required for production load'            
ERR21    DC    CL(EMSGL)'Field format must be valid for SCANNER'                
ERR22    DC    CL(EMSGL)'A second value is not allowed for this input'          
ERR23    DC    CL(EMSGL)'Value input is too long'                               
ERR24    DC    CL(EMSGL)'Value input is too short'                              
ERR25    DC    CL(EMSGL)'Invalid language override letter'                      
ERR26    DC    CL(EMSGL)'Level must be <live>, A, B or C'                       
ERR27    DC    CL(EMSGL)'Input field must be valid HEX'                         
ERR28    DC    CL(EMSGL)'The next 5 bytes must be valid HEX'                    
ERR29    DC    CL(EMSGL)'Language code is not known - see FALANG'               
ERR30    DC    CL(EMSGL)'Can''t add live DUMMY phase'                           
ERR31    DC    CL(EMSGL)'You are not the only task - can''t fix hole'           
*&&US                                                                           
ERR32    DC    CL(EMSGL)'Level must be <live>, A, B, C or S'                    
*&&                                                                             
*&&UK                                                                           
ERR32    DC    CL(EMSGL)'Level must be <live>, A, B, C or X'                    
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVER1   DS    A                                                                
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
AERRMSGS DS    A                                                                
AOKMSGS  DS    A                                                                
ALANGTAB DS    A                                                                
*                                                                               
ASSB     DS    V                                                                
ACALLOV  DS    V                                                                
ADMOD000 DS    V                                                                
AWCTYPE  DS    V                                                                
ATICTOC  DS    V                                                                
ALOGGER  DS    V                                                                
ASCANNER DS    V                                                                
*                                                                               
AHEXIN   DS    V                                                                
AHEXOUT  DS    V                                                                
*                                                                               
PSREC    DS    XL(PROGSPCL)                                                     
SPSREC   DS    XL(PROGSPCL)                                                     
*                                                                               
PROFSID  DS    CL8                                                              
PASSWORD DS    CL8                                                              
REASON   DS    CL16                                                             
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
PHASE    DS    XL3                                                              
DKEY     DS    XL(PROGSPCL)                                                     
LKEY     DS    XL(PROGSPCL)                                                     
*                                                                               
ACTN     DS    C                   CURRENT ACTION                               
INPHLST  DS    C                   SET TO C'Y' IF IN DDS PHASE LIST             
NODES    DS    C                                                                
CORERES  DS    C                                                                
FLAG     DS    C                                                                
SAVELANG DS    X                                                                
SAVETEST DS    X                                                                
SAVELUID DS    CL8                                                              
SAVETIME DS    PL4                                                              
LANGDEF  DS    C                                                                
IBMOVL1  DS    C                   IBM LOAD LIBRARY LANG CHR                    
IBMLANG  DS    X                   IBM LOAD LIBRARY LANG CODE (0-9)             
DDSOVL1  DS    C                   DDS PHASE LIST LANG CHR                      
PRODSYS  DS    C                                                                
SYSID    DS    X                                                                
SYSCH    DS    C                                                                
SYSNAME  DS    CL4                                                              
SYSSHRT  DS    CL3                                                              
*                                                                               
MSG      DS    CL60                                                             
WORK     DS    CL128                                                            
*                                                                               
OPERMSG  DS    0CL53'+LOAD+   P1...... P2...... P3...... P4...... (F'           
OPERID   DS    CL8                                                              
         DS    CL1                                                              
OPERPARM DS    0CL36                                                            
OPERP1   DS    CL8                                                              
         DS    CL1                                                              
OPERRCD  DS    0CL16                                                            
OPERP2   DS    CL8                                                              
         DS    CL1                                                              
OPERP3   DS    CL8                                                              
         DS    CL1                                                              
OPERP4   DS    CL8                                                              
         DS    CL1                                                              
OPERFAC  DS    CL8                                                              
OPERMSGL EQU   *-OPERMSG                                                        
*                                                                               
LOGREC   DS    0CL38                                                            
LOGID    DS    CL4                 $LD.                                         
LOGLUID  DS    CL8                 LUID                                         
LOGTIME  DS    PL4                 0HHMMSS+                                     
LOGPHS   DS    CL8                 TSPPNNL                                      
LOGLEV   DS    CL1                 LEVEL                                        
LOGACT   DS    CL1                 ACTION                                       
LOGPRID  DS    CL8                 PROFSID                                      
         DS    CL4                                                              
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRLODFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRLODFFD                                                       
         EJECT                                                                  
***********************************************************************         
* INPUT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
SRVPD    DSECT                                                                  
SRVP1H   DS    CL8                 DSECT TO COVER INPUT PARAMETERS              
SRVP1    DS    CL16                                                             
SRVP2H   DS    CL8                                                              
SRVP2    DS    CL16                                                             
SRVP3H   DS    CL8                                                              
SRVP3    DS    CL16                                                             
SRVP4H   DS    CL8                                                              
SRVP4    DS    CL16                                                             
SRVNEXT  EQU   *-SRVPD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
*FADSECTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DMGREQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
*DDARREDITD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDARREDITD                                                     
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*DDFH                                                                           
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
*FAPROGSPCD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAPROGSPCD                                                     
         PRINT ON                                                               
*CTGENPHASE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENPHASE                                                     
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SRLOD00C  10/09/01'                                      
         END                                                                    
