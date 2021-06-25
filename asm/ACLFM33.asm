*          DATA SET ACLFM33    AT LEVEL 017 AS OF 08/12/02                      
*PHASE T60333A,*                                                                
ACLFM33  TITLE '- GST RULES RECORDS W/ QST'                                     
ACLFM33  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**FM33**                                             
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(WORKING STORAGE)                        
         L     RA,0(R1)                                                         
         USING T603FFD,RA          RA=A(TWA)                                    
         LA    R8,LOGWORK                                                       
         USING LOGWORK,R8          R8=A(REMAINING WORK IN TWA)                  
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC         RC=A(GLOBAL WORKING STORAGE)                 
         SPACE 1                                                                
         TM    INDICS,IND1DONE     TEST INITIALIZATION DONE                     
         BO    *+8                                                              
         BAS   RE,RUNF                                                          
         SPACE 1                                                                
         CLI   MODE,BUILDKEY                                                    
         BE    VALKEY                                                           
         CLI   MODE,DSPLYREC                                                    
         BE    DISREC                                                           
         CLI   MODE,BUILDREC                                                    
         BE    BLDREC                                                           
         B     XIT                                                              
         SPACE 3                                                                
XITIFMSG LA    RE,GTMINF           XIT WITH INFO. MESSAGE                       
         B     *+8                                                              
XITERMSG LA    RE,GTMERR           XIT WITH ERROR MESSAGE                       
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         STC   RE,GTMTYP                                                        
         MVC   GTMSGNO,MSGNO                                                    
         GOTO1 GETTXT              GETTXT SETS INTENSITY                        
         NI    LOGHEADH+FHOID,FF-FHOIHI    SO RESET IT                          
         MVI   ERROR,X'FE'                                                      
         DROP  R1                                                               
         B     XIT                                                              
         SPACE 1                                                                
XITOK    MVI   ERROR,FF            XIT WITH NO ERRORS                           
         SPACE 1                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
***********************************************************************         
* FIRST TIME ROUTINE                                                  *         
***********************************************************************         
         SPACE 1                                                                
RUNF     ST    RE,FULL                                                          
         SPACE 1                                                                
         L     RF,COMFACS          COPY ROUTINE ADDRESSES                       
         USING COMFACSD,RF                                                      
         MVC   DICTATE,CDICTATE                                                 
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   GETTXT,CGETTXT                                                   
         DROP  RF                                                               
         SPACE 1                                                                
         GOTO1 DICTATE,DMCB,C'LU  ',DDIN,DDOUT                                  
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         SPACE 1                                                                
         LA    R2,TAXIOKEY         READ COMPANY RECORD                          
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,COMPANY                                                  
         GOTO1 TAXREAD,IO                                                       
         BE    *+6                                                              
         DC    H'0'                CRASH IF COMPANY RECORD DON'T EXIST          
         SPACE 1                                                                
         GOTO1 FINDEL,NAMELQ       GET NAME ELEMENT                             
         USING NAMELD,RF                                                        
         XC    COMPNAME,COMPNAME                                                
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   COMPNAME(0),NAMEREC                                              
         DROP  RF                                                               
         SPACE 1                                                                
         GOTO1 FINDEL,CPYELQ       GET COMPANY ELEMENT                          
         USING CPYELD,RF                                                        
*&&UK*&& MVC   TAXUL,=C'SG'                                                     
*&&US*&& MVC   TAXUL,CPYTAX                                                     
         TM    CPYSTAT1,CPYSOROE                                                
         BO    RUNF2                                                            
         XC    LOGOFF,LOGOFF       OFFICE IS NOT REQUIRED                       
         OI    LOGOFFH+FHOID,FHOITR    SO PROTECT & CLEAR                       
         OI    LOGOFFH+FHATD,FHATPR       OFFICE FIELDS                         
         XC    LOGOFHD,LOGOFHD                                                  
         OI    LOGOFHDH+FHOID,FHOITR                                            
         OI    LOGOFHDH+FHATD,FHATPR                                            
         MVC   LOGOFNM,COMPNAME                                                 
         OI    LOGOFNMH+FHOID,FHOITR                                            
         SPACE 1                                                                
RUNF2    TM    CPYSTAT4,CPYSOFF2                                                
         BNO   *+8                                                              
         OI    INDICS,INDNOFFS     NEW OFFICE SYSTEM                            
         DROP  RF                                                               
         SPACE 1                                                                
         USING LDGRECD,R2                                                       
         MVC   LDGKUNT(L'TAXUL),TAXUL  READ TAX LEDGER RECORD                   
         GOTO1 TAXREAD,IO                                                       
         BE    RUNF4                                                            
         MVC   MSGNO,LGNTONFL      LEDGER RECORD MUST BE THERE                  
         LA    R2,LOGRECH                                                       
         B     XITERMSG                                                         
         DROP  R2                                                               
*                                                                               
RUNF4    GOTO1 FINDEL,ACLELQ       GET LENGTHS ELEMENT                          
         IC    R1,1(RF)                                                         
         AR    R1,RF                                                            
         BCTR  R1,0                R1=A(END OF LENGTHS ELEMENT)                 
         LA    RF,ACLVALS-ACLELD(RF)                                            
         USING ACLVALS,RF          FIND LENGTHS                                 
         LA    R0,L'ACLVALS          FOR HIGH/LOW LEVEL ACCOUNTS                
         MVI   ACTHILEN,0                                                       
         MVI   ACTLOLEN,0                                                       
RUNF6    CLI   ACLVLEN,0                                                        
         BE    RUNF8                                                            
         CLI   ACLVLEN,L'ACTKACT   IF ACLVLEN IS NOT A VALID LENGTH             
         BH    RUNF8                 ASSUME LOWEST LEVEL FOUND                  
         MVC   ACTHILEN,ACTLOLEN                                                
         MVC   ACTLOLEN,ACLVLEN                                                 
         BXLE  RF,R0,RUNF6                                                      
         DROP  RF                                                               
         SPACE 1                                                                
RUNF8    OI    INDICS,IND1DONE                                                  
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
* - INITIALIZE KEY & VALIDATE DATE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R3,TAXIOKEY                                                      
         USING TAXRECD,R3                                                       
         XC    TAXKEY,TAXKEY                                                    
         MVI   TAXKTYP,TAXKTYPQ                                                 
         MVC   TAXKCPY,COMPANY                                                  
         SPACE 1                                                                
         GOTO1 INPUT,LOGDATEH      VALIDATE DATE                                
         BZ    XIT                                                              
         BCTR  RF,0                                                             
         SPACE 1                                                                
         EX    RF,*+8              'TODAY'                                      
         B     *+10                                                             
         CLC   WORK(0),AC@TODAY                                                 
         BNE   VDAT2                                                            
         MVC   EFFDATE,TODAY                                                    
         B     VDAT8                                                            
         SPACE 1                                                                
VDAT2    CLI   LFMACT,C'N'                                                      
         BE    VDAT6                                                            
         EX    RF,*+8              'LAST'                                       
         B     *+10                                                             
         CLC   WORK(0),AC@LAST                                                  
         BNE   VDAT4                                                            
         MVC   EFFDATE,EFFS                                                     
         B     VDAT8                                                            
         SPACE 1                                                                
VDAT4    EX    RF,*+8              'PREVIOUS' DATE                              
         B     *+10                                                             
         CLC   WORK(0),AC@PRV                                                   
         BNE   VDAT6                                                            
         ICM   RE,7,EFFDATE                                                     
         BCTR  RE,0                                                             
         STCM  RE,7,EFFDATE                                                     
         B     VDAT8                                                            
         SPACE 1                                                                
VDAT6    GOTO1 DATVAL,DMCB,WORK,DUB  A REAL DATE                                
         MVI   ERROR,DATERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,DUB,(1,EFFDATE)                                      
         SPACE 1                                                                
VDAT8    CLI   LFMACT,C'I'                                                      
         BE    VDAT10                                                           
*                                                                               
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BE    VDAT10              YES, ALLOW BACKDATE                          
*                                                                               
         CLC   EFFDATE,TODAY       CAN'T CHANGE OR CREATE A RECORD              
         MVI   ERROR,DATTOOLO         THAT PRE-DATES TODAY                      
         BL    XIT                                                              
         SPACE 1                                                                
VDAT10   MVC   TAXKDATE,EFFDATE    TAXKDATE = COMPLEMENT OF DATE                
         XC    TAXKDATE,EFFS                                                    
         EJECT                                                                  
***********************************************************************         
* - VALIDATE PROVINCE                                                *          
***********************************************************************         
VALPRV   DS    0H                                                               
         GOTO1 INPUT,LOGPRVH                                                    
         BZ    VALPRVX             NONE, ASSUME GST                             
*                                                                               
         CLC   LOGPRV,SPACES       GST                                          
         BE    VALPRVX                                                          
*                                                                               
         LA    RF,PRVTAB           PROVINCES, FROM DDPSTVAL                     
VALPRV10 CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    VALPRVER                                                         
         CLC   0(L'PRVTAB,RF),LOGPRV                                            
         BE    VALPRV20                                                         
         LA    RF,L'PRVTAB(RF)                                                  
         B     VALPRV10                                                         
*                                                                               
VALPRVER MVI   ERROR,NOOFFICE      INVALID PST CODE                             
         B     XIT                                                              
*                                                                               
VALPRV20 MVC   TAXKPRV,LOGPRV                                                   
VALPRVX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* - VALIDATE OFFICE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         NI    INDICS,FF-INDOFREC-INDCDFNO                                      
         OI    LOGOFNMH+FHOID,FHOITR                                            
         GOTO1 INPUT,LOGOFFH                                                    
         BNZ   VOFF2                                                            
         MVC   TAXKOFF,EFFS                                                     
         MVC   LOGOFNM,COMPNAME    NO INPUT = COMPANY RECORD                    
         B     VOFFX                                                            
         SPACE 1                                                                
VOFF2    OI    INDICS,INDOFREC     OFFICE RECORD                                
         MVC   TAXKOFF,WORK                                                     
         TM    LOGOFFH+FHIID,FHIIVA                                             
         BO    VOFF10              OFFICE ALREADY VALIDATED                     
         SPACE 1                                                                
         MVC   TAXKEYSV,TAXIOKEY   SAVE TAX KEY                                 
         MVC   TAXIOKEY,SPACES                                                  
         TM    INDICS,INDNOFFS                                                  
         BNO   VOFF4                                                            
         SPACE 1                                                                
         USING OFFRECD,R3          FOR NEW OFFICE SYSTEM                        
         MVI   OFFKTYP,OFFKTYPQ      SET UP OFFICE RECORD                       
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,WORK                                                     
         B     VOFF6                                                            
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
VOFF4    MVI   ERROR,FLD2LONG      FOR OLD OFFICE SYSTEM                        
         CLI   WORK+1,C' '           OFFICE MUST BE ONE CHAR.                   
         BNE   XIT                                                              
         MVC   ACTKCPY,COMPANY     SET UP ACCOUNT RECORD                        
         MVC   ACTKUNT(L'DEPTUL),DEPTUL                                         
         MVC   ACTKACT(1),WORK                                                  
         SPACE 1                                                                
VOFF6    MVI   ERROR,NOOFFICE                                                   
         GOTO1 TAXREAD,IO          IF RECORD NOT THERE                          
         BNE   XIT                   OFFICE DOES NOT EXIST                      
         SPACE 1                                                                
         TM    INDICS,INDNOFFS                                                  
         BNO   VOFF8                                                            
         GOTO1 FINDEL,OFIELQ       IF NEW SYSTEM                                
         MVC   MSGNO,OFFISLST        MUST NOT BE AN OFFICE LIST                 
         TM    OFISTAT-OFIELD(RF),OFISLIST                                      
         BO    XIT                                                              
         SPACE 1                                                                
VOFF8    GOTO1 FINDEL,NAMELQ       DISPLAY OFFICE NAME                          
         XC    LOGOFNM,LOGOFNM                                                  
         IC    RE,NAMLN-NAMELD(RF)                                              
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   LOGOFNM(0),NAMEREC-NAMELD(RF)                                    
         SPACE 1                                                                
         MVC   TAXIOKEY,TAXKEYSV   RESTORE TAX KEY                              
         USING TAXRECD,R3                                                       
         SPACE 1                                                                
VOFF10   CLI   LFMACT,C'N'                                                      
         BNE   VOFFX                                                            
         TM    LOGACTH+FHIID,FHIIPR    ACTION IS 'NEW'                          
         BNO   VOFF12              IF ACTION INPUT PREVIOUSLY                   
         TM    LOGDATEH+FHIID,FHIIVA                                            
         BNO   VOFF12                 & THE DATE IS VALIDATED                   
         TM    LOGOFFH+FHIID,FHIIVA   & THE OFFICE IS VALIDATED                 
         BO    VOFFX                    THE RECORD MAY BE ADDED                 
         SPACE 1                                                                
VOFF12   LA    R2,LOGDATEH                                                      
         MVI   ERROR,RECONFLE      OTHERWISE                                    
         GOTO1 TAXREAD,IO            CHECK RECORD DOESN'T EXIST                 
         BE    XIT                                                              
         MVC   TAXKOFF,EFFS        COMPANY TAX RECORD MUST EXIST                
         MVC   MSGNO,NOCPYRLS                                                   
         GOTO1 TAXREAD,IO2                                                      
         BNE   XITERMSG                                                         
         OI    LOGDATEH+FHIID,FHIIVA                                            
         OI    LOGOFFH+FHIID,FHIIVA    VALIDATED                                
         OI    LOGDATEH+FHOID,FHOITR                                            
         OI    LOGOFFH+FHOID,FHOITR    TRANSMIT                                 
         OI    INDICS,INDCDFNO     DISPLAY COMPANY TAX RECORD                   
         B     DISREC                                                           
         SPACE 1                                                                
VOFFX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* - READ FOR MOST RECENT DATE AND DISPLAY NEW DATE                    *         
***********************************************************************         
         SPACE 1                                                                
         LA    R2,LOGDATEH                                                      
         CLI   LFMACT,C'N'                                                      
         BE    VALK2                                                            
         SPACE 1                                                                
         TM    LOGDATEH+FHIID,FHIIVA ACTION IS 'NEW' OR 'AMEND'                 
         BNO   *+12                                                             
         TM    LOGOFFH+FHIID,FHIIVA  IF DATE & OFFICE VALIDATED                 
         BO    *+8                     KEY HAS NOT CHANGED                      
         MVI   ANYKEY,C'Y'                                                      
         SPACE 1                                                                
         GOTO1 TAXHIGH,IO2         READ FOR MOST RECENT DATE                    
         MVI   ERROR,RECNOTON                                                   
         CLC   TAXKEY(TAXKDATE-TAXKEY),TAXKEYSV                                 
         BNE   XIT                                                              
         MVC   EFFDATE,TAXKDATE    SET EFFDATE TO DATE OF RECORD                
         XC    EFFDATE,EFFS                                                     
         SPACE 1                                                                
         CLI   LFMACT,C'A'                                                      
         BNE   VALK4                                                            
*                                                                               
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BE    VALK1               YES, ALLOW BACKDATE                          
*                                                                               
         CLC   EFFDATE,TODAY       FOR 'AMEND' RE-CHECK DATE                    
         MVI   ERROR,DATTOOLO                                                   
         BL    XIT                                                              
*                                                                               
VALK1    NI    INDICS,FF-INDADDTX  TAX RECORD TO BE WRITTEN, NOT ADDED          
         B     VALK4                                                            
         SPACE 1                                                                
VALK2    XC    TAXRSTA,TAXRSTA     ACTION IS 'NEW'                              
         MVI   ERROR,RECONFLE                                                   
         GOTO1 TAXRDDEL,IO                                                      
         BE    XIT                 RECORD MUST NOT EXIST                        
         OI    INDICS,INDADDTX                                                  
         TM    DMERR,DMERRDEL                                                   
         BNO   *+8                                                              
         NI    INDICS,FF-INDADDTX  IF DELETED RECORD MUST BE WRITTEN            
         SPACE 1                                                                
VALK4    XC    LOGDATE,LOGDATE     DISPLAY DATE OF RECORD                       
         GOTO1 DATCON,DMCB,(1,EFFDATE),(8,LOGDATE)                              
         SPACE 1                                                                
         OI    LOGDATEH+FHIID,FHIIVA                                            
         OI    LOGOFFH+FHIID,FHIIVA    VALIDATED                                
         OI    LOGDATEH+FHOID,FHOITR                                            
         OI    LOGOFFH+FHOID,FHOITR    TRANSMIT                                 
         SPACE 1                                                                
         B     XITOK                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TAX RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISREC   LA    R3,LOGIPL1H         INITIALIZE TWA FIELDS                        
         XR    R0,R0                                                            
         SPACE 1                                                                
         USING LOGIPL1H,R3                                                      
DISR2    LA    RF,LOGVTCDH         RF=A(1ST FIELD OF LINE)                      
         LA    R1,LOGRATEH         R1=A(LAST FIELD OF LINE)                     
         DROP  R3                                                               
         SPACE 1                                                                
         USING FHD,RF              INITIALIZE LINE                              
DISR4    IC    R0,FHLN                                                          
         LR    RE,R0               CLEAR FIELD                                  
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         NI    FHAT,FF-FHATPR      UNPROTECT FIELD IF COMPANY RECORD            
         TM    INDICS,INDOFREC                                                  
         BNO   *+8                                                              
         OI    FHAT,FHATPR         PROTECT FIELD IF OFFICE RECORD               
         BXLE  RF,R0,DISR4                                                      
         DROP  RF                                                               
         SPACE 1                                                                
         LA    R3,LOGLINEL(R3)     BUMP R3 TO NEXT LINE                         
         LA    RE,LOGOPHDH         IF ALL INPUT LINES DONE                      
         CR    R3,RE                  GO ON TO OUTPUT LINES                     
         BNE   *+8                                                              
         LA    R3,LOGOPL1H                                                      
         CLI   0(R3),0                                                          
         BNE   DISR2                                                            
         SPACE 3                                                                
         GOTO1 FINDEL2,FFTELQ      DISPLAY REGISTRATION NUMBER                  
         USING FFTELD,RF                                                        
         XC    LOGREG,LOGREG                                                    
         IC    RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   LOGREG(0),FFTDATA                                                
         OI    LOGREGH+FHOID,FHOITR                                             
         DROP  RF                                                               
         SPACE 1                                                                
         LA    R4,IO2                                                           
         AH    R4,DATADISP         R4=A(1ST ELEMEMT)                            
         LA    R3,LOGIPL1H         R3=A(LINE ON SCREEN)                         
         OI    INDICS,INDIPEL                                                   
         SPACE 1                                                                
         USING TAXELD,R4                                                        
DISR6    CLI   TAXEL,0             ALL ELEMENTS DONE?                           
         BE    DISR18                                                           
         CLI   TAXEL,FFTELQ        IGNORE TEXT ELEMENT                          
         BE    DISR16                                                           
         CLI   TAXEL,X'F9'       IGNORE ACTIVITY ELEMENT                        
         BE    DISR16                                                           
         CLI   TAXEL,PTRELQ        AND POINTER ELEMENTS                         
         BE    DISR16                                                           
         CLI   TAXEL,TAXIELQ       INPUT ELEMENT                                
         BE    DISR12                                                           
         CLI   TAXEL,TAXOELQ       OUTPUT ELEMENT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    INDICS,INDIPEL                                                   
         BNO   DISR8                                                            
         NI    INDICS,FF-INDIPEL                                                
         LA    R3,LOGOPL1H         RESET R3 FOR FIRST OUTPUT ELEMENT            
         SPACE 1                                                                
         USING LOGIPL1H,R3                                                      
DISR8    LA    R0,REQOPVCN         IF A REQUIRED OUTPUT TAX CODE                
         LA    RF,REQOPVC             PROTECT THE CODE'S FIELD                  
DISR10   CLC   TAXCODE,0(RF)                                                    
         BNE   *+8                                                              
         OI    LOGVTCDH+FHATD,FHATPR                                            
         LA    RF,1(RF)                                                         
         BCT   R0,DISR10                                                        
         SPACE 1                                                                
DISR12   MVC   LOGVTCD(L'TAXCODE),TAXCODE     DISPLAY CODE                      
         TM    TAXINDS,TAXIDFLT                                                 
         BNO   *+8                                                              
         MVI   LOGVTCD+1,C'*'                                                   
         MVC   LOGTYPE,TAXTYPE     DISPLAY TYPE                                 
         MVC   LOGACCD,TAXACTA     DISPLAY ACCOUNT CODE                         
         XR    RE,RE               DISPLAY ACCOUNT NAME                         
         IC    RE,TAXLN                                                         
         SH    RE,=Y(TAXLN1Q+1)                                                 
         BM    *+14                                                             
         EX    RE,*+4                                                           
         MVC   LOGACNM(0),TAXNAME                                               
         OC    TAXRATE,TAXRATE     DISPLAY RATE                                 
         BNZ   *+14                                                             
         MVC   LOGRATE,AC@ZERO                                                  
         B     DISR14                                                           
         CURED (2,TAXRATE),(6,LOGRATE),2,ALIGN=LEFT                             
         SPACE 1                                                                
DISR14   NI    LOGTYPEH+FHATD,FF-FHATPR    TAX TYPE,                            
         NI    LOGACCDH+FHATD,FF-FHATPR    ACCOUNT CODE,                        
         NI    LOGACNMH+FHATD,FF-FHATPR    ACCOUNT NAME,                        
         NI    LOGRATEH+FHATD,FF-FHATPR    & RATE MUST BE UNPROTECTED           
         DROP  R3                                                               
         SPACE 1                                                                
         LA    R3,LOGLINEL(R3)     BUMP R3 TO NEXT LINE                         
DISR16   XR    RF,RF                                                            
         IC    RF,TAXLN                                                         
         BXH   R4,RF,DISR6         BUMP R4 TO NEXT ELEMENT                      
         DROP  R4                                                               
         SPACE 3                                                                
DISR18   LA    R2,LOGACTH          PUT CURSOR AT LOGACT                         
         CLI   LFMACT,C'I'            FOR AN INQUIRY                            
         BE    XITOK                                                            
         SPACE 1                                                                
         XR    RF,RF               OTHERWISE PUT CURSOR                         
         LA    R2,LOGOFNMH           AT FIRST UNPROTECED FIELD                  
         USING FHD,R2                AFTER THE OFFICE NAME                      
DISR20   TM    FHAT,FHATPR                                                      
         BNO   DISR22                                                           
         IC    RF,FHLN                                                          
         BXH   R2,RF,DISR20                                                     
         DROP  R2                                                               
         SPACE 1                                                                
DISR22   TM    INDICS,INDCDFNO                                                  
         BNO   XITOK               IF COMPANY DISPLAYED FOR NEW OFFICE          
         MVC   MSGNO,ENTOFVAL        SIMULATE AN ERROR CONDITION                
         B     XITIFMSG                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TAX RECORD                                                    *         
* - INITIALIZE FOR LOOP                                               *         
***********************************************************************         
         SPACE 1                                                                
BLDREC   LA    R1,IO2              INITIALIZE IO2 TO BE                         
         USING TAXRECD,R1             ELEMENTLESS TAX RECORD                    
         MVC   TAXKEY,TAXIOKEY                                                  
         XC    TAXRSTA,TAXRSTA                                                  
         LH    RF,DATADISP                                                      
         LA    RE,TAXKEY(RF)                                                    
         MVI   0(RE),0                                                          
         LA    RF,1(RF)                                                         
         STCM  RF,3,TAXRLEN                                                     
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 INPUT,LOGREGH       ADD TEXT ELEMENT (REG#) TO TAX REC.          
         BZ    XIT                                                              
         LA    R1,ELEMENT                                                       
         USING FFTELD,R1                                                        
         MVI   FFTEL,FFTELQ                                                     
         STC   RF,FFTDLEN                                                       
         LA    RF,FFTLN1Q+L'FFTDLEN(RF)                                         
         STC   RF,FFTLN                                                         
         MVI   FFTTYPE,FFTTFREE                                                 
         MVI   FFTSEQ,0                                                         
         MVC   FFTDATA(L'LOGREG),WORK                                           
         GOTO1 ADDANEL                                                          
         OI    LOGREGH+FHOID,FHOITR                                             
         DROP  R1                                                               
         SPACE 1                                                                
         XC    ACCDTABX,ACCDTABX                                                
         XC    TAXCTABX,TAXCTABX                                                
         NI    INDICS,FF-INDFNDID                                               
         LA    R5,ELEMENT          R5=A(ELEMENT FOR ACCOUNT RECORD)             
         LA    R4,TAXELMNT         R4=A(TAX ELEMENT TO BE BUILT)                
         USING TAXELD,R4                                                        
         OI    INDICS,INDIPEL      INPUT ELEMENTS DONE FIRST                    
         LA    R3,LOGIPL1H         R3=A(LINE IN TWA)                            
         USING LOGIPL1H,R3                                                      
         SPACE 1                                                                
BLDR2    LA    R0,IO               SAVE TAX RECORD IN IO                        
         LA    RE,IO2                                                           
         LA    R1,IOLENQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SPACE 1                                                                
         XC    TAXELMNT,TAXELMNT   CLEAR TAX ELEMENT                            
         MVI   TAXEL,TAXIELQ                                                    
         TM    INDICS,INDIPEL                                                   
         BO    *+8                                                              
         MVI   TAXEL,TAXOELQ                                                    
         EJECT                                                                  
***********************************************************************         
* - VALIDATE TAX CODE AND TYPE                                        *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 INPUT,LOGVTCDH      VALIDATE TAX CODE                            
         BZ    BLDR4                                                            
         MVC   TAXCODE,WORK                                                     
         LA    RF,TAXCTAB          MAKE SURE CODE IS NOT DUPLICATED             
         ICM   R1,15,TAXCTABX                                                   
         BZ    VVTCD4              NO ENTRIES IN TABLE YET                      
         LA    R0,L'TAXCTAB                                                     
         MVI   ERROR,DUPLCATE                                                   
VVTCD2   CLC   TAXCODE,0(RF)                                                    
         BE    XIT                                                              
         BXLE  RF,R0,VVTCD2                                                     
VVTCD4   STCM  RF,15,TAXCTABX      ADD CURRENT CODE TO TABLE                    
         MVC   0(L'TAXCTAB,RF),TAXCODE                                          
         SPACE 1                                                                
         CLI   WORK+1,C' '                                                      
         BE    VVTCD6                                                           
         MVI   ERROR,FLD2LONG      CODE HAS LENGTH OF 2 CHARACTERS              
         TM    INDICS,INDIPEL         SO SEE IF STANDARD INPUT DEFAULT          
         BNO   XIT                    IS VALIDLY DEFINED                        
         CLI   WORK+1,C'*'                                                      
         BNE   XIT                                                              
         MVC   MSGNO,ONLY1DF                                                    
         TM    INDICS,INDFNDID                                                  
         BO    XITERMSG                                                         
         OI    INDICS,INDFNDID                                                  
         OI    TAXINDS,TAXIDFLT                                                 
         SPACE 1                                                                
VVTCD6   OI    LOGVTCDH+FHOID,FHOITR                                            
         SPACE 1                                                                
         GOTO1 INPUT,LOGTYPEH      VALIDATE TYPE                                
         BZ    XIT                                                              
         MVC   TAXTYPE,WORK                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),AC@DEL                                                   
         BNE   VTYPE2                                                           
         TM    LOGVTCDH+FHATD,FHATPR   IF TAX CODE IS PROTECTED                 
         BNO   BLDR4                     CANNOT DELETE                          
         MVI   ERROR,NOTVLACT                                                   
         B     XIT                                                              
VTYPE2   OI    LOGTYPEH+FHOID,FHOITR                                            
         EJECT                                                                  
***********************************************************************         
* - VALIDATE ACCOUNT CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 INPUT,LOGACCDH                                                   
         BZ    XIT                                                              
         MVI   ERROR,ACTOOLNG                                                   
         CLM   RF,1,ACTLOLEN                                                    
         BH    XIT                 ACCOUNT MUST NOT BE TOO LONG                 
         MVC   MSGNO,NOTLOWAC                                                   
         CLM   RF,1,ACTHILEN                                                    
         BNH   XITERMSG            ACCOUNT MUST BE LOW LEVEL                    
         MVC   TAXACTU(L'TAXUL),TAXUL                                           
         MVC   TAXACTA,WORK                                                     
         SPACE 1                                                                
         LA    RF,ACCDTAB          SEE IF DUPLICATE ACCOUNT CODE                
         ICM   R1,15,ACCDTABX                                                   
         BZ    VACC4               NO ENTRIES IN TABLE YET                      
         LA    R0,L'ACCDTAB                                                     
         MVC   MSGNO,DUPACCD                                                    
VACC2    CLC   TAXACTA,0(RF)                                                    
         BE    XITERMSG                                                         
         BXLE  RF,R0,VACC2                                                      
VACC4    STCM  RF,15,ACCDTABX      ADD CURRENT CODE TO TABLE                    
         MVC   0(L'ACCDTAB,RF),TAXACTA                                          
         SPACE 1                                                                
         LA    R6,TAXIOKEY         CHECK HIGH LEVEL ACCOUNT EXISTS              
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'TAXUL),TAXUL                                           
         XR    RE,RE                                                            
         ICM   RE,1,ACTHILEN                                                    
         BZ    VACC6               FORGET IT IF NO HIGHER LEVEL ACCOUNT         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),WORK                                                  
         MVI   ERROR,NOHIGHER                                                   
         GOTO1 TAXREAD,IO2                                                      
         BNE   XIT                                                              
         SPACE 1                                                                
VACC6    MVC   ACTKACT,TAXACTA     SEE IF LOW LEVEL ACCOUNT EXISTS              
         GOTO1 TAXRDDEL,IO2                                                     
         BE    VACC8                                                            
         DROP  R6                                                               
         SPACE 1                                                                
         OI    INDICS,INDADDAC     ACCOUNT NEEDS TO BE ADDED                    
         TM    DMERR,DMERRDEL      OR                                           
         BNO   *+8                 IF IT'S BEEN DELETED                         
         NI    INDICS,FF-INDADDAC     ACCOUNT NEEDS TO BE WRITTEN               
         SPACE 1                                                                
         LA    R1,IO2              BUILD RECORD IN IO2                          
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,TAXIOKEY                                                  
         XC    ACTKSTA,ACTKSTA                                                  
         LH    RF,DATADISP                                                      
         LA    RE,ACTRECD(RF)                                                   
         MVI   0(RE),0                                                          
         LA    RF,1(RF)                                                         
         STCM  RF,3,ACTRLEN                                                     
         DROP  R1                                                               
         SPACE 1                                                                
         USING RSTELD,R5           ADD STATUS ELEMENT                           
         GOTO1 STATIN                                                           
*&&UK*&& OI    RSTSTAT,RSTSIVAT    SET TO INPUT                                 
*&&US*&& OI    RSTSTAT2,RSTSIVAT                                                
         CLI   TAXEL,TAXIELQ                                                    
         BE    *+8                                                              
*&&UK*&& NI    RSTSTAT,FF-RSTSIVAT    OR OUTPUT                                 
*&&US*&& NI    RSTSTAT2,FF-RSTSIVAT                                             
         GOTO1 REMANEL,DMCB,('RSTELQ',0)                                        
         GOTO1 ADDANEL                                                          
         GOTO1 BALIN               ADD BALANCE ELEMENT                          
         B     VACC10                                                           
         SPACE 1                                                                
VACC8    NI    INDICS,FF-INDADDAC  ACCOUNT RECORD ALREADY EXISTS                
         USING RSTELD,R5           CHECK THAT TAXEL AND THE STATUS              
         GOTO1 STATIN                ELEMENT ARE CONSISTENT                     
         LA    RF,TAXIELQ                                                       
*&&UK*&& TM    RSTSTAT,RSTSIVAT                                                 
*&&US*&& TM    RSTSTAT2,RSTSIVAT                                                
         BO    *+8                                                              
         LA    RF,TAXOELQ                                                       
         MVC   MSGNO,WGTPVTAC                                                   
         CLM   RF,1,TAXEL                                                       
         BNE   XITERMSG                                                         
         SPACE 1                                                                
VACC10   OI    LOGACCDH+FHOID,FHOITR                                            
         EJECT                                                                  
***********************************************************************         
* - VALIDATE ACCOUNT NAME & RATE                                      *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 INPUT,LOGACNMH      VALIDATE ACCOUNT NAME                        
         BZ    XIT                                                              
         MVC   TAXNAME,WORK                                                     
         LA    RE,TAXLN1Q(RF)      SET TAX ELEMENT LENGTH                       
         STC   RE,TAXLN                                                         
         SPACE 1                                                                
         USING NAMELD,R5           ADD/CHANGE ACCOUNT NAME ELEMENT              
         MVI   NAMEL,NAMELQ                                                     
         MVC   NAMEREC,TAXNAME                                                  
         LA    RF,NAMLN1Q(RF)                                                   
         STC   RF,NAMLN                                                         
         GOTO1 REMANEL,DMCB,('NAMELQ',0)                                        
         GOTO1 ADDANEL                                                          
         OI    LOGACNMH+FHOID,FHOITR                                            
         SPACE 1                                                                
         GOTO1 INPUT,LOGRATEH      VALIDATE RATE                                
         BZ    XIT                                                              
         LA    RE,WORK-1(RF)       ALLOW PERCENTAGE SIGN AT END                 
         CLI   0(RE),C'%'                                                       
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         GOTO1 CASHVAL,DMCB,(2,WORK),(RF)                                       
         MVI   ERROR,NOTNUMRC                                                   
         CLI   0(R1),0                                                          
         BNE   XIT                                                              
         MVC   MSGNO,INVRATE                                                    
         CLC   4(4,R1),=F'10000'   ENSURE REASONABLE TAX RATE                   
         BH    XIT                                                              
         MVC   TAXRATE,6(R1)                                                    
         SPACE 1                                                                
         USING RATELD,R5           ADD/CHANGE ACCOUNT RATE ELEMENT              
         MVI   RATEL,RATEVATQ                                                   
         MVI   RATLN,RATLNQ                                                     
         MVC   RATRATE,TAXRATE                                                  
         GOTO1 REMANEL,DMCB,('RATEVATQ',0)                                      
         GOTO1 ADDANEL                                                          
         OI    LOGRATEH+FHOID,FHOITR                                            
         EJECT                                                                  
***********************************************************************         
* - ADD ACCOUNT RECORD & TAX ELEMENT                                  *         
* - GO ON TO NEXT TWA LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         LA    RF,TAXWRITE         WRITE OR ADD ACCOUNT RECORD                  
         TM    INDICS,INDADDAC                                                  
         BNO   *+8                                                              
         LA    RF,TAXADD                                                        
         GOTO1 (RF)                                                             
         SPACE 1                                                                
         LA    R0,IO2              GET BACK TAX RECORD                          
         LA    RE,IO                                                            
         LA    R1,IOLENQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   ELEMENT,TAXELMNT    ADD TAX ELEMENT                              
         GOTO1 ADDANEL                                                          
         SPACE 1                                                                
         DROP  R3                                                               
BLDR4    LA    R3,LOGLINEL(R3)     BUMP R3 TO NEXT LINE                         
         LA    R0,LOGOPHDH                                                      
         CR    R3,R0               IF R3 = OUTPUT HEADER FIELD                  
         BNE   BLDR6                 ALL INPUT ELEMENTS HAVE BEEN DONE          
         SPACE 1                                                                
         NI    INDICS,FF-INDIPEL   NOW DO OUTPUT ELEMENTS                       
         LA    R3,LOGOPL1H                                                      
         XC    TAXCTABX,TAXCTABX                                                
         TM    INDICS,INDFNDID     STANDARD INPUT DEFAULT MUST                  
         BO    BLDR6               HAVE BEEN DEFINED                            
         LA    R2,LOGVTCDH                                                      
         MVC   MSGNO,ONLY1DF                                                    
         B     XITERMSG                                                         
         SPACE 1                                                                
BLDR6    CLI   0(R3),0             SEE IF AT END OF TWA FIELD                   
         BNE   BLDR2                                                            
         DROP  R4,R5                                                            
         SPACE 1                                                                
         BAS   RE,UPTAX            ADD OR WRITE TAX RECORD                      
         B     DISREC                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT A TWA INPUT FIELD INTO WORK (SPACE FILLED)       *         
*        (FIELD CAN BE PROTECTED)                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
* EXIT - R2=A(TWA FIELD HEADER)                                       *         
*        RF=LEN(TWA FIELD)                                            *         
*        IF NO INPUT CC=EQUAL, ERROR=MISSING                          *         
*        IF INPUT CC=NOT EQUAL                                        *         
***********************************************************************         
         SPACE 1                                                                
INPUT    LR    R2,R1               R2=A(INPUT FIELD)                            
         USING FHD,R2                                                           
         MVC   WORK,SPACES                                                      
         MVI   ERROR,MISSING                                                    
         XR    RF,RF                                                            
         IC    RF,FHIL             UNPROTECTED FIELD LENGTH                     
         TM    FHAT,FHATPR                                                      
         BNO   *+8                                                              
         IC    RF,FHOL             PROTECTED FIELD LENGTH                       
         LTR   RF,RF                                                            
         BZR   RE                  RETURN IF LENGTH IS 0                        
         SPACE 1                                                                
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   WORK(0),FHDA        EXTRACT FIELD INTO WORK                      
         LA    RF,1(RF)                                                         
INP2     CLI   WORK,C' '           LEFT ALIGN FIELD                             
         BH    *+14                                                             
         MVC   WORK(L'WORK-1),WORK+1                                            
         BCT   RF,INP2                                                          
         LTR   RF,RF                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD/WRITE TAX RECORD AND HANDLE RECORD ACTIVITY POINTERS *         
* NTRY: INDICS = INDADDTX TO ADD TAX RECORD                           *         
*       FOR TAXWRITE/TAXADD, IO2 IS USED                              *         
***********************************************************************         
         SPACE 1                                                                
UPTAX    NTR1  ,                                                                
         CLI   RAPTR,C'Y'          TEST TO GENERATE RA POINTERS                 
         BNE   UPTAX10             NO                                           
         SPACE 1                                                                
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    BUILD RAP PTR ELEM                           
         MVC   RAPCPY,COMPANY                                                   
         MVI   RAPRTYP,RAPKRTAX                                                 
         MVI   RAPEMU,C'Y'                                                      
         MVC   RAPACOM,COMFACS                                                  
         LA    RE,IO2                                                           
         ST    RE,RAPAREC                                                       
         GOTO1 RAPPER,RAPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
UPTAX10  LA    RF,TAXADD           TEST TO ADD/WRITE TAX RECORD                 
         TM    INDICS,INDADDTX                                                  
         BO    *+8                                                              
         LA    RF,TAXWRITE                                                      
         BASR  RE,RF                                                            
         CLI   DMERR,0             TEST FOR IO ERROR                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
UPTAX20  CLI   RAPTR,C'Y'                                                       
         BNE   UPTAXX                                                           
         SPACE 1                                                                
         MVI   RAPACTN,RAPAPTR     NOW MAINTAIN POINTERS                        
         GOTO1 RAPPER,RAPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
UPTAXX   B     TAXIOXIT                                                         
         EJECT                                                                  
***********************************************************************         
* IO ROUTINES                                                         *         
* NTRY: FOR TAXREAD/TAXHIGH, R1=A(IO AREA)                            *         
*       FOR TAXWRITE/TAXADD, IO2 IS USED                              *         
* EXIT: DMERR=ERROR CODE RETURNED FROM DATAMGR                        *         
***********************************************************************         
         SPACE 1                                                                
TAXREAD  NTR1  ,                                                                
         LR    RF,R1                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,TAXIOKEY,(RF)                         
         MVC   DMERR,8(R1)                                                      
         B     TAXIOXIT                                                         
         SPACE 3                                                                
TAXRDDEL NTR1  ,                   READ DELETED RECORDS, TOO                    
         LR    RF,R1                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCFIL,TAXIOKEY,(RF)                 
         MVC   DMERR,8(R1)                                                      
         B     TAXIOXIT                                                         
         SPACE 3                                                                
TAXHIGH  NTR   ,                                                                
         LR    R2,R1                                                            
         MVC   TAXKEYSV,TAXIOKEY   SAVE ORIGINAL KEY                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,TAXIOKEY,(R2)                         
         MVC   TAXIOKEY,0(R2)      COPY NEW KEY                                 
         MVC   DMERR,8(R1)                                                      
         B     TAXIOXIT                                                         
         SPACE 3                                                                
TAXWRITE NTR   ,                                                                
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,IO2,IO2                                
         MVC   DMERR,8(R1)                                                      
         B     TAXIOXIT                                                         
         SPACE 3                                                                
TAXADD   NTR   ,                                                                
         GOTO1 DATAMGR,DMCB,DMADD,ACCFIL,IO2,IO2                                
         MVC   DMERR,8(R1)                                                      
         B     TAXIOXIT                                                         
         SPACE 3                                                                
TAXIOXIT XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND ADDRESS OF AN ELEMENT IN RECORD IN IO               *         
* NTRY - R1=ELEMENT                                                   *         
* EXIT - IF ELEMENT FOUND:     RF=A(ELEMENT)                          *         
*      - IF ELEMENT NOT FOUND: DC H'0'                                *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   LA    RF,IO                                                            
         B     *+8                                                              
FINDEL2  LA    RF,IO2                                                           
         AH    RF,DATADISP         RF=A(1ST ELEMENT)                            
         XR    R0,R0                                                            
         B     NEXTEL2                                                          
         SPACE 1                                                                
NEXTEL   IC    R0,1(RF)            BUMP RF TO NEXT ELEMENT                      
         AR    RF,R0                                                            
NEXTEL2  CLM   R1,1,0(RF)          EXIT IF ELEMENT FOUND                        
         BER   RE                                                               
         CLI   0(RF),0                                                          
         BNE   NEXTEL                                                           
         SR    RE,RB                                                            
         DC    H'0'                CRASH IF AT END OF RECORD                    
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
REQOPVC  DC    C'X'                REQUIRED OUTPUT CODES                        
         DC    C'S'                                                             
         DC    C'Z'                                                             
REQOPVCN EQU   *-REQOPVC           # REQUIRED OUTPUT CODES                      
PRVTAB   DS    0CL2                                                             
         DC    C'BC'               BRITISH COLUMBIA                             
         DC    C'AL'               ALBERTA                                      
         DC    C'SA'               SASKATCHEWAN                                 
         DC    C'MA'               MANITOBA                                     
         DC    C'ON'               ONTARIO                                      
         DC    C'PQ'               QUEBEC                                       
         DC    C'NB'               NEW BRUNSWICK                                
         DC    C'NS'               NOVA SCOTIA                                  
         DC    C'PE'               PRINCE EDWARD ISLAND                         
         DC    C'NF'               NEWFOUNDLAND                                 
         DC    X'FF'               END OF TABLE                                 
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
EFFS     DC    8XL1'FF'                                                         
         SPACE 1                                                                
DEPTUL   DC    C'2D'               DEPARTMENT UNIT LEDGER                       
         SPACE 1                                                                
DUPACCD  DC    AL2(1012)           DUPLICATE ACCOUNT CODE                       
NOTLOWAC DC    AL2(1112)           NOT A LOW LEVEL ACCOUNT                      
WGTPVTAC DC    AL2(1033)           WRONG TYPE OF TAX ACCOUNT                    
NOCPYRLS DC    AL2(1034)           NO COMPANY RULES FOR DATE                    
ONLY1DF  DC    AL2(1035)           ONLY 1 INPUT DEFAULT TYPE ALLOWED            
ENTOFVAL DC    AL2(193)            ENTER OFFICE VALUES                          
LGNTONFL DC    AL2(110)            LEDGER NOT ON FILE                           
OFFISLST DC    AL2(241)            OFFICE IS A LIST                             
INVRATE  DC    AL2(261)            INVALID TAX RATE                             
         SPACE 1                                                                
DMWRT    DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
         SPACE 1                                                                
DDIN     DS    0C                                                               
         DCDDL AC#LAST,L'LOGDATE,L                                              
         DCDDL AC#PRV,L'LOGDATE,L                                               
         DCDDL AC#TODAY,L'LOGDATE,L                                             
         DCDDL AC#DEL,L'LOGTYPE,L                                               
         DCDDL AC#ZERO,L'LOGRATE,L                                              
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMCCD                                                       
LOGLINEL EQU   LOGIPL2H-LOGIPL1H   LENGTH OF LINE                               
         SPACE 1                                                                
DDOUT    DS    0C                  DATA DICTIONARY OUTPUT                       
         DSDDL PRINT=YES                                                        
         SPACE 1                                                                
COMPNAME DS    CL36                COMPANY NAME                                 
         SPACE 1                                                                
TAXIOKEY DS    CL42                KEY FOR RECORD                               
         SPACE 3                                                                
         ORG   T603FFD+((TWAUSER+1)-TWAD)    SAVED WORK IN TWA                  
         SPACE 1                                                                
INDICS   DS    XL1                                                              
IND1DONE EQU   X'80'               FIRST TIME ROUTINE HAS EXECUTED              
INDNOFFS EQU   X'40'               NEW OFFICE SYSTEM                            
INDIPEL  EQU   X'20'               INPUT ELEMENT                                
INDOFREC EQU   X'10'               OFFICE RECORD                                
INDCDFNO EQU   X'08'               COMPANY DISPLAY FOR A NEW OFFICE             
INDFNDID EQU   X'04'               FOUND STANDARD INPUT DEFAULT                 
INDADDAC EQU   X'02'               ADD ACCOUNT RECORED, NOT WRITE               
INDADDTX EQU   X'01'               ADD TAX RECORD, NOT WRITE                    
         SPACE 1                                                                
TAXUL    DS    CL2                 TAX UNIT LEDGER                              
         SPACE 1                                                                
ACTHILEN DS    XL1                 LENGTH OF HIGH LEVEL ACCOUNT                 
ACTLOLEN DS    XL1                 LENGTH OF LOW LEVEL ACCOUNT                  
TODAY    DS    PL3                 TODAY'S DATE (PWOS)                          
EFFDATE  DS    PL3                 EFFECTIVE DATE (PWOS)                        
         SPACE 1                                                                
CUREDIT  DS    V                                                                
DICTATE  DS    V                                                                
GETTXT   DS    V                                                                
         SPACE 3                                                                
WORKD    DSECT                     WORKING STORAGE                              
         SPACE 1                                                                
MSGNO    DS    AL2                 ERROR/INFO. MESSAGE #                        
         SPACE 1                                                                
TAXCTABX DS    AL4                 A(LAST ENTRY IN TAXCTAB)                     
TAXCTAB  DS    7CL(L'TAXCODE)      TABLE OF INPUTTED TAX CODES                  
ACCDTABX DS    AL4                 A(LAST ENTRY IN ACCDTAB)                     
ACCDTAB  DS    14CL(L'TAXACTA)     TABLE OF INPUTTED ACCOUNT CODES              
         SPACE 1                                                                
DMERR    DS    XL1                                                              
DMERRDEL EQU   X'02'               RECORD IS DELETED                            
         SPACE 1                                                                
TAXKEYSV DS    CL42                AREA TO SAVE KEY FOR RECORD                  
TAXELMNT DS    CL255               TAX ELEMENT BUILT HERE                       
         SPACE 1                                                                
       ++INCLUDE ACRAPPERD                                                      
         SPACE 1                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
* ACLFMWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
         PRINT ON                                                               
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACLFM33   08/12/02'                                      
         END                                                                    
