*          DATA SET SPREPAN02  AT LEVEL 010 AS OF 08/05/14                      
*PHASE SPAN02A,*                                                                
*INCLUDE BUFFERIN                                                               
*INCLUDE NETNET                                                                 
*INCLUDE SPBVAL                                                                 
                                                                                
***********************************************************************         
* QOPT1  -  N=Net,G=Gross                                             *         
* QOPT2  -  Print unit trace                                          *         
* QOPT3  -  Sub-media filter                                          *         
* QOPT4  -  Y=Include STW estimates                                   *         
***********************************************************************         
                                                                                
SPAN02   TITLE 'SPAN02 - Network Agency Summary for Accent'                     
SPAN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPAN02                                                       
                                                                                
         L     R9,0(R1)                                                         
         LR    RA,R9                                                            
         AHI   RA,FOURK                                                         
         USING SPWORKD,R9,RA       R9/RA=A(SPWORKD)                             
         LARL  RC,GLOBALS                                                       
         USING GLOBALS,RC          RC=A(Global values)                          
         USING BUFFRECD,BUFFREC    BUFFER record                                
         USING BUFFPARM,DMCB       BUFFERIN parameter list                      
                                                                                
         CLI   MODE,REQFRST        Test correct calling mode                    
         JNE   EXIT                                                             
                                                                                
         OPEN  (FILEOUT,(OUTPUT))  Open the output file                         
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Open Network files                                                  *         
***********************************************************************         
                                                                                
         GOTOR DATAMGR,DMCB,DMOPEN,SPOTSYS,UNTLIST,ADBUY                        
                                                                                
***********************************************************************         
* Set request and optional unit filter dates                          *         
***********************************************************************         
                                                                                
         LHI   R0,18               Set start/end dates in file header           
         CLC   QAREA+52(2),SPACES                                               
         JE    *+14                                                             
         PACK  DUB,QAREA+52(2)     Months back                                  
         CVB   R0,DUB                                                           
         LCR   R0,R0                                                            
         MVC   WORK(6),TODAY                                                    
         MVC   WORK+4(2),=C'15'                                                 
         GOTOR ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
         GOTOR DATCON,DMCB,(X'30',WORK+6),(0,STRDATE),0                         
         GOTOR (RF),(R1),(0,STRDATE),(2,STRDATEC)                               
                                                                                
         CLC   QSTART,SPACES       Test start date given for units              
         JNH   SPAN0004                                                         
         GOTOR DATCON,DMCB,(0,QSTART),(2,UNTSTRDT)                              
                                                                                
SPAN0004 LHI   R0,18                                                            
         CLC   QAREA+54(2),SPACES                                               
         JE    *+14                                                             
         PACK  DUB,QAREA+54(2)     Months forward                               
         CVB   R0,DUB                                                           
         GOTOR ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
         GOTOR DATCON,DMCB,(X'30',WORK+6),ENDDATE,(1,0)                         
                                                                                
         CLC   QEND,SPACES         Test end date given for units                
         JNH   SPAN0006                                                         
         GOTOR DATCON,DMCB,(0,QEND),(2,UNTENDDT)                                
                                                                                
SPAN0006 MVC   FILEDOLS,QOPT1                                                   
         CLI   FILEDOLS,C' '                                                    
         JNE   *+8                                                              
         MVI   FILEDOLS,C'G'       Default is gross                             
         MVC   FILEDATE,TODAY                                                   
         MVC   FILEQSTR,QSTART                                                  
         MVC   FILEQEND,QEND                                                    
         L     R0,ADBUY                                                         
         LHI   R1,OUTRECL                                                       
         LA    RE,FILEHDR                                                       
         LHI   RF,FILEHDRL                                                      
         MVCL  R0,RE                                                            
         L     R0,ADBUY                                                         
         PUT   FILEOUT,(0)         Put header record to output file             
                                                                                
         CLC   QCLT,SPACES         Client filter                                
         JE    SPAN0008                                                         
         CLC   =C'ALL',QCLT                                                     
         JE    SPAN0008                                                         
         GOTOR CLPACK,DMCB,QCLT,CLTFILT                                         
                                                                                
SPAN0008 CLC   QPRD,SPACES         Product filter                               
         JE    SPAN0010                                                         
         CLC   =C'ALL',QPRD                                                     
         JE    SPAN0010                                                         
         MVC   PRDFILT,QPRD                                                     
                                                                                
SPAN0010 CLC   QEST,SPACES         Estimate filter                              
         JE    SPAN0012                                                         
         CLC   =C'ALL',QEST                                                     
         JE    SPAN0012                                                         
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,ESTFILT                                                       
                                                                                
SPAN0012 CLC   QSTA,SPACES         Network filter                               
         JE    SPAN0040                                                         
         CLC   =C'ALL',QSTA                                                     
         JE    SPAN0040                                                         
         MVC   NETFILT,QSTA                                                     
                                                                                
***********************************************************************         
* Build a list of networks for posting by sub-media                   *         
***********************************************************************         
                                                                                
SPAN0040 L     R2,ADBUY                                                         
         USING STAREC,R2           R2=A(Station record)                         
         LARL  R3,NETTAB                                                        
         USING NETTABD,R3          R3=A(Network table)                          
         SR    R0,R0               R0=Number of network table entries           
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,STAKTYPQ                                                
         MVI   STAKMED,C'N'        Set to read network station records          
         MVC   KEYSAVE,STAKEY                                                   
                                                                                
SPAN0050 GOTOR DATAMGR,DMCB,DMRDHI,STATION,STAREC,STAREC                        
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   STAKEY(STAKCALL-STAKEY),KEYSAVE                                  
         JNE   SPAN0080                                                         
         CLC   STAKAGY,AGY         Match on agency code                         
         JL    SPAN0070                                                         
         JH    SPAN0060                                                         
         CLC   STAKCLT,=C'000'     Only want agency level stations              
         JL    SPAN0070                                                         
         JH    SPAN0060                                                         
         MVC   NETTNET,STAKCALL    Build network table entry                    
         MVC   NETTMED,STYPE                                                    
         CLI   NETTMED,C' '                                                     
         JH    *+8                                                              
         MVI   NETTMED,C'N'                                                     
         AHI   R3,NETTABL                                                       
         AHI   R0,1                Bump number of networks in table             
                                                                                
SPAN0060 LLC   R1,STAKCALL+L'STAKCALL-1                                         
         AHI   R1,1                                                             
         STCM  R1,1,STAKCALL+L'STAKCALL-1                                       
                                                                                
SPAN0070 MVC   STAKAGY,AGY         Move agency into key                         
         MVC   STAKCLT,=C'000'                                                  
         XC    STAKFILL,STAKFILL                                                
         J     SPAN0050                                                         
         DROP  R2,R3                                                            
                                                                                
SPAN0080 ST    R0,NETTABN          Set number of network table entries          
                                                                                
***********************************************************************         
* Read unit directory records                                         *         
***********************************************************************         
                                                                                
U        USING NURECD,UNTKEY                                                    
         XC    U.NUKEY,U.NUKEY                                                  
         MVI   U.NUKTYPE,NUKTYPEQ  Read active unit pointers                    
         MVC   U.NUKAM,BAGYMD                                                   
         MVC   U.NUKCLT,CLTFILT    Set client code                              
         MVC   U.NUKDATE,UNTSTRDT                                               
                                                                                
SPAN0100 GOTOR DATAMGR,DMCB,DMRDHI,UNTDIR,U.NUKEY,U.NUKEY                       
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   U.NUKTYPE,NUKTYPEQ  Test unit pointer                            
         JNE   SPANX               No - all done                                
         CLC   U.NUKAM,BAGYMD      Test correct agency                          
         JNE   SPANX               No - all done                                
                                                                                
         OC    CLTFILT,CLTFILT     Test one client request                      
         JZ    *+14                                                             
         CLC   U.NUKCLT,CLTFILT    Yes - match client code                      
         JNE   SPANX                                                            
                                                                                
         CLC   U.NUKDATE,UNTSTRDT  Test lower than start date                   
         JL    SPAN0110                                                         
         CLC   U.NUKDATE,UNTENDDT  Test higher than end date                    
         JNH   SPAN0112                                                         
         SR    R0,R0                                                            
         ICM   R0,3,U.NUKCLT       Bump to next client                          
         AHI   R0,1                                                             
         STCM  R0,3,U.NUKCLT                                                    
SPAN0110 MVC   U.NUKDATE,UNTSTRDT  Set start date                               
         XC    U.NUKTIME(NUKSTAT-NUKTIME),U.NUKTIME                             
         J     SPAN0100                                                         
                                                                                
SPAN0112 OC    NETFILT,NETFILT     Test network filter present                  
         JZ    SPAN0116                                                         
         CLC   U.NUKNET,NETFILT    Match network filter                         
         JE    SPAN0116                                                         
         JL    SPAN0114                                                         
         LLC   R0,U.NUKTIME                                                     
         AHI   R0,1                                                             
         STC   R0,U.NUKTIME                                                     
SPAN0114 MVC   U.NUKNET,NETFILT    Set network filter in key                    
         XC    U.NUKPROG(NUKSTAT-NUKPROG),U.NUKPROG                             
         J     SPAN0100                                                         
                                                                                
SPAN0116 CLI   ESTFILT,0           Test estimate filter present                 
         JE    SPAN0120                                                         
         CLC   U.NUKEST,ESTFILT    Match estimate filter                        
         JE    SPAN0120                                                         
         JL    SPAN0118                                                         
         LLC   R0,U.NUKPROG+L'NUKPROG-1                                         
         AHI   R0,1                                                             
         STC   R0,U.NUKPROG+L'NUKPROG-1                                         
SPAN0118 MVC   U.NUKEST,ESTFILT    Set estimate filter in key                   
         XC    U.NUKSUB(NUKSTAT-NUKSUB),U.NUKSUB                                
         J     SPAN0100                                                         
                                                                                
SPAN0120 CLI   U.NUKSUB,NUKSTRAQ   Test traffic unit                            
         JL    SPAN0140                                                         
                                                                                
SPAN0130 XC    U.NUKSUB(NUKSTAT-NUKSUB),U.NUKSUB                                
         LA    R1,U.NUKEST         Bump estimate or previous key field          
                                                                                
SPAN0132 CLI   0(R1),FF            Test this byte is bumpable                   
         JNE   *+12                                                             
         MVI   0(R1),0             No - set to zero and back-up                 
         JCT   R1,SPAN0132                                                      
         LLC   R0,0(R1)                                                         
         AHI   R0,1                                                             
         STC   R0,0(R1)                                                         
         J     SPAN0100                                                         
                                                                                
***********************************************************************         
* Handle change of client                                             *         
***********************************************************************         
                                                                                
SPAN0140 CLC   U.NUKCLT,LCLT       Test change of client code                   
         JE    SPAN0400                                                         
                                                                                
         GOTOR PUTOUT              Put records for previous client              
                                                                                
         MVC   LCLT,U.NUKCLT       Set current client code                      
                                                                                
C        USING CKEY,KEY            Read client record                           
         XC    C.CKEY,C.CKEY                                                    
         MVC   C.CKEYAM,U.NUKAM                                                 
         MVC   C.CKEYCLT,U.NUKCLT                                               
         GOTOR DATAMGR,DMCB,DMREAD,SPTDIR,C.CKEY,C.CKEY                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ADCLT                                                         
         USING CLTHDR,R2           Extract client values                        
         GOTOR DATAMGR,DMCB,GETREC,SPTFILE,C.CKDA,CLTHDR,DMWORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    CLTVALS(CLTVALL),CLTVALS                                         
         MVC   CLTVNAM,CNAME                                                    
         OC    CLTVNAM,SPACES                                                   
         GOTOR CLUNPK,DMCB,(CPROF+6,CKEYCLT),CLTVCLT                            
         MVC   CLTVACOF,CACCOFC                                                 
         MVC   CLTVCACS,CACCESS                                                 
         MVC   CLTVOFFC,COFFICE                                                 
         MVC   CLTVMDOF,SPACES                                                  
         MVC   CLTVMDOF(L'COFFICE),COFFICE                                      
                                                                                
         TM    COPT4,COP4MIDS      check for midas client                       
         JZ    *+8                                                              
         OI    CLTVFLAG,CLTVFMID                                                
         TM    COPT3,COP3TI        Test intg included in assigned               
         JZ    *+8                                                              
         OI    CLTVFLAG,CLTVFIAC   Set intg included in assigned                
         TM    COPT4,COP4TIS       Test intg+spcl included in assigned          
         JZ    SPAN0142                                                         
         OI    CLTVFLAG,CLTVFIAC   Set intg included in assigned                
         OI    CLTVFLAG,CLTVFSAC   Set spcl included in assigned                
                                                                                
O        USING OFFICED,WORK                                                     
SPAN0142 XC    O.OFFICED(OFCLENQ),O.OFFICED                                     
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAGY,AGY                                                     
         MVC   O.OFCOFC,COFFICE                                                 
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTOR (RF),DMCB,(C'2',O.OFFICED),ACOMFACS                              
         CLI   0(R1),0                                                          
         JNE   *+10                                                             
         MVC   CLTVMDOF,O.OFCOFC2  Use 2 character office if available          
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Read profiles and set values in media table                         *         
***********************************************************************         
                                                                                
         USING PROFINFD,PROFINFO                                                
         XC    WORK,WORK                                                        
         MVI   WORK+00,C'S'                                                     
         MVC   WORK+04(2),AGY                                                   
         MVC   WORK+07(3),CLTVCLT                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTVOFFC                                              
                                                                                
         LARL  R2,MEDTAB                                                        
         USING MEDTABD,R2          R2=A(Media table)                            
         LHI   R0,MEDTABN                                                       
         L     RF,GETPROF          Set up for GETPROF calls below               
         GOTOR ,DMCB,(X'A0',WORK),PROFVALS,DATAMGR,0,PROFINFD                   
                                                                                
SPAN0146 MVC   WORK+06(1),MEDTMED  Set media code from table                    
                                                                                
         MVC   WORK+02(2),=C'BN'   Get BN profile                               
         GOTOR (RF),(R1)                                                        
         MVI   MEDTACAS,MEDTAACT   Set bill actual amount                       
         CLI   PROFVALS+3,C'Y'                                                  
         JE    *+8                                                              
         MVI   MEDTACAS,MEDTAASS   Set bill assigned amount                     
                                                                                
         TM    CLTVFLAG,CLTVFMID   See if Midas client                          
         JZ    *+8                                                              
         MVI   MEDTACAS,MEDTAASS   Also set bill assigned amount                
*                                                                               
         MVC   WORK+02(2),=C'B3'   Get B3 profile                               
         GOTOR (RF),(R1)                                                        
         MVC   MEDTDCON,PROFVALS   Set profile values for media                 
*                                                                               
*      Code no-opted for now that honored a B3 profile for all media            
*      for mindshare. SPAN0147 and SPAN147A are new tags.                       
*                                                                               
*****    CLC   QAGY,=C'H7'         See if Mindshare                             
*****    JNE   SPAN0147                                                         
*****    OC    PROFVALS,PROFVALS   See if I found a profile                     
*****    JZ    SPAN147A            That applies to this client                  
*****    CLI   PROFVALS,0          Special calendar specified?                  
*****    JE    SPAN147A            If not use defaults                          
*****    J     SPAN0148            Honor it, even if not for a media            
*                                                                               
SPAN0147 CLI   PROFIMED,0          Test media specific profile found            
         JNE   SPAN0148                                                         
SPAN147A MVI   MEDTDCTY,1          Set default fiscal start                     
         MVI   MEDTDFMO,1                                                       
         MVI   MEDTDFDT,1                                                       
         MVI   MEDTDFDY,1                                                       
         MVI   MEDTDCTY,0          Set default calendar type                    
         TM    MEDTINDS,MEDTIBRD                                                
         JNZ   SPAN0148                                                         
         MVI   MEDTDCTY,2                                                       
         TM    MEDTINDS,MEDTICAL                                                
         JNZ   SPAN0148                                                         
         DC    H'0'                                                             
                                                                                
SPAN0148 AHI   R2,MEDTABL          Bump to next table entry                     
         JCT   R0,SPAN0146                                                      
         DROP  R2                                                               
                                                                                
         GOTOR BLDCAL              Build a calendar for each media              
                                                                                
***********************************************************************         
* Build product table                                                 *         
***********************************************************************         
                                                                                
B        USING PLSTPSSV,KEY                                                     
         XC    B.PLSTPSSV,B.PLSTPSSV                                            
         MVI   B.PLSTTYPE,PLSTTYPQ                                              
         MVI   B.PLSTSUB,PLSTSUBQ                                               
         MVC   B.PLSTAM,U.NUKAM                                                 
         MVC   B.PLSTCLT,U.NUKCLT                                               
         LARL  R3,PRDTAB                                                        
         USING PRDTABD,R3          R3=A(Product table)                          
         MVC   KEYSAVE,B.PLSTPSSV                                               
         GOTOR DATAMGR,DMCB,DMRDHI,SPTDIR,B.PLSTPSSV,B.PLSTPSSV                 
         JE    SPAN0160                                                         
         DC    H'0'                                                             
                                                                                
SPAN0150 GOTOR DATAMGR,DMCB,DMRSEQ,SPTDIR,B.PLSTPSSV,B.PLSTPSSV                 
         JE    SPAN0160                                                         
         DC    H'0'                                                             
                                                                                
SPAN0160 CLC   B.PLSTPSSV(PLSTXFF-PLSTPSSV),KEYSAVE                             
         JNE   SPAN0170                                                         
                                                                                
         L     R2,ADPRD            Build product table entry                    
         USING PRDHDR,R2                                                        
         GOTOR DATAMGR,DMCB,GETREC,SPTFILE,B.PKEYDA,PRDHDR,DMWORK               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   PRDTPRD,PKEYPRD                                                  
         MVC   PRDTNAM,PNAME                                                    
         OC    PRDTNAM,SPACES                                                   
         MVC   PRDTNUM,PCODE+1                                                  
         AHI   R3,PRDTABL          Bump to next table entry                     
         J     SPAN0150            Get next record                              
         DROP  R2                                                               
                                                                                
SPAN0170 XC    PRDTABD(PRDTABL),PRDTABD                                         
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Build estimate table                                                *         
***********************************************************************         
                                                                                
         LARL  R0,ESTTAB           Initialize estimate table                    
         LAY   R1,ESTTABLN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
E        USING EKEY,KEY            Build estimate table                         
         XC    E.EKEY,E.EKEY                                                    
         MVC   E.EKEYAM,U.NUKAM                                                 
         MVC   E.EKEYCLT,U.NUKCLT                                               
         MVC   E.EKEYPRD,POLPRDC   Set product to 'POL'                         
         MVC   KEYSAVE,E.EKEY                                                   
                                                                                
SPAN0180 CLI   E.EKEYEST,FF        Test highest estimate just done              
         JE    SPAN0200                                                         
                                                                                
         LLC   R0,E.EKEYEST        Bump estimate and get next                   
         AHI   R0,1                                                             
         STC   R0,E.EKEYEST                                                     
         XC    E.EKEYREST,E.EKEYREST                                            
         GOTOR DATAMGR,DMCB,DMRDHI,SPTDIR,E.EKEY,E.EKEY                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   E.EKEY(EKEYEST-EKEY),KEYSAVE                                     
         JNE   SPAN0200                                                         
                                                                                
         L     R2,ADEST                                                         
         USING ESTHDR,R2           R2=A(Estimate record)                        
         GOTOR DATAMGR,DMCB,GETREC,SPTFILE,E.EKDA,ESTHDR,DMWORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LLC   R3,EKEYEST                                                       
         BCTR  R3,0                                                             
         MHI   R3,ESTTABL                                                       
         LARL  R0,ESTTAB                                                        
         AR    R3,R0                                                            
         USING ESTTABD,R3          R3=A(Estimate table entry)                   
         MVC   ESTTNAM,EDESC                                                    
         OC    ESTTNAM,SPACES                                                   
         MVC   ESTTTYP,ETYPE                                                    
         J     SPAN0180            Get next record                              
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Read station billing bucket records                                 *         
***********************************************************************         
                                                                                
S        USING STABUCKD,KEY                                                     
SPAN0200 XC    S.STABUCK(STABKSTS-STABUCK),S.STABUCK                            
         MVI   S.STABPCOD+0,STABPTYQ                                            
         MVI   S.STABPCOD+1,STABPSTQ                                            
         MVC   S.STABPAM,U.NUKAM                                                
         MVC   S.STABPCLT,U.NUKCLT                                              
         MVC   KEYSAVE,S.STABUCK                                                
         GOTOR DATAMGR,DMCB,DMRDHI,SPTDIR,S.STABUCK,S.STABUCK                   
         JE    SPAN0220                                                         
         DC    H'0'                                                             
                                                                                
SPAN0210 GOTOR DATAMGR,DMCB,DMRSEQ,SPTDIR,S.STABUCK,S.STABUCK                   
         JE    SPAN0220                                                         
         DC    H'0'                                                             
                                                                                
SPAN0220 CLC   S.STABUCK(STABPPRD-STABUCK),KEYSAVE                              
         JNE   SPAN0300                                                         
                                                                                
         L     R2,ADBUY                                                         
         USING STABUCKD,R2                                                      
         GOTOR DATAMGR,DMCB,GETREC,SPTFILE,S.STABDA,STABUCKD,DMWORK             
         JNE   SPAN0210                                                         
*        JE    *+6                                                              
*        DC    H'0'                                                             
                                                                                
         MVC   PRDVPRD,S.STABPPRD  Set product code                             
         MVC   ESTVEST,S.STABPEST  Set estimate number                          
                                                                                
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
         SR    R0,R0                                                            
                                                                                
SPAN0230 CLI   STABELEM,0          Test end of record                           
         JE    SPAN0210                                                         
         CLI   STABELEM,STABELQ    Test billing element                         
         JNE   SPAN0250                                                         
                                                                                
         MVC   NETVMED,STABSTYP    Set sub-media code                           
         CLI   NETVMED,C' '                                                     
         JH    *+8                                                              
         MVI   NETVMED,C'N'                                                     
         MVC   PSTVYM,STABPER      Set posting month                            
         GOTOR INIBUF              Initialize buffer record                     
                                                                                
         GOTO1 SPBVAL,DMCB,(C'E',STABELEM),SPBVALD,0                            
*                                                                               
*   must use effective values to allow for UFC/Net billing                      
*                                                                               
         ZAP   DUB,SPBVGRSP                                                     
         CLI   QOPT1,C'N'          Test net request                             
         JNE   *+10                                                             
         ZAP   DUB,SPBVNETP                                                     
*                                                                               
*****    ICM   RF,15,STABGRS                                                    
*****    CLI   QOPT1,C'N'          Test net request                             
*****    JNE   *+8                                                              
*****    ICM   RF,15,STABNET                                                    
*****    CVD   RF,DUB              DUB=Gross/Net billing value                  
                                                                                
         GOTOR GETMON,BUFFBILL     Point to posting month                       
         AP    0(L'BUFFBILL,R1),DUB                                             
         GOTOR PUTBUF              Put record to buffer                         
                                                                                
SPAN0250 IC    R0,STABELEM+1       Bump to next element                         
         AR    R2,R0                                                            
         J     SPAN0230                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Read unit billing records                                           *         
***********************************************************************         
                                                                                
B        USING NUBRECD,XKEY                                                     
SPAN0300 XC    B.NUBK0KEY,B.NUBK0KEY                                            
         MVI   B.NUBK0SYS,NUBK0SYQ                                              
         MVI   B.NUBK0STY,NUBK0STQ                                              
         MVC   B.NUBK0AM,U.NUKAM                                                
         MVC   B.NUBK0CLI,U.NUKCLT                                              
         MVC   B.NUBK0DAT,UNTSTRDT                                              
         MVC   XKEYSAVE,B.NUBK0KEY                                              
                                                                                
SPAN0308 GOTOR DATAMGR,DMCB,DMRDHI,XSPDIR,B.NUBK0KEY,B.NUBK0KEY                 
         JE    SPAN0320                                                         
         DC    H'0'                                                             
                                                                                
SPAN0310 GOTOR DATAMGR,DMCB,DMRSEQ,XSPDIR,B.NUBK0KEY,B.NUBK0KEY                 
         JE    SPAN0320                                                         
         DC    H'0'                                                             
                                                                                
SPAN0320 CLC   B.NUBK0KEY(NUBK0DAT-NUBK0KEY),XKEYSAVE                           
         JNE   SPAN0400                                                         
         CLC   B.NUBK0DAT,UNTENDDT Test higher than end date                    
         JH    SPAN0400                                                         
                                                                                
         OC    NETFILT,NETFILT     Test network filter present                  
         JZ    SPAN0324                                                         
         CLC   B.NUBK0NET,NETFILT  Match network filter                         
         JE    SPAN0324                                                         
         JL    SPAN0322                                                         
         LLC   R0,B.NUBK0TIM                                                    
         AHI   R0,1                                                             
         STC   R0,B.NUBK0TIM                                                    
SPAN0322 MVC   B.NUBK0NET,NETFILT  Set network filter in key                    
         XC    B.NUBK0PRG(NUBKSTAT-NUBK0PRG),B.NUBK0PRG                         
         J     SPAN0308                                                         
                                                                                
SPAN0324 CLI   ESTFILT,0           Test estimate filter present                 
         JE    SPAN0328                                                         
         CLC   B.NUBK0EST,ESTFILT  Match estimate filter                        
         JE    SPAN0328                                                         
         JL    SPAN0326                                                         
         LLC   R0,B.NUBK0PRG+L'NUBK0PRG-1                                       
         AHI   R0,1                                                             
         STC   R0,B.NUBK0PRG+L'NUBK0PRG-1                                       
SPAN0326 MVC   B.NUBK0EST,ESTFILT  Set estimate filter in key                   
         XC    B.NUBK0SUB(NUBKSTAT-NUBK0SUB),B.NUBK0SUB                         
         J     SPAN0308                                                         
                                                                                
SPAN0328 L     R2,ADBUY                                                         
         USING NUBRECD,R2          R2=A(Unit billing record)                    
         GOTOR DATAMGR,DMCB,GETREC,XSPFIL,B.NUBDA,NUBRECD,DMWORK                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETMED,NUBK0NET     Set sub-media code for network               
         CLI   QOPT3,C' '          Test media filter set                        
         JE    *+14                                                             
         CLC   QOPT3,NETVMED       Match media filter                           
         JNE   SPAN0310                                                         
         MVC   ESTVEST,NUBK0EST    Set estimate number                          
                                                                                
         GOTOR GETMOS,NUBK0DAT     Establish posting month                      
         GOTOR GETMON,BUFFBILL     Point to posting month                       
                                                                                
         AHI   R2,NUBELDQ                                                       
         USING NBILD,R2            R2=A(Billing element)                        
         SR    R0,R0                                                            
                                                                                
SPAN0330 CLI   NBILEL,0            Test end of record                           
         JE    SPAN0310                                                         
         CLI   NBILEL,NBILELQ      Test billing element                         
         JNE   SPAN0370                                                         
         TM    NBILST,NBILUBQ      Test unbilled                                
         JNZ   SPAN0370                                                         
                                                                                
         CLI   NBILLEN,NBILELNQ                                                 
         JL    SPAN0340                                                         
         OC    NBILPRDC,NBILPRDC   Test product code present                    
         JZ    SPAN0340                                                         
         MVC   PRDVPRD,NBILPRDC    Yes - use product code in element            
         J     SPAN0350                                                         
                                                                                
SPAN0340 MVC   PRDVPRD,POLPRDC     Preset 'POL' product code                    
         CLI   NBILPRD,0           Test billing product set                     
         JE    SPAN0350                                                         
         GOTOR GETPRC,NBILPRD      Look-up product number in table              
                                                                                
SPAN0350 GOTOR INIBUF              Initialize buffer record                     
                                                                                
         TM    CLTVFLAG,CLTVFMID   See if Midas client                          
         JZ    SPAN0351                                                         
         ICM   RF,15,NBILGR2       report cos2 billing                          
         CLI   QOPT1,C'G'          Gross report?                                
         JE    SPAN0352                                                         
*                                                                               
*        DERIVE NET FROM COST2 GROSS                                            
*                                                                               
         ICM   RF,15,NBILGR2                                                    
         LTR   RF,RF                                                            
         JZ    SPAN0352            Is there any cost2 gross                     
*                                                                               
         CLC   NBILCHGT,NUSPRAMI    Midas trade credit?                         
         JE    SPAN0352          if so, don't net down cost2 gross              
*                                                                               
         LHI   RF,100                                                           
         LHI   RE,85                                                            
         L     R1,NBILGR2                                                       
         SR    R0,R0                                                            
         MR    R0,RE                                                            
         DR    R0,RF                                                            
         LR    RF,R1                                                            
         J     SPAN0352            RF now calculated net                        
                                                                                
SPAN0351 DS    0H                                                               
*                                                                               
*        SET EFFECTIVE VALUES INTO ELEM                                         
*        CAN DO SINCE RECORD IS NOT WRITTEN BACK                                
*                                                                               
*        code below like SPBVAL's                                               
*        it doesn't support this kind of billing element                        
*        only those in the unit                                                 
*                                                                               
         ICM   R0,15,NBILGRS                                                    
         ICM   R1,15,NBILNET                                                    
         TM    NBILST,NBILSCQ+NBILSNQ    sep comm bill + net                    
         JZ    SAN0351N                                                         
*                                                                               
         TM    NBILST,NBILSCQ      see if UFC                                   
         JZ    SAN0351B                                                         
*                                                                               
         SR    R0,R1               set nbilgrs to gross-net                     
         ST    R0,NBILGRS                                                       
         XC    NBILNET,NBILNET     clear the net billed                         
         J     SAN0351N                                                         
*                                                                               
SAN0351B DS    0H              UFC/Net bill                                     
         ST    R1,NBILGRS                                                       
         ST    R1,NBILNET                                                       
*                                                                               
SAN0351N ICM   RF,15,NBILGRS                                                    
         CLI   QOPT1,C'N'          Test net request                             
         JNE   *+8                                                              
         ICM   RF,15,NBILNET                                                    
                                                                                
SPAN0352 CVD   RF,DUB              DUB=gross/net billing value                  
                                                                                
         L     R1,ABUFMON          Point to posting month                       
         AP    0(L'BUFFBILL,R1),DUB                                             
         GOTOR PUTBUF              Put record to buffer                         
                                                                                
SPAN0370 ZIC   R0,NBILLEN          Bump to next billing element                 
         AR    R2,R0                                                            
         J     SPAN0330                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Read unit record and process                                        *         
***********************************************************************         
                                                                                
SPAN0400 XC    UNTVALS(UNTVALL),UNTVALS                                         
         MVC   ESTVEST,U.NUKEST    Set estimate number                          
                                                                                
         LLC   R1,ESTVEST          Locate estimate table entry                  
         BCTR  R1,0                                                             
         MHI   R1,ESTTABL                                                       
         LARL  R0,ESTTAB                                                        
         AR    R1,R0                                                            
         ST    R1,AESTTAB          Set A(Estimate table entry)                  
         CLI   QOPT4,C'Y'          Test including STW estimates                 
         JE    SPAN0402                                                         
         CLI   ESTTTYP-ESTTABD(R1),C'S'                                         
         JE    SPAN0130            Ignore STW estimates                         
                                                                                
SPAN0402 L     R2,ADBUY                                                         
         USING NURECD,R2           R2=A(Unit record)                            
         GOTOR DATAMGR,DMCB,GETREC,UNTFIL,U.NUDA,NURECD,DMWORK                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   UNTVDATE,NUKDATE    Set unit date                                
                                                                                
***********************************************************************         
* Get addresses of unit elements and extract some useful fields       *         
***********************************************************************         
                                                                                
         GOTOR GETMED,NUKNET       Look-up sub-media code                       
         CLI   QOPT3,C' '          Test media filter set                        
         JE    *+14                                                             
         CLC   QOPT3,NETVMED       Match media filter                           
         JNE   SPAN1000                                                         
                                                                                
         GOTOR UNTTRC              Trace unit                                   
                                                                                
         LA    R3,NUDATA           Locate required elements on unit             
         SR    R0,R0                                                            
                                                                                
SPAN0410 CLI   0(R3),0             Test end of unit record                      
         JE    SPAN0470                                                         
                                                                                
         USING NUMAINEL,R3                                                      
         CLI   NUMAINEL,NUMAIELQ   Test main element                            
         JNE   SPAN0420                                                         
                                                                                
         TM    NUPACKST,UNTVPLKQ+UNTVPNSQ                                       
         JNZ   SPAN1000            Drop Locked/No-show packages                 
                                                                                
         ST    R3,UNTVAX01                                                      
         MVC   UNTVPKST,NUPACKST   Set package status                           
         MVC   UNTVSTAT,NUUNITST   Set unit status                              
         MVC   UNTVPPCT,NUP1SHR    Set product 1 share                          
                                                                                
         TM    UNTVSTAT,UNTVSPEQ+UNTVSMIQ                                       
         JNZ   SPAN0460            No ordered/assigned for these units          
                                                                                
         ICM   RF,15,NUACTUAL                                                   
         CVD   RF,UNTVORDR         Set actual (ordered) cost                    
                                                                                
         ICM   RF,15,NUINTEG                                                    
         CVD   RF,UNTVINTG         Set integration cost                         
                                                                                
         ICM   RF,15,NUASSIGN      Get assigned cost (binary)                   
         CVD   RF,UNTVASSN         Set assigned cost (packed)                   
                                                                                
         TM    UNTVSTAT,UNTVSMUQ   Test minus unit                              
         JZ    SPAN0460                                                         
         MP    UNTVORDR,=P'-1'                                                  
         MP    UNTVINTG,=P'-1'                                                  
         MP    UNTVASSN,=P'-1'                                                  
         J     SPAN0460                                                         
                                                                                
         USING NUSDRD,R3                                                        
SPAN0420 CLI   NUSDREL,NUSDRELQ    Test second standard element                 
         JNE   SPAN0430                                                         
                                                                                
         ST    R3,UNTVAX02                                                      
         MVC   UNTVSTA3,NUSDST3    Set unit status 3                            
         MVC   UNTVRTCV,NUSDRTCV   Set rate coverage                            
         MVC   UNTVSRTC,NUSDSRT    Set special rate type                        
         J     SPAN0460                                                         
                                                                                
         USING NUSPRD,R3                                                        
SPAN0430 CLI   NUSPREL,NUSPRELQ    Test special rate element                    
         JNE   SPAN0434                                                         
         CLC   NUSPRTYP,NUSPRAMI   Test midas                                   
         JE    SPAN0431                                                         
         CLC   NUSPRTYP,NUSPRACC   Test barter actual credit element            
         JNE   SPAN0432                                                         
         OC    UNTVAXAC,UNTVAXAC   Test only one barter actual element          
         JZ    *+6                                                              
         DC    H'0'                                                             
         ST    R3,UNTVAXAC                                                      
         J     SPAN0460                                                         
                                                                                
SPAN0431 OC    UNTVAXAG,UNTVAXAG   Test only one barter assign element          
         JZ    *+6                                                              
         DC    H'00'                                                            
         ST    R3,UNTVAXAG                                                      
         J     SPAN0460                                                         
                                                                                
SPAN0432 OC    UNTVAX03,UNTVAX03                                                
         JNZ   SPAN0460                                                         
         ST    R3,UNTVAX03                                                      
         J     SPAN0460                                                         
                                                                                
         USING NUASCD,R3                                                        
SPAN0434 CLI   NUASCEL,NUASCELQ    Test barter assign credit                    
         JNE   SPAN0440                                                         
         OC    UNTVAXAG,UNTVAXAG   Test only one barter assign element          
         JZ    *+6                                                              
         DC    H'00'                                                            
         ST    R3,UNTVAXAG                                                      
         J     SPAN0460                                                         
                                                                                
         USING NUPAYD,R3                                                        
SPAN0440 CLI   NUPAYEL,NUPAYELQ    Test pay element                             
         JNE   SPAN0450                                                         
         OC    UNTVAX12,UNTVAX12                                                
         JNZ   SPAN0460                                                         
         ST    R3,UNTVAX12                                                      
         J     SPAN0460                                                         
                                                                                
         USING NUPRDD,R3                                                        
SPAN0450 CLI   NUPRDEL,NUPRDELQ    Test old style product element               
         JNE   *+12                                                             
         ST    R3,UNTVAX14                                                      
         J     SPAN0460                                                         
                                                                                
         USING NUPDED,R3                                                        
         CLI   NUPDEEL,NUPDEELQ    Test new style product element               
         JNE   *+12                                                             
         ST    R3,UNTVAX19                                                      
         J     SPAN0460                                                         
                                                                                
         USING NUOTH,R3                                                         
         CLI   NUOTEL,NUOTELQ      Test other element                           
         JNE   SPAN0460                                                         
         CLI   NUOTTYP,C'N'        Test integration product                     
         JNE   SPAN0460                                                         
         MVC   UNTVIPRD,NUOTHER    Set integration product                      
                                                                                
SPAN0460 IC    R0,NUOTLEN          Bump to next element on record               
         AR    R3,R0                                                            
         J     SPAN0410                                                         
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Point to posting month bucket for this unit                         *         
***********************************************************************         
                                                                                
SPAN0470 GOTOR GETMOS,UNTVDATE     Establish posting month                      
         GOTOR GETMON,BUFFORDR     Point to posting month                       
                                                                                
***********************************************************************         
* Build table of products and cost percentages                        *         
***********************************************************************         
                                                                                
         LARL  R3,PCTTAB                                                        
         USING PCTTABD,R3          R3=A(Product percentage table)               
                                                                                
         ICM   R2,15,UNTVAX19      Point to NUPDED                              
         JZ    SPAN0550                                                         
         USING NUPDED,R2                                                        
         MVC   BYTE,NUPDEIND       Save indicator byte                          
         CLI   NUPDELEN,NUPDEDL    Test one product in element                  
         JE    SPAN0510                                                         
         OC    UNTVPPCT,UNTVPPCT   Test product 1 share is set                  
         JNZ   SPAN0510                                                         
         LHI   R0,5000                                                          
         STCM  R0,3,UNTVPPCT       No - set to 50%                              
SPAN0510 LLC   R0,NUPDELEN                                                      
         SHI   R0,NUPDEPR-NUPDED   R0=Length of product entries                 
         LA    R2,NUPDEPR                                                       
         USING NUPDEPR,R2          R2=A(Product entry)                          
SPAN0520 MVC   PCTTPRD,NUPDEPR     Set product code                             
         MVC   PCTTPCT,NUPDEPCT    Set product percentage                       
         TM    BYTE,X'C0'          Test more than 2 products                    
         JNZ   *+14                                                             
         MVC   PCTTPCT,UNTVPPCT    Set product 1 percentage                     
         J     SPAN0530                                                         
         OC    PCTTPCT,PCTTPCT     Don't update if zero cost percentage         
         JZ    SPAN0540                                                         
SPAN0530 AHI   R3,PCTTABL          Bump to next table entry                     
SPAN0540 AHI   R2,NUPDTLEN         Bump to next product in element              
         SHI   R0,NUPDTLEN         Decrement entry length                       
         JNZ   SPAN0520            Do next product                              
         J     SPAN0640                                                         
                                                                                
SPAN0550 ICM   R2,15,UNTVAX14      Point to NUPRDD                              
         JZ    SPAN0600                                                         
         USING NUPRDD,R2                                                        
         CLI   NUPRDLEN,NUPRDDL    Test one product in element                  
         JE    SPAN0560                                                         
         OC    UNTVPPCT,UNTVPPCT   Test product 1 share is set                  
         JNZ   SPAN0560                                                         
         LHI   R0,5000                                                          
         STCM  R0,3,UNTVPPCT       No - set to 50%                              
SPAN0560 LLC   R0,NUPRDLEN                                                      
         SHI   R0,NUPRDPR-NUPRDD   R0=Length of product entries                 
         LA    R2,NUPRDPR                                                       
         USING NUPRDPR,R2          R2=A(Product entry)                          
SPAN0570 GOTOR GETPRC,NUPRDPR      Look-up product code                         
         MVC   PCTTPRD,PRDVPRD     Set product code                             
         MVC   PCTTPCT,NUPRDPCT    Set product percentage                       
         OC    PCTTPCT,PCTTPCT     Don't update if zero cost percentage         
         JZ    SPAN0590                                                         
SPAN0580 AHI   R3,PCTTABL          Bump to next table entry                     
SPAN0590 AHI   R2,NUPRPLEN         Bump to next product in element              
         SHI   R0,NUPRPLEN         Decrement entry length                       
         JNZ   SPAN0570            Do next product                              
         J     SPAN0640                                                         
                                                                                
SPAN0600 ICM   R2,15,UNTVAX01      Point to NUMAINEL                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R2                                                      
         CLI   NUPRD,0             Test allocated                               
         JE    SPAN0630                                                         
         CLI   NUPRD2,0            Test second product allocated                
         JE    SPAN0610                                                         
         CLC   NUPRD,NUPRD2        or first=second product                      
         JE    SPAN0610                                                         
         TM    NUUNST2,X'04'       Test product 1 zero allocation               
         JNZ   SPAN0620                                                         
         GOTOR GETPRC,NUPRD        Build entry for product 1                    
         MVC   PCTTPRD,PRDVPRD                                                  
         MVC   PCTTPCT,UNTVPPCT                                                 
         LHI   R0,5000                                                          
         OC    PCTTPCT,PCTTPCT                                                  
         JNZ   *+8                                                              
         STCM  R0,3,PCTTPCT        Set 50% if none specified                    
         AHI   R3,PCTTABL                                                       
         GOTOR GETPRC,NUPRD2                                                    
         MVC   PCTTPRD,PRDVPRD                                                  
         AHI   R3,PCTTABL                                                       
         J     SPAN0640                                                         
                                                                                
SPAN0610 GOTOR GETPRC,NUPRD        Build single entry for product 1             
         MVC   PCTTPRD,PRDVPRD                                                  
         AHI   R3,PCTTABL                                                       
         J     SPAN0640                                                         
                                                                                
SPAN0620 GOTOR GETPRC,NUPRD2       Build single entry for product 2             
         MVC   PCTTPRD,PRDVPRD                                                  
         AHI   R3,PCTTABL                                                       
         J     SPAN0640                                                         
         DROP  R2                                                               
                                                                                
SPAN0630 MVC   PCTTPRD,POLPRDC     Build single entry for product 'POL'         
         AHI   R3,PCTTABL                                                       
                                                                                
SPAN0640 SHI   R3,PCTTABL                                                       
         MVI   PCTTPCT,PCTTPLEQ    Set this is last entry                       
         LARL  R3,PCTTAB           and point back to first entry                
                                                                                
***********************************************************************         
* Calculate net and gross amounts for ordered and assigned costs      *         
***********************************************************************         
                                                                                
         TM    UNTVSTAT,UNTVSPEQ+UNTVSMIQ                                       
         JNZ   SPAN0810            No ordered/assigned for these units          
                                                                                
         GOTOR NETNET,DMCB,(UNTVSRTC,UNTVORDR),(C'P',UNTVOGRS)                  
                                                                                
         ZAP   UNTVIGRS,UNTVINTG   Preset gross=net                             
         ZAP   UNTVINET,UNTVINTG                                                
         TM    UNTVPKST,UNTVPINQ   Test integration non-commissionable          
         JNZ   SPAN0644                                                         
         MVC   BYTE,UNTVSRTC                                                    
         CLI   UNTVRTCV,0                                                       
         JE    SPAN0642                                                         
         CLI   UNTVRTCV,C'A'                                                    
         JE    SPAN0642                                                         
         MVI   BYTE,0                                                           
                                                                                
SPAN0642 GOTOR NETNET,DMCB,(BYTE,UNTVINTG),(C'P',UNTVIGRS)                      
                                                                                
SPAN0644 TM    UNTVSTAT,UNTVSACQ   Test assigned cost input                     
         JNZ   SPAN0646                                                         
         CP    UNTVASSN,=P'0'      Test assigned cost present                   
         JNE   SPAN0646                                                         
*                                                                               
         ZAP   UNTVAGRS,UNTVOGRS   No - use actual amounts                      
         ZAP   UNTVANET,UNTVONET                                                
         J     SPAN0648                                                         
                                                                                
SPAN0646 GOTOR NETNET,DMCB,(UNTVSRTC,UNTVASSN),(C'P',UNTVAGRS)                  
                                                                                
SPAN0648 CLI   QOPT1,C'N'          Test net request                             
         JE    SPAN0652                                                         
                                                                                
         ZAP   UNTVORDR,UNTVOGRS   Set gross amounts                            
         ZAP   UNTVINTG,UNTVIGRS                                                
         ZAP   UNTVASSN,UNTVAGRS                                                
         OC    UNTVIPRD,UNTVIPRD   Test integration product set                 
         JNZ   SPAN0656                                                         
         AP    UNTVORDR,UNTVIGRS   Add integration amount                       
         TM    UNTVSTA3,UNTVSAOQ   Test assigned manually overridden            
         JNZ   SPAN0650                                                         
         TM    UNTVSTAT,UNTVSACQ   Test assigned cost on unit                   
         JZ    SPAN0650                                                         
         TM    CLTVFLAG,CLTVFIAC   Test integration in assigned cost            
         JNZ   SPAN0656                                                         
SPAN0650 AP    UNTVASSN,UNTVIGRS                                                
         J     SPAN0656                                                         
                                                                                
SPAN0652 ZAP   UNTVORDR,UNTVONET   Set net amounts                              
         ZAP   UNTVINTG,UNTVINET                                                
         ZAP   UNTVASSN,UNTVANET                                                
         OC    UNTVIPRD,UNTVIPRD   Test integration product set                 
         JNZ   SPAN0656                                                         
         AP    UNTVORDR,UNTVINET   Add integration amount                       
         TM    UNTVSTA3,UNTVSAOQ   Test assigned manually overridden            
         JNZ   SPAN0654                                                         
         TM    UNTVSTAT,UNTVSACQ   Test assigned cost on unit                   
         JZ    SPAN0654                                                         
         TM    CLTVFLAG,CLTVFIAC   Test integration in assigned cost            
         JNZ   SPAN0656                                                         
SPAN0654 AP    UNTVASSN,UNTVINET                                                
                                                                                
SPAN0656 ZAP   UNTVOUNA,UNTVORDR   Set totals (for decrementing)                
         ZAP   UNTVAUNA,UNTVASSN                                                
                                                                                
***********************************************************************         
* Allocate actual and assigned values to each product on unit         *         
***********************************************************************         
                                                                                
SPAN0660 CLI   PCTTPCT,PCTTPLEQ    Test last entry                              
         JNE   SPAN0670                                                         
         ZAP   PCTTORDR,UNTVOUNA   Last product has remainder                   
         ZAP   PCTTASSN,UNTVAUNA                                                
         J     SPAN0680                                                         
                                                                                
SPAN0670 SR    R0,R0                                                            
         ICM   R0,3,PCTTPCT        There must be a split percent                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   R0,DUB              DUB=Percentage split (2dp)                   
                                                                                
         ZAP   WORK(16),UNTVORDR   Calculate ordered share                      
         MP    WORK(16),DUB                                                     
         SRP   WORK(16),64-4,5                                                  
         ZAP   PCTTORDR,WORK(16)                                                
         SP    UNTVOUNA,WORK(16)                                                
                                                                                
         ZAP   WORK(16),UNTVASSN   Calculate assigned share                     
         MP    WORK(16),DUB                                                     
         SRP   WORK(16),64-4,5                                                  
         ZAP   PCTTASSN,WORK(16)                                                
         SP    UNTVAUNA,WORK(16)                                                
                                                                                
         AHI   R3,PCTTABL          Bump to next table entry                     
         J     SPAN0660                                                         
                                                                                
***********************************************************************         
* Build a buffer record for each product                              *         
***********************************************************************         
                                                                                
SPAN0680 LARL  R3,PCTTAB           Point to product percent table               
                                                                                
SPAN0690 GOTOR OASTRC              Trace ordered and assigned                   
                                                                                
         MVC   PRDVPRD,PCTTPRD     Set product code                             
         GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFORDR-BUFFORDR(L'BUFFORDR,R1),PCTTORDR                        
         ZAP   BUFFASSN-BUFFORDR(L'BUFFASSN,R1),PCTTASSN                        
         GOTOR PUTBUF              Put record to buffer                         
         CLI   PCTTPCT,PCTTPLEQ    Test last product in table                   
         JE    *+12                                                             
         AHI   R3,PCTTABL          No - bump and post the next one              
         J     SPAN0690                                                         
         DROP  R3                                                               
                                                                                
         OC    UNTVIPRD,UNTVIPRD   Test integration product set                 
         JZ    SPAN0700                                                         
         MVC   PRDVPRD,UNTVIPRD    Set product code                             
         GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFORDR-BUFFORDR(L'BUFFORDR,R1),UNTVINTG                        
         TM    UNTVSTA3,UNTVSAOQ   Test assigned manually overridden            
         JNZ   SPAN0692                                                         
         TM    UNTVSTAT,UNTVSACQ   Test assigned cost on unit                   
         JZ    SPAN0692                                                         
         TM    CLTVFLAG,CLTVFIAC   Test integration in assigned cost            
         JNZ   SPAN0694                                                         
SPAN0692 ZAP   BUFFASSN-BUFFORDR(L'BUFFASSN,R1),UNTVINTG                        
                                                                                
SPAN0694 GOTOR INTTRC              Trace integration                            
                                                                                
         GOTOR PUTBUF              Put record to buffer                         
                                                                                
***********************************************************************         
* Deal with special rates                                             *         
***********************************************************************         
                                                                                
SPAN0700 ICM   R2,15,UNTVAX03                                                   
         JZ    SPAN0792                                                         
         USING NUSPRD,R2           R2=A(Special rate element)                   
                                                                                
SPAN0710 CLC   NUSPRTYP,NUSPRACC   Test barter actual credit element            
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   UNTVRTYP,NUSPRTYP   Set special rate type                        
                                                                                
         ICM   RF,15,NUSPRAMT      Set special rate amount                      
         CVD   RF,UNTVORDR                                                      
         TM    UNTVSTAT,UNTVSMUQ   Test minus unit                              
         JZ    *+10                                                             
         MP    UNTVORDR,=P'-1'                                                  
                                                                                
         MVI   BYTE,C'F'           Non-commissionable rate type                 
         CLI   NUSPRCOM,C'C'       Test commissionable                          
         JNE   SPAN0720                                                         
         MVC   BYTE,UNTVSRTC                                                    
         CLI   UNTVRTCV,0                                                       
         JE    SPAN0720                                                         
         CLI   UNTVRTCV,C'A'                                                    
         JE    SPAN0720                                                         
         MVI   BYTE,0                                                           
                                                                                
SPAN0720 GOTOR NETNET,DMCB,(BYTE,UNTVORDR),(C'P',UNTVOGRS)                      
                                                                                
         ZAP   UNTVORDR,UNTVOGRS                                                
         CLI   QOPT1,C'N'          Test net option                              
         JNE   *+10                                                             
         ZAP   UNTVORDR,UNTVONET                                                
                                                                                
         CLI   NUSPRLEN,NUSPRLN4                                                
         JL    SPAN0730                                                         
         OC    NUSPRBPC,NUSPRBPC   Test billing product code set                
         JZ    SPAN0730                                                         
         MVC   PRDVPRD,NUSPRBPC                                                 
         J     SPAN0740                                                         
                                                                                
SPAN0730 CLI   NUSPRLEN,NUSPRLN2                                                
         JL    SPAN0750                                                         
         CLI   NUSPRBPR,0          Test billing product number set              
         JE    SPAN0750                                                         
         GOTOR GETPRC,NUSPRBPR     Set product code                             
                                                                                
SPAN0740 GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFORDR-BUFFORDR(L'BUFFORDR,R1),UNTVORDR                        
         TM    UNTVSTA3,UNTVSAOQ   Test assigned manually overridden            
         JNZ   SPAN0742                                                         
         TM    UNTVSTAT,UNTVSACQ   Test assigned cost on unit                   
         JZ    SPAN0742                                                         
         LARL  RF,PCTTAB                                                        
         CLC   PRDVPRD,PCTTPRD-PCTTABD(RF)                                      
         JNE   SPAN0742                                                         
         TM    CLTVFLAG,CLTVFSAC   Test special in assigned cost                
         JNZ   SPAN0744                                                         
SPAN0742 ZAP   BUFFASSN-BUFFORDR(L'BUFFASSN,R1),UNTVORDR                        
                                                                                
SPAN0744 GOTOR SPCTRC              Trace special rate                           
                                                                                
         GOTOR PUTBUF              Put record to buffer                         
         J     SPAN0790                                                         
                                                                                
***********************************************************************         
* Allocate special rate amounts to each product on unit               *         
***********************************************************************         
                                                                                
SPAN0750 LARL  R3,PCTTAB                                                        
         USING PCTTABD,R3          R3=A(Product percentage table)               
         ZAP   UNTVOUNA,UNTVORDR   Set total (for decrementing)                 
                                                                                
SPAN0760 CLI   PCTTPCT,PCTTPLEQ    Test last entry                              
         JNE   *+14                                                             
         ZAP   PCTTORDR,UNTVOUNA   Last product has remainder                   
         J     SPAN0770                                                         
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,PCTTPCT        There must be a split percent                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RF,DUB              DUB=Percentage split (2dp)                   
                                                                                
         ZAP   WORK(16),UNTVORDR   Calculate product share                      
         MP    WORK(16),DUB                                                     
         SRP   WORK(16),64-4,5                                                  
         ZAP   PCTTORDR,WORK(16)                                                
         SP    UNTVOUNA,WORK(16)                                                
                                                                                
         AHI   R3,PCTTABL          Bump to next table entry                     
         J     SPAN0760                                                         
                                                                                
***********************************************************************         
* Build a buffer record for each product                              *         
***********************************************************************         
                                                                                
SPAN0770 LARL  R3,PCTTAB           Point to product percent table               
SPAN0780 MVC   PRDVPRD,PCTTPRD     Set product code                             
         GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFORDR-BUFFORDR(L'BUFFORDR,R1),PCTTORDR                        
         TM    UNTVSTA3,UNTVSAOQ   Test assigned manually overridden            
         JNZ   SPAN0782                                                         
         TM    UNTVSTAT,UNTVSACQ   Test assigned cost on unit                   
         JZ    SPAN0782                                                         
         TM    CLTVFLAG,CLTVFSAC   Test special in assigned cost                
         JNZ   SPAN0784                                                         
SPAN0782 ZAP   BUFFASSN-BUFFORDR(L'BUFFASSN,R1),PCTTORDR                        
                                                                                
SPAN0784 GOTOR SPCTRC              Trace special rate                           
                                                                                
         GOTOR PUTBUF              Put record to buffer                         
         CLI   PCTTPCT,PCTTPLEQ    Test last product in table                   
         JE    SPAN0790                                                         
         AHI   R3,PCTTABL          No - bump and post the next one              
         J     SPAN0780                                                         
         DROP  R3                                                               
                                                                                
SPAN0790 LLC   R0,NUSPRLEN         Bump to next element on unit                 
         AR    R2,R0                                                            
         CLI   NUSPREL,NUSPRELQ    Test special rate element                    
         JE    SPAN0710            Yes - go process it                          
                                                                                
***********************************************************************         
* Deal with barter actual credit                                      *         
***********************************************************************         
                                                                                
SPAN0792 ICM   R2,15,UNTVAXAC                                                   
         JZ    SPAN0798                                                         
         USING NUSPRD,R2           R2=A(Barter actual credit element)           
                                                                                
         MVC   UNTVRTYP,NUSPRTYP   Set barter actual credit type                
                                                                                
         ICM   RF,15,NUSPRAMT      Set barter actual credit amount              
         CVD   RF,UNTVORDR                                                      
         TM    UNTVSTAT,UNTVSMUQ   Test minus unit                              
         JZ    *+10                                                             
         MP    UNTVORDR,=P'-1'                                                  
                                                                                
         MVC   PRDVPRD,NUSPRBPC    Set barter actual credit product             
                                                                                
         MVC   BYTE,UNTVSRTC                                                    
         CLI   UNTVRTCV,0                                                       
         JE    SPAN0794                                                         
         CLI   UNTVRTCV,C'A'                                                    
         JE    SPAN0794                                                         
         MVI   BYTE,0                                                           
                                                                                
SPAN0794 GOTOR NETNET,DMCB,(BYTE,UNTVORDR),(C'P',UNTVOGRS)                      
                                                                                
         ZAP   UNTVORDR,UNTVOGRS                                                
         CLI   QOPT1,C'N'          Test net option                              
         JNE   *+10                                                             
         ZAP   UNTVORDR,UNTVONET                                                
                                                                                
         CLI   PRDVPRD,0           Test barter actual credit product            
         JNE   SPAN0796                                                         
         LARL  R3,PCTTAB                                                        
         USING PCTTABD,R3          R3=A(Product percentage table)               
         MVC   PRDVPRD,PCTTPRD     Set barter actual credit product             
                                                                                
SPAN0796 GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFORDR-BUFFORDR(L'BUFFORDR,R1),UNTVORDR                        
                                                                                
         GOTOR SPCTRC              Trace barter actual credit                   
                                                                                
         GOTOR PUTBUF              Put record to buffer                         
                                                                                
***********************************************************************         
* Deal with barter assign credit                                      *         
***********************************************************************         
                                                                                
SPAN0798 ICM   R2,15,UNTVAXAG                                                   
         JZ    SPAN0810                                                         
         USING NUASCD,R2           R2=A(Barter assign credit element)           
                                                                                
         CLI   NUASCEL,NUSPRELQ    Might be a midas SPR element                 
         JE    *+10                                                             
                                                                                
         MVC   UNTVRTYP,NUASCTYP   Set barter assign credit type                
                                                                                
         ICM   RF,15,NUASCAMT      Set barter assign credit amount              
*                                  can do since $ are in the same place         
         CVD   RF,UNTVASSN                                                      
         TM    UNTVSTAT,UNTVSMUQ   Test minus unit                              
         JZ    *+10                                                             
         MP    UNTVASSN,=P'-1'                                                  
                                                                                
         CLI   NUASCEL,NUSPRELQ    Might be a midas SPR element                 
         JE    *+10                if so can't set product here                 
         MVC   PRDVPRD,NUASCBPC    Set barter assign credit product             
                                                                                
         MVC   BYTE,UNTVSRTC                                                    
         CLI   UNTVRTCV,0                                                       
         JE    SPAN0800                                                         
         CLI   UNTVRTCV,C'A'                                                    
         JE    SPAN0800                                                         
         MVI   BYTE,0                                                           
                                                                                
SPAN0800 GOTOR NETNET,DMCB,(BYTE,UNTVASSN),(C'P',UNTVAGRS)                      
                                                                                
         ZAP   UNTVASSN,UNTVAGRS                                                
         CLI   NUASCEL,NUSPRELQ    Might be a midas SPR element                 
         JE    *+18                if so always use gross                       
                                                                                
         CLI   QOPT1,C'N'          Test net option                              
         JNE   *+10                                                             
         ZAP   UNTVASSN,UNTVANET                                                
                                                                                
         CLI   PRDVPRD,0           Test barter assign credit product            
         JNE   SPAN0802                                                         
         LARL  R3,PCTTAB                                                        
         USING PCTTABD,R3          R3=A(Product percentage table)               
         MVC   PRDVPRD,PCTTPRD     Set barter assign credit product             
                                                                                
SPAN0802 GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFASSN-BUFFORDR(L'BUFFASSN,R1),UNTVASSN                        
                                                                                
         GOTOR SPCTRC              Trace barter assign credit                   
                                                                                
         GOTOR PUTBUF              Put record to buffer                         
                                                                                
***********************************************************************         
* Deal with unit pay elements                                         *         
***********************************************************************         
                                                                                
SPAN0810 ICM   R2,15,UNTVAX12                                                   
         JZ    SPAN1000                                                         
         USING NUPAYD,R2           R2=A(Pay element)                            
                                                                                
SPAN0820 ICM   RF,15,NUPAYGRS                                                   
         CLI   QOPT1,C'N'          Test net option                              
         JNE   *+8                                                              
         ICM   RF,15,NUPAYNET                                                   
         CVD   RF,UNTVPAID                                                      
                                                                                
         CLI   NUPAYTYP,C'T'       Test time payment                            
         JE    SPAN0860                                                         
         CLI   NUPAYTYP,C'I'       Test integration payment                     
         JE    SPAN0860                                                         
                                                                                
         ICM   R3,15,UNTVAX03      Find appropriate special rate                
         JZ    SPAN0860                                                         
         USING NUSPRD,R3                                                        
         SR    R0,R0                                                            
SPAN0822 CLC   NUSPRTYP,NUPAYTYP   Match type                                   
         JE    SPAN0830                                                         
         IC    R0,NUSPRLEN                                                      
         AR    R3,R0                                                            
         CLI   NUSPREL,NUSPRELQ    Test special rate element                    
         JE    SPAN0822                                                         
         J     SPAN0860                                                         
                                                                                
SPAN0830 CLI   NUSPRLEN,NUSPRLN4                                                
         JL    SPAN0840                                                         
         OC    NUSPRBPC,NUSPRBPC   Test billing product code set                
         JZ    SPAN0840                                                         
         MVC   PRDVPRD,NUSPRBPC    Set product code                             
         J     SPAN0850                                                         
                                                                                
SPAN0840 CLI   NUSPRLEN,NUSPRLN2                                                
         JL    SPAN0860                                                         
         CLI   NUSPRBPR,0          Test billing product number set              
         JE    SPAN0860                                                         
         GOTOR GETPRC,NUSPRBPR     Set billing product code                     
         DROP  R3                                                               
                                                                                
SPAN0850 GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFPAID-BUFFORDR(L'BUFFPAID,R1),UNTVPAID                        
         GOTOR PAYTRC,NUPAYTYP     Trace payment                                
         GOTOR PUTBUF              Put record to buffer                         
         J     SPAN0900                                                         
                                                                                
***********************************************************************         
* Allocate paid amount to each product on unit                        *         
***********************************************************************         
                                                                                
SPAN0860 LARL  R3,PCTTAB                                                        
         USING PCTTABD,R3          R3=A(Product percentage table)               
         ZAP   UNTVPUNA,UNTVPAID   Set total (for decrementing)                 
SPAN0870 CLI   PCTTPCT,PCTTPLEQ    Test last entry                              
         JNE   *+14                                                             
         ZAP   PCTTPAID,UNTVPUNA   Last product has remainder                   
         J     SPAN0880                                                         
         SR    R0,R0                                                            
         ICM   R0,3,PCTTPCT        There must be a split percent                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   R0,DUB              DUB=Percentage split (2dp)                   
         ZAP   WORK(16),UNTVPAID   Calculate product share                      
         MP    WORK(16),DUB                                                     
         SRP   WORK(16),64-4,5                                                  
         ZAP   PCTTPAID,WORK(16)                                                
         SP    UNTVPUNA,WORK(16)                                                
         AHI   R3,PCTTABL          Bump to next table entry                     
         J     SPAN0870                                                         
                                                                                
***********************************************************************         
* Build a buffer record for each product                              *         
***********************************************************************         
                                                                                
SPAN0880 LARL  R3,PCTTAB           Point to product percent table               
SPAN0890 MVC   PRDVPRD,PCTTPRD     Set product code                             
         GOTOR INIBUF              Initialize buffer record                     
         L     R1,ABUFMON                                                       
         ZAP   BUFFPAID-BUFFORDR(L'BUFFPAID,R1),PCTTPAID                        
         GOTOR PAYTRC,NUPAYTYP     Trace payment                                
         GOTOR PUTBUF              Put record to buffer                         
         CLI   PCTTPCT,PCTTPLEQ    Test last product in table                   
         JE    SPAN0900                                                         
         AHI   R3,PCTTABL          No - bump and post the next one              
         J     SPAN0890                                                         
         DROP  R3                                                               
                                                                                
SPAN0900 LLC   R0,NUPAYLEN         Bump to next element on unit                 
         AR    R2,R0                                                            
         CLI   NUPAYEL,NUPAYELQ    Test this is a pay element                   
         JE    SPAN0820            Yes - go process it                          
         DROP  R2                                                               
                                                                                
SPAN1000 LLC   R0,U.NUKDP          Bump to next daypart                         
         AHI   R0,1                                                             
         STC   R0,U.NUKDP                                                       
         J     SPAN0100            Get next unit key                            
                                                                                
SPANX    GOTOR PUTOUT              Put records for last client                  
                                                                                
         BASR  RB,0                Close output file and exit                   
         USING *,RB                                                             
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         JZ    EXIT                                                             
         DC    H'0'                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Initialize buffer record                                            *         
***********************************************************************         
                                                                                
INIBUF   STM   RE,R1,12(RD)                                                     
         MVC   BUFFMED,NETVMED                                                  
         MVC   BUFFPRD,PRDVPRD                                                  
         MVC   BUFFEST,ESTVEST                                                  
         MVC   BUFFYEAR,PSTVYEAR                                                
         LA    R0,BUFFCOLS                                                      
         LHI   R1,BUFFCOLL                                                      
         LA    RE,PZEROS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Put record to buffer                                                *         
***********************************************************************         
                                                                                
PUTBUF   NTR1  LABEL=NO                                                         
                                                                                
         LA    R0,BUFFCOLS         Test anything to post                        
         LHI   R1,BUFFCOLL                                                      
         LA    RE,PZEROS                                                        
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         JE    EXIT                                                             
                                                                                
         OC    PRDFILT,PRDFILT     Test product filter present                  
         JZ    *+14                                                             
         CLC   BUFFPRD,PRDFILT     Yes - match product code                     
         JNE   EXIT                                                             
                                                                                
         CLI   ESTFILT,0           Test estimate filter present                 
         JE    *+14                                                             
         CLC   BUFFEST,ESTFILT     Yes - match estimate                         
         JNE   EXIT                                                             
                                                                                
         GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFER),BUFFREC,ACOMFACS               
         OI    BUFFFLAG,BUFFFPUT                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Look-up sub-media for a network - on entry R1=A(Network code)       *         
***********************************************************************         
                                                                                
GETMED   NTR1  LABEL=NO                                                         
         LTR   R1,R1               Test sub-media code set                      
         JZ    GETMED02            Yes                                          
         MVC   WORK(L'NETTNET),0(R1)                                            
         LARL  R0,NETTAB           R0=A(Network table)                          
         L     RF,NETTABN          RF=Number of entries in table                
         GOTOR BINSRCH,DMCB,WORK,(R0),(RF),NETTABL,L'NETTNET,(RF)               
         CLI   0(R1),0             Test network found in table                  
         JE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,7,1(R1)          Extract sub-media code                       
         MVC   NETVMED,NETTMED-NETTABD(RF)                                      
                                                                                
GETMED02 LARL  R1,MEDTAB           Locate media table entry                     
         LHI   R0,MEDTABN                                                       
         USING MEDTABD,R1                                                       
GETMED04 CLC   MEDTMED,NETVMED     Match on sub-media code                      
         JE    GETMED06                                                         
         AHI   R1,MEDTABL                                                       
         JCT   R0,GETMED04                                                      
         DC    H'0'                                                             
GETMED06 ST    R1,AMEDTAB          Set pointer to media table entry             
         MVC   MEDVINDS,MEDTINDS   Set media indicators                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Look-up product code in table - on entry R1=A(Product number)       *         
***********************************************************************         
                                                                                
GETPRC   LARL  RF,PRDTAB                                                        
         USING PRDTABD,RF          RF=A(Product table)                          
GETPRC02 OC    PRDTPRD,PRDTPRD     Test end of table                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   PRDTNUM,0(R1)       Match product number                         
         JE    *+12                                                             
         AHI   RF,PRDTABL          Bump to next table entry                     
         J     GETPRC02                                                         
         MVC   PRDVPRD,PRDTPRD     Set product code from table                  
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Get month of service for a date - R1=A(Compressed date)             *         
***********************************************************************         
                                                                                
GETMOS   NTR1  LABEL=NO                                                         
         L     RE,AMEDTAB                                                       
         ICM   R3,7,MEDTACAL-MEDTABD(RE)                                        
         USING CALTABD,R3          R3=A(Calendar table entry)                   
         MVC   PSTVYM,CALTBYMB     Set before MOS                               
         CLC   CALTSDTC,0(R1)      Test in before period                        
         JH    EXIT                                                             
         MVC   PSTVYM,CALTAYMB     Set after MOS                                
         CLC   CALTEDTC,0(R1)      Test in after period                         
         JL    EXIT                                                             
                                                                                
         LA    R3,CALTMNTH                                                      
         USING CALTMNTH,R3         R3=A(Month entry)                            
GETMOS02 CLI   CALTMNTH,FF                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PSTVYM,CALTMBYR                                                  
         CLC   CALTMEDC,0(R1)      Test in this month                           
         JNL   EXIT                                                             
         AHI   R3,CALTMLEN                                                      
         J     GETMOS02                                                         
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Point to posting month bucket - R1=A(first month bucket)            *         
*                               - PSTVMNTH=Posting month (binary)     *         
***********************************************************************         
                                                                                
GETMON   SR    RF,RF                                                            
         ICM   RF,1,PSTVMNTH                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CHI   RF,MONYEAR          Test 13th period                             
         JNH   *+8                                                              
         LHI   RF,MONYEAR          Yes - set to 12                              
         BCTR  RF,0                                                             
         MHI   RF,L'BUFFORDR                                                    
         AR    R1,RF               R1=A(Accumulator)                            
         ST    R1,ABUFMON                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Read back buffer records and put to output file                     *         
***********************************************************************         
                                                                                
PUTOUT   NTR1  LABEL=NO                                                         
         TM    BUFFFLAG,BUFFFINI   Test buffer initialized                      
         JZ    PUTOUTX             (No - must be first time)                    
         TM    BUFFFLAG,BUFFFPUT   Test any records put                         
         JZ    EXIT                                                             
                                                                                
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFER),BUFFREC,ACOMFACS               
         CLI   BUFFERRS,0                                                       
         JE    PUTOUT04                                                         
                                                                                
PUTOUT02 GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFER),BUFFREC,ACOMFACS               
         TM    BUFFERRS,BUFFEEOF                                                
         JNZ   PUTOUTX                                                          
                                                                                
***********************************************************************         
* Initialize table pointers for buffer record key fields              *         
***********************************************************************         
                                                                                
PUTOUT04 L     R2,ADBUY            R2=A(Output record)                          
                                                                                
         MVC   NETVMED,BUFFMED                                                  
         GOTOR GETMED,0            Locate media table entry                     
                                                                                
         LARL  R1,PRDTAB           Locate product table entry                   
         LHI   R0,PRDTABM                                                       
         USING PRDTABD,R1                                                       
         BASR  RE,0                                                             
         CLC   PRDTPRD,BUFFPRD                                                  
         JE    *+12                                                             
         AHI   R1,PRDTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
         ST    R1,APRDTAB          Set A(Product table entry)                   
         DROP  R1                                                               
                                                                                
         CLC   BUFFPRD,POLPRDC     Point 'POL' product to 'UNA'                 
         JNE   *+12                                                             
         LA    R1,UNAPRD                                                        
         ST    R1,APRDTAB                                                       
                                                                                
         LLC   R1,BUFFEST          Locate estimate table entry                  
         BCTR  R1,0                                                             
         MHI   R1,ESTTABL                                                       
         LARL  R0,ESTTAB                                                        
         AR    R1,R0                                                            
         USING ESTTABD,R1                                                       
         OC    ESTTNAM,ESTTNAM     Test have estimate name                      
         JNZ   *+8                                                              
         LA    R1,ESTNOF           No - point to not on file entry              
         ST    R1,AESTTAB          Set A(Estimate table entry)                  
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Construct an output record                                          *         
***********************************************************************         
                                                                                
         LARL  R3,OUTTAB                                                        
         USING OUTTABD,R3          R3=A(Output record builder table)            
                                                                                
PUTOUT06 CLI   OUTTIBLK,OUTTIEOT   Test end of table                            
         JE    PUTOUT30                                                         
         LA    RF,SPWORKD                                                       
         CLI   OUTTIBLK,OUTTIBSD   Value in SPWORKD                             
         JE    PUTOUT08                                                         
         LA    RF,GLOBALS                                                       
         CLI   OUTTIBLK,OUTTIBGV   Value in GLOBALS                             
         JE    PUTOUT08                                                         
         LA    RF,BUFFREC                                                       
         CLI   OUTTIBLK,OUTTIBBU   Value in BUFFRECD                            
         JE    PUTOUT08                                                         
         L     RF,AMEDTAB                                                       
         CLI   OUTTIBLK,OUTTIBMT   Value in MEDTABD                             
         JE    PUTOUT08                                                         
         L     RF,APRDTAB                                                       
         CLI   OUTTIBLK,OUTTIBPT   Value in PRDTABD                             
         JE    PUTOUT08                                                         
         L     RF,AESTTAB                                                       
         CLI   OUTTIBLK,OUTTIBET   Value in ESTTABD                             
         JE    PUTOUT08                                                         
         DC    H'0'                                                             
                                                                                
PUTOUT08 SR    RE,RE                                                            
         ICM   RE,3,OUTTIDSP       RE=Displacement into block                   
         AR    RE,RF               RF=From address                              
                                                                                
         CLI   OUTTITYP,OUTTITCF   Test character field                         
         JE    PUTOUT10                                                         
         CLI   OUTTITYP,OUTTITB1   Test one byte binary                         
         JE    PUTOUT12                                                         
         CLI   OUTTITYP,OUTTITP8   Test accumulator                             
         JE    PUTOUT14                                                         
         CLI   OUTTITYP,OUTTITMC   Test posting month                           
         JNH   PUTOUT16                                                         
         DC    H'0'                                                             
                                                                                
PUTOUT10 LR    R0,R2               R0=Output address                            
         LLC   R1,OUTTIOLN         R1=Output length                             
         AR    R2,R1               Bump output pointer                          
         LLC   RF,OUTTIILN         RF=Input length                              
         ICM   RF,8,SPACES         Set pad character                            
         MVCL  R0,RE               Move data to record                          
         J     PUTOUT20                                                         
                                                                                
PUTOUT12 LLC   R1,0(RE)            Estimate number                              
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(3,R2),DUB                                                      
         AHI   R2,3                                                             
         J     PUTOUT20                                                         
                                                                                
PUTOUT14 ZAP   0(L'BUFFCOLS,R2),0(L'BUFFCOLS,RE)                                
         AHI   R2,L'BUFFCOLS                                                    
         J     PUTOUT20                                                         
                                                                                
PUTOUT16 MVC   WORK(1),0(RE)       Posting month                                
         LLC   R1,OUTTITYP                                                      
         SHI   R1,OUTTITM1-1       Convert to base month                        
         STC   R1,WORK+1                                                        
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+6)                                  
         MVC   0(L'OUTRMYM,R2),WORK+6                                           
         AHI   R2,L'OUTRMYM                                                     
                                                                                
PUTOUT20 AHI   R3,OUTTABL          Bump to next table entry                     
         J     PUTOUT06                                                         
         DROP  R3                                                               
                                                                                
PUTOUT30 XC    0(256,R2),0(R2)     Clear anything at end of record              
         L     R0,ADBUY            Put record to output file                    
         PUT   FILEOUT,(0)                                                      
         J     PUTOUT02            Get next buffer record                       
                                                                                
PUTOUTX  GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFER),0,ACOMFACS                     
         MVI   BUFFFLAG,BUFFFINI   Set buffer initialized                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Build a calendar for each media table entry                         *         
***********************************************************************         
                                                                                
BLDCAL   NTR1  LABEL=NO                                                         
                                                                                
         LARL  R2,MEDTAB                                                        
         USING MEDTABD,R2          R2=A(Media table)                            
         LHI   R5,MEDTABN          R5=Number of media table entries             
         LARL  R3,CALTAB                                                        
         USING CALTABD,R3          R3=A(Calendar table)                         
                                                                                
BLDCAL02 STCM  R3,7,MEDTACAL       Set A(Calendar) in media table               
                                                                                
         MVC   SPOTPROF+2(1),MEDTDCTY                                           
         MVC   SPOTPROF+6(3),MEDTDFMO                                           
                                                                                
         CLI   MEDTDCTY,2          Test calendar months                         
         JE    *+12                                                             
         CLI   MEDTDCTY,3                                                       
         JNE   BLDCAL10                                                         
                                                                                
***********************************************************************         
* Handle calendar periods                                             *         
***********************************************************************         
                                                                                
         MVC   CALTSDTE,STRDATE    Set start date                               
         GOTOR DATCON,DMCB,CALTSDTE,(2,CALTSDTC)                                
         GOTOR ADDAY,DMCB,(C'M',CALTSDTE),WORK,-1                               
         GOTOR DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   CALTBYMB,WORK+6     Set 'Before MOS' (binary)                    
                                                                                
         MVC   CALTEDTE,ENDDATE    Set end date                                 
         GOTOR DATCON,DMCB,CALTEDTE,(2,CALTEDTC)                                
         GOTOR ADDAY,DMCB,(C'M',CALTEDTE),WORK,1                                
         GOTOR DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   CALTAYMB,WORK+6     Set 'After MOS' (binary)                     
                                                                                
BLDCAL06 MVC   WORK+0(6),CALTSDTE  Set period start date                        
         MVC   WORK+6(6),CALTEDTE  Set period end date                          
         GOTOR MOBILE,DMCB,WORK,(MEDTDCTY,ESTLST)                               
                                                                                
         LA    R6,CALTMNTH                                                      
         USING CALTMNTH,R6         R6=A(Month entry)                            
         LA    R4,ESTLST           R4=A(MOBILE start/end date list)             
                                                                                
BLDCAL08 CLI   0(R4),FF            Test end of month list                       
         JE    BLDCAL30                                                         
         MVC   CALTMEDC,2(R4)      Set end date for month                       
         GOTOR DATCON,DMCB,(2,CALTMEDC),(3,WORK)                                
         MVC   CALTMYMB,WORK       Set year                                     
         AHI   R4,4                Bump to next input month                     
         AHI   R6,CALTMLEN         Bump to next output month                    
         J     BLDCAL08                                                         
         DROP  R6                                                               
                                                                                
***********************************************************************         
* Handle broadcast periods                                            *         
***********************************************************************         
                                                                                
BLDCAL10 GOTOR DATCON,DMCB,(0,ENDDATE),(2,ENDDATEC)                             
         GOTOR ADDAY,DMCB,(C'M',STRDATE),WORK,-11                               
         GOTOR (RF),(R1),(C'M',ENDDATE),WORK+6,3                                
         GOTOR MOBILE,DMCB,WORK,(MEDTDCTY,ESTLST)                               
                                                                                
         LA    R4,ESTLST           Locate first period of next year             
BLDCAL12 GOTOR NEWYEAR                                                          
         JE    *+12                                                             
         AHI   R4,4                                                             
         J     BLDCAL12                                                         
                                                                                
         LA    R6,CALTMNTH                                                      
         USING CALTMNTH,R6         Point to first output entry                  
         LLC   R0,2(R4)                                                         
         SRL   R0,1                R0=year                                      
         XC    CALTSDTC,CALTSDTC   Start date of first period                   
BLDCAL14 LHI   R1,1                R1=starting period number                    
BLDCAL16 CLI   0(R4),FF            Test for end of input dates                  
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R4),ENDDATEC    Test starts after end date                   
         JH    BLDCAL20                                                         
         CLC   2(2,R4),STRDATEC    Test ends before start date                  
         JL    BLDCAL18                                                         
         OC    CALTSDTC,CALTSDTC   Test start date set                          
         JNZ   *+10                                                             
         MVC   CALTSDTC,0(R4)      Save start date of first period              
         MVC   CALTMEDC,2(R4)      Set end date of this period                  
         STC   R0,CALTMBYR         Set year                                     
         STC   R1,CALTMBMO         Set period number                            
         MVC   CALTEDTC,2(R4)      Set period end date                          
         AHI   R6,CALTMLEN         Bump to next output month                    
BLDCAL18 AHI   R4,4                Bump to next input month                     
         AHI   R1,1                Bump month                                   
         GOTOR NEWYEAR             Test start of new year                       
         JNE   BLDCAL16                                                         
         AHI   R0,1                Bump year                                    
         J     BLDCAL14            and reset period                             
         DROP  R6                                                               
                                                                                
BLDCAL20 STC   R0,CALTAYYR         Set 'After MOS'                              
         STC   R1,CALTAYMO                                                      
         GOTOR DATCON,DMCB,(2,CALTSDTC),(0,CALTSDTE)                            
         GOTOR (RF),(R1),(2,CALTEDTC),(0,CALTEDTE)                              
         LLC   R0,CALTMBYR         Get start year                               
         LLC   R1,CALTMBMO         Get start period                             
         SHI   R1,1                Back-up one period                           
         JP    *+10                                                             
         BCTR  R0,0                Back-up one year if negative                 
         LHI   R1,MONYEAR          and set last period                          
         STC   R0,CALTBYYR         Set 'Before MOS'                             
         STC   R1,CALTBYMO                                                      
                                                                                
         USING CALTMNTH,R6                                                      
BLDCAL30 MVI   CALTMNTH,FF         Indicate end of calendar                     
         MVC   CALTMNTH+1(8),=C'*CALEND*'                                       
         LA    R3,9(R6)            Point to next calendar entry                 
         AHI   R2,MEDTABL          Bump to next media table entry               
         JCT   R5,BLDCAL02         Do for number of media                       
         J     EXIT                                                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* Check period starts a new year                                      *         
*                                                                     *         
* 1) A period that spans a year change and begins no further away     *         
*    from Dec/31 than it ends or                                      *         
*                                                                     *         
* 2) A period that starts before Jan/14                               *         
***********************************************************************         
                                                                                
NEWYEAR  STM   RE,R1,12(RD)                                                     
         MVC   DUB(4),0(R4)                                                     
         NI    DUB,X'01'           Strip year                                   
         CLC   DUB(2),CDJAN14                                                   
         JL    NEWYEARY                                                         
         CLC   DUB(2),CDDEC00                                                   
         JNH   NEWYEARN                                                         
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),CDDEC00                                                 
         JH    NEWYEARN                                                         
         NI    DUB+1,X'1F'         Isolate day                                  
         LLC   RF,DUB+1                                                         
         LHI   R0,30                                                            
         SR    R0,RF                                                            
         JNP   NEWYEARY            Starts on 30th or 31st                       
         STC   R0,DUB+4                                                         
         NI    DUB+3,X'1F'         Isolate day                                  
         CLC   DUB+4(1),DUB+3                                                   
         JNH   NEWYEARY                                                         
                                                                                
NEWYEARN LM    RE,R1,12(RD)        Is not a new year                            
         LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
NEWYEARY LM    RE,R1,12(RD)        Is a new year                                
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Trace unit records                                                  *         
***********************************************************************         
                                                                                
UNTTRC   CLI   QOPT2,C' '          Test tracing                                 
         BER   RE                                                               
         NTR1  LABEL=NO                                                         
         L     R2,ADBUY                                                         
         USING NURECD,R2                                                        
         MVC   TLINE,SPACES                                                     
         MVC   TLCLT,CLTVCLT                                                    
         GOTOR DATCON,DMCB,(2,NUKDATE),(8,TLDATE)                               
         LLC   R0,NUKTIME                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TLTIME,DUB                                                       
         MVC   TLNET,NUKNET                                                     
         MVC   TLMED,NETVMED                                                    
         MVC   TLPROG,NUKPROG                                                   
         LLC   R0,NUKEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TLEST,DUB                                                        
         LLC   R0,NUKSUB                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TLSUB,DUB                                                        
         MVC   TLDP,NUKDP                                                       
         GOTOR HEXOUT,DMCB,U.NUDA,TLDA,L'NUDA,0                                 
         MVC   TLSRTC,UNTVSRTC                                                  
         EDIT  (B4,NUASSIGN),(12,TLASSIGN),2,MINUS=YES                          
         EDIT  (B4,NUACTUAL),(12,TLACTUAL),2,MINUS=YES                          
         EDIT  (B4,NUINTEG),(12,TLINTEG),2,MINUS=YES                            
         GOTOR REPORT                                                           
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Trace ordered and assigned cost                                     *         
***********************************************************************         
                                                                                
OASTRC   CLI   QOPT2,C' '          Test tracing                                 
         BER   RE                                                               
         NTR1  LABEL=NO                                                         
         USING PCTTABD,R3                                                       
         MVC   OLINE,SPACES                                                     
         MVC   OLLABEL,=C'ORDRASSN'                                             
         MVC   OLPRD,PCTTPRD                                                    
         CLI   PCTTPCT,PCTTPLEQ                                                 
         JE    OASTRC02                                                         
         EDIT  (B2,PCTTPCT),(6,OLPCT),2                                         
OASTRC02 GOTOR TRCMON,OLMONTH                                                   
         EDIT  (P8,PCTTORDR),(12,OLORDR),2,MINUS=YES                            
         EDIT  (P8,PCTTASSN),(12,OLASSN),2,MINUS=YES                            
         GOTOR REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Trace payment                                                       *         
***********************************************************************         
                                                                                
PAYTRC   CLI   QOPT2,C' '          Test tracing                                 
         BER   RE                                                               
         NTR1  LABEL=NO                                                         
         MVC   PLINE,SPACES                                                     
         MVC   PLLABEL,=C'PAID    '                                             
         MVC   PLPRD,BUFFPRD                                                    
         MVC   PLTYPE,0(R1)                                                     
         GOTOR TRCMON,PLMONTH                                                   
         L     R1,ABUFMON                                                       
         ZAP   DUB,BUFFPAID-BUFFORDR(L'BUFFPAID,R1)                             
         CVB   R0,DUB                                                           
         EDIT  (R0),(12,PLPAID),2,MINUS=YES                                     
         GOTOR REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Integration trace (posted as ordered)                               *         
***********************************************************************         
                                                                                
INTTRC   CLI   QOPT2,C' '          Test tracing                                 
         BER   RE                                                               
         NTR1  LABEL=NO                                                         
         MVC   ILINE,SPACES                                                     
         MVC   ILLABEL,=C'INTG    '                                             
         MVC   ILPRD,BUFFPRD                                                    
         GOTOR TRCMON,ILMONTH                                                   
         L     R1,ABUFMON                                                       
         ZAP   DUB,BUFFORDR-BUFFORDR(L'BUFFORDR,R1)                             
         CVB   R0,DUB                                                           
         EDIT  (R0),(12,ILORDR),2,MINUS=YES                                     
         GOTOR REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Special rate trace                                                  *         
***********************************************************************         
                                                                                
SPCTRC   CLI   QOPT2,C' '          Test tracing                                 
         BER   RE                                                               
         NTR1  LABEL=NO                                                         
         MVC   SLINE,SPACES                                                     
         MVC   SLLABEL,=C'SPECIAL '                                             
         MVC   SLPRD,BUFFPRD                                                    
         MVC   SLRTYP,UNTVRTYP                                                  
                                                                                
         CLC   UNTVRTYP,NUSPRACC                                                
         JNE   *+10                                                             
         MVC   SLLABEL,=C'ORDRCRED'                                             
         CLC   UNTVRTYP,NUSPRAGC                                                
         JNE   *+10                                                             
         MVC   SLLABEL,=C'ASSNCRED'                                             
                                                                                
         CLC   UNTVRTYP,NUSPRAMI                                                
         JNE   *+10                                                             
         MVC   SLLABEL,=C'MIDAS CR'                                             
                                                                                
         GOTOR TRCMON,SLMONTH                                                   
         L     R1,ABUFMON                                                       
         ZAP   DUB,BUFFORDR-BUFFORDR(L'BUFFORDR,R1)                             
         CVB   R0,DUB                                                           
         EDIT  (R0),(12,SLORDR),2,MINUS=YES                                     
         L     R1,ABUFMON                                                       
         ZAP   DUB,BUFFASSN-BUFFORDR(L'BUFFASSN,R1)                             
         CVB   R0,DUB                                                           
         EDIT  (R0),(12,SLASSN),2,MINUS=YES                                     
         GOTOR REPORT                                                           
         J     EXIT                                                             
                                                                                
TRCMON   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         MVC   WORK(2),PSTVYEAR                                                 
         MVI   WORK+2,1                                                         
         GOTOR DATCON,DMCB,(3,WORK),(0,WORK+6)                                  
         MVC   0(4,R2),WORK+6                                                   
         J     EXIT                                                             
         EJECT                                                                  
EXIT     XIT1  ,                                                                
                                                                                
NUKSTRAQ EQU   X'C1'               First traffic unit                           
NUMAIELQ EQU   X'01'               Unit record main element                     
NUOTELQ  EQU   X'60'               Others element                               
STABELQ  EQU   X'0E'               Bill element                                 
STABPTYQ EQU   X'0E'               Station billing record type                  
STABPSTQ EQU   X'81'               Station billing sub-record type              
                                                                                
MONYEAR  EQU   12                  Number of months in a year                   
ONEK     EQU   1024                1K                                           
FOURK    EQU   4*ONEK              4K                                           
FF       EQU   X'FF'                                                            
                                                                                
GLOBALS  DS    0D                  ** Global values **                          
         LTORG                                                                  
                                                                                
BUFFERIN DC    V(BUFFERIN)                                                      
NETNET   DC    V(NETNET)                                                        
*                                                                               
NETTABN  DS    F                   Number of NETTAB entries                     
AMEDTAB  DS    A                   A(Current MEDTAB entry)                      
APRDTAB  DS    A                   A(Current PRDTAB entry)                      
AESTTAB  DS    A                   A(Current ESTTAB entry)                      
ABUFMON  DS    A                   A(Month bucket for unit)                     
                                                                                
CLTFILT  DC    XL(L'NUKCLT)'00'    Client filter                                
PRDFILT  DC    XL(L'PKEYPRD)'00'   Product FILter                               
ESTFILT  DC    XL(L'NUKEST)'00'    Estimate filter                              
NETFILT  DC    XL(L'NUKNET)'00'    Network filter                               
                                                                                
BUFFFLAG DC    X'00'               ** Buffer flag **                            
BUFFFINI EQU   X'80'               Buffer initialized                           
BUFFFPUT EQU   X'40'               Record put to buffer                         
                                                                                
STRDATE  DS    CL6                 Request start date (EBCDIC)                  
ENDDATE  DS    CL6                 Request end date (EBCDIC)                    
STRDATEC DS    XL2                 Request start date (compressed)              
ENDDATEC DS    XL2                 Request end date (compressed)                
                                                                                
CDJAN14  DC    X'002E'             JAN14                                        
CDDEC00  DC    X'0180'             DEC00                                        
                                                                                
NUSPRACC DC    C'C'                Barter actual credit element                 
NUSPRAGC DC    C'F'                Barter assign credit element                 
NUSPRAMI DC    C'M'                Barter midas credit element                  
                                                                                
DMOPEN   DC    C'OPEN    '                                                      
SPOTSYS  DC    C'SPOT   '                                                       
                                                                                
UNTLIST  DS    0C                  ** Network file open list **                 
         DC    C'N'                                                             
UNTDIR   DC    C'UNTDIR '                                                       
         DC    C'N'                                                             
UNTFIL   DC    C'UNTFIL '                                                       
UNTLISTX DC    C'X'                                                             
                                                                                
XSPDIR   DC    C'XSPDIR '                                                       
XSPFIL   DC    C'XSPFIL '                                                       
                                                                                
XKEY     DS    XL64                XSPDIR key                                   
XKEYSAVE DS    XL64                XSPDIR key save                              
*                                                                               
       ++INCLUDE SPBVALD                                                        
                                                                                
UNTSTRDT DC    X'0000'             Start date for units                         
UNTENDDT DC    X'FFFF'             End date for units                           
                                                                                
PROFVALS DS    XL16                GETPROF profile values                       
PROFINFO DS    XL(PROFINFL)        GETPROF record key                           
                                                                                
LCLT     DC    XL(L'NUKCLT)'00'    Last client code                             
                                                                                
UNTKEY   DC    XL32'00'            Unit directory record                        
                                                                                
FILEHDR  DS    0X                  ** Output file header record **              
FILEID   DC    C'NA9HDR'           File id                                      
FILESYS  DC    C'NET '                                                          
FILEDATE DC    C'YYMMDD'           FILE CREATION DATE                           
FILEQSTR DC    C'YYMMDD'           REQUEST START DATE                           
FILEQEND DC    C'YYMMDD'           REQUEST END DATE                             
FILEDOLS DS    C                   G=gross, N=net (set from QOPT1)              
FILEHDRL EQU   *-FILEHDR           Length of header record                      
                                                                                
POLPRDC  DC    C'POL'              POL product code                             
POLPRDN  DC    AL1(FF)             POL product number                           
                                                                                
UNAPRD   DS    0X                  ** UNA product table entry **                
         DC    CL(L'PRDTPRD)'UNA'                                               
         DC    CL(L'PRDTNAM)'UNALLOCATED'                                       
                                                                                
ESTNOF   DS    0X                  ** Missing estimate entry **                 
         DC    CL(L'ESTTNAM)'ESTIMATE NOT ON FILE'                              
         DC    CL(L'ESTTTYP)'?'                                                 
                                                                                
MEDVINDS DS    XL(L'MEDTINDS)      Sub-media indicator                          
                                                                                
CLTVALS  DS    0X                  ** Client values **                          
CLTVCLT  DS    CL3                 Client code                                  
CLTVNAM  DS    CL(L'CNAME)         Client name                                  
CLTVACOF DS    CL(L'CACCOFC)       Client accounting office code                
CLTVOFFC DS    CL(L'COFFICE)       Client office                                
CLTVMDOF DS    CL(L'OFCOFC2)       Client media office code                     
CLTVCACS DS    CL(L'CACCESS)       Client access code                           
                                                                                
CLTVFLAG DS    X                   ** Client flag byte **                       
CLTVFIAC EQU   X'80'               Integration included in assigned             
CLTVFSAC EQU   X'40'               Specials included in assigned                
CLTVFMID EQU   X'20'               Midas client                                 
CLTVALL  EQU   *-CLTVALS                                                        
                                                                                
PRDVPRD  DS    CL(L'PKEYPRD)       Product code                                 
                                                                                
ESTVEST  DS    XL(L'EKEYEST)       Estimate number                              
                                                                                
NETVMED  DS    CL(L'STYPE)         Posting media type                           
                                                                                
UNTVALS  DS    0F                  ** Unit values **                            
UNTVAX01 DS    A                   A(NUMAINEL)                                  
UNTVAX02 DS    A                   A(NUSDRD)                                    
UNTVAX03 DS    A                   A(First NUSPRD)                              
UNTVAX12 DS    A                   A(First NUPAYD)                              
UNTVAX14 DS    A                   A(NUPRDD)                                    
UNTVAX19 DS    A                   A(NUPDED)                                    
UNTVAXAC DS    A                   A(Barter actual credit)                      
UNTVAXAG DS    A                   A(Barter assign credit)                      
                                                                                
UNTVIPRD DS    CL(L'EKEYPRD)       Integration product                          
UNTVDATE DS    XL(L'NUKDATE)       Unit date                                    
                                                                                
UNTVSTAT DS    XL(L'NUUNITST)      ** Unit status **                            
UNTVSMUQ EQU   X'80'               Minus unit                                   
UNTVSPEQ EQU   X'40'               Pre-empted unit                              
UNTVSACQ EQU   X'08'               Assigned cost input                          
UNTVSMIQ EQU   X'02'               Missed unit                                  
                                                                                
UNTVSTA3 DS    XL(L'NUSDST3)       Unit status 3                                
UNTVSAOQ EQU   X'80'               Assigned cost manually overridden            
                                                                                
UNTVRTCV DS    CL(L'NUSDRTCV)      Rate coverage                                
UNTVSRTC DS    CL(L'NUSDSRT)       Special rate type                            
                                                                                
UNTVPKST DS    XL(L'NUPACKST)      ** Package status **                         
UNTVPLKQ EQU   X'20'               Locked package                               
UNTVPINQ EQU   X'04'               Integration non-commissionable               
UNTVPNSQ EQU   X'01'               No-show package                              
                                                                                
UNTVPPCT DS    XL(L'NUP1SHR)       Product 1 share                              
UNTVRTYP DS    CL(L'NUSPRTYP)      Special rate type                            
UNTVALL  EQU   *-UNTVALS                                                        
                                                                                
UNTVPAID DS    0PL8                Paid amount                                  
UNTVORDR DS    PL8                 Ordered cost                                 
UNTVOGRS DS    PL8                 Ordered cost (gross)                         
UNTVONET DS    PL8                 Ordered cost (net)                           
UNTVOUNA DS    PL8                 Ordered cost unallocated                     
                                                                                
UNTVINTG DS    PL8                 Integration cost                             
UNTVIGRS DS    PL8                 Integration cost (gross)                     
UNTVINET DS    PL8                 Integration cost (net)                       
                                                                                
UNTVPUNA DS    0PL8                Paid amount unallocated                      
UNTVASSN DS    PL8                 Assigned cost                                
UNTVAGRS DS    PL8                 Assigned cost (gross)                        
UNTVANET DS    PL8                 Assigned cost (net)                          
UNTVAUNA DS    PL8                 Assigned cost unallocated                    
                                                                                
PSTVYM   DS    0AL2                ** Posting year/month **                     
PSTVYEAR DS    AL1                 Posting year                                 
PSTVMNTH DS    AL1                 Posting month                                
                                                                                
         DS    0D                  ** Buffer record **                          
         DC    C'*BUFREC*'                                                      
BUFFREC  DS    XL(BUFFRECL)                                                     
                                                                                
         DS    0D                  ** Output record **                          
         DC    C'*OUTREC*'                                                      
OUTREC   DS    XL(OUTRECL)                                                      
                                                                                
PZEROS   DC    (BUFFCOLN)PL8'0'    For clearing/comparing BUFFCOLS              
                                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,BLKSIZE=OUTRECL*10,    +        
               MACRF=PM,LRECL=OUTRECL                                           
                                                                                
BUFFER   BUFFD TYPE=P,KEYLEN=BUFFKEYL,COMLEN=0,COLUMNS=BUFFCOLN,       +        
               FILE=BUFFWK,BUFFERS=50                                           
                                                                                
         DS    0H                  ** Media table **                            
         DC    C'*MEDTAB*'                                                      
MEDTAB   DS    0XL(MEDTABL)                                                     
                                                                                
         DC    CL(L'MEDTMED)'N',AL1(MEDTICAL)                                   
         DC    CL(L'MEDTNAM)'NETWORK'                                           
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
         DC    CL(L'MEDTMED)'C',AL1(MEDTIBRD)                                   
         DC    CL(L'MEDTNAM)'CABLE'                                             
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
         DC    CL(L'MEDTMED)'S',AL1(MEDTIBRD)                                   
         DC    CL(L'MEDTNAM)'SYNDICATION'                                       
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
         DC    CL(L'MEDTMED)'D',AL1(MEDTIBRD)                                   
         DC    CL(L'MEDTNAM)'RADIO'                                             
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
         DC    CL(L'MEDTMED)'H',AL1(MEDTIBRD)                                   
         DC    CL(L'MEDTNAM)'HISPANIC'                                          
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
         DC    CL(L'MEDTMED)'O',AL1(MEDTIBRD)                                   
         DC    CL(L'MEDTNAM)'OTHER'                                             
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
         DC    CL(L'MEDTMED)'V',AL1(MEDTIBRD)                                   
         DC    CL(L'MEDTNAM)'VOD'        VIDEO ON DEMAND                        
         DC    CL(L'MEDTACAS)' '                                                
         DC    XL(MEDTDLEN)'00'                                                 
         DC    AL(L'MEDTACAL)(0)                                                
                                                                                
MEDTABN  EQU   (*-MEDTAB)/MEDTABL                                               
                                                                                
         DS    0H                  ** Product percents table **                 
         DC    C'*PCTTAB*'                                                      
PCTTAB   DC    (PCTTABM)XL(PCTTABL)'00'                                         
                                                                                
         DS    0H                  ** Network table **                          
         DC    C'*NETTAB*'                                                      
NETTAB   DC    (NETTABM)XL(NETTABL)'00'                                         
                                                                                
         DS    0H                  ** Product table **                          
         DC    C'*PRDTAB*'                                                      
PRDTAB   DC    (PRDTABM)XL(PRDTABL)'00'                                         
                                                                                
         DS    0H                  ** Estimate table **                         
         DC    C'*ESTTAB*'                                                      
ESTTAB   DC    (ESTTABM)XL(ESTTABL)'00'                                         
ESTTABLN EQU   *-ESTTAB                                                         
                                                                                
         DS    0H                  ** Calendar table **                         
         DC    C'*CALTAB*'                                                      
CALTAB   DS    XL(4*ONEK)                                                       
                                                                                
OUTTAB   DS    0H                  ** Output record builder table **            
                                                                                
         DC    AL1(OUTTIBSD,OUTTITCF)                                           
         DC    AL2(AGY-SPWORKD)                                                 
         DC    AL1(L'AGY,L'OUTRAGY)                                             
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFMED-BUFFRECD)                                            
         DC    AL1(L'BUFFMED,L'OUTRMED)                                         
                                                                                
         DC    AL1(OUTTIBSD,OUTTITCF)                                           
         DC    AL2(BAGYMD-SPWORKD)                                              
         DC    AL1(L'BAGYMD,L'OUTRAGMD)                                         
                                                                                
         DC    AL1(OUTTIBGV,OUTTITCF)                                           
         DC    AL2(CLTVCLT-GLOBALS)                                             
         DC    AL1(L'CLTVCLT,L'OUTRCLT)                                         
                                                                                
         DC    AL1(OUTTIBPT,OUTTITCF)                                           
         DC    AL2(PRDTPRD-PRDTABD)                                             
         DC    AL1(L'PRDTPRD,L'OUTRPRD)                                         
                                                                                
         DC    AL1(OUTTIBBU,OUTTITB1)                                           
         DC    AL2(BUFFEST-BUFFRECD)                                            
         DC    AL1(L'BUFFEST,L'OUTREST)                                         
                                                                                
         DC    AL1(OUTTIBET,OUTTITCF)                                           
         DC    AL2(ESTTTYP-ESTTABD)                                             
         DC    AL1(L'ESTTTYP,L'OUTRTYPE)                                        
                                                                                
         DC    AL1(OUTTIBGV,OUTTITCF)                                           
         DC    AL2(CLTVMDOF-GLOBALS)                                            
         DC    AL1(L'CLTVMDOF,L'OUTRMDOF)                                       
                                                                                
         DC    AL1(OUTTIBGV,OUTTITCF)                                           
         DC    AL2(CLTVACOF-GLOBALS)                                            
         DC    AL1(L'CLTVACOF,L'OUTRACOF)                                       
                                                                                
         DC    AL1(OUTTIBGV,OUTTITCF)                                           
         DC    AL2(CLTVCACS-GLOBALS)                                            
         DC    AL1(L'CLTVCACS,L'OUTRCACS)                                       
                                                                                
         DC    AL1(OUTTIBMT,OUTTITCF)                                           
         DC    AL2(MEDTACAS-MEDTABD)                                            
         DC    AL1(L'MEDTACAS,L'OUTRACAS)                                       
                                                                                
         DC    AL1(OUTTIBSD,OUTTITCF)                                           
         DC    AL2(SPACES-SPWORKD)                                              
         DC    AL1(L'OUTRFILL,L'OUTRFILL)                                       
                                                                                
         DC    AL1(OUTTIBMT,OUTTITCF)                                           
         DC    AL2(MEDTNAM-MEDTABD)                                             
         DC    AL1(L'MEDTNAM,L'OUTRMDNM)                                        
                                                                                
         DC    AL1(OUTTIBGV,OUTTITCF)                                           
         DC    AL2(CLTVNAM-GLOBALS)                                             
         DC    AL1(L'CLTVNAM,L'OUTRCLNM)                                        
                                                                                
         DC    AL1(OUTTIBPT,OUTTITCF)                                           
         DC    AL2(PRDTNAM-PRDTABD)                                             
         DC    AL1(L'PRDTNAM,L'OUTRPRNM)                                        
                                                                                
         DC    AL1(OUTTIBET,OUTTITCF)                                           
         DC    AL2(ESTTNAM-ESTTABD)                                             
         DC    AL1(L'ESTTNAM,L'OUTRESNM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM1)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*00))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*00))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*00))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*00))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM2)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*01))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*01))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*01))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*01))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM3)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*02))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*02))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*02))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*02))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM4)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*03))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*03))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*03))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*03))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM5)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*04))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*04))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*04))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*04))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM6)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*05))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*05))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*05))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*05))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM7)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*06))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*06))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*06))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*06))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM8)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*07))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*07))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*07))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*07))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITM9)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*08))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*08))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*08))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*08))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITMA)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*09))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*09))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*09))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*09))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITMB)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*10))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*10))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*10))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*10))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITMC)                                           
         DC    AL2(BUFFYEAR-BUFFRECD)                                           
         DC    AL1(L'BUFFYEAR,L'OUTRMYM)                                        
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFORDR-BUFFRECD+(L'BUFFORDR*11))                           
         DC    AL1(L'BUFFORDR,L'OUTRMOAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFPAID-BUFFRECD+(L'BUFFPAID*11))                           
         DC    AL1(L'BUFFPAID,L'OUTRMPAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFBILL-BUFFRECD+(L'BUFFBILL*11))                           
         DC    AL1(L'BUFFBILL,L'OUTRMBAM)                                       
                                                                                
         DC    AL1(OUTTIBBU,OUTTITCF)                                           
         DC    AL2(BUFFASSN-BUFFRECD+(L'BUFFASSN*11))                           
         DC    AL1(L'BUFFASSN,L'OUTRMAAM)                                       
                                                                                
OUTTABX  DC    AL1(OUTTIEOT)       End of output builder table                  
                                                                                
NETTABD  DSECT                     ** Network table **                          
NETTNET  DS    CL4                 Network code                                 
NETTMED  DS    CL(L'STYPE)         Network sub-media                            
NETTABL  EQU   *-NETTABD           Length of table entry                        
NETTABM  EQU   2048                Maximum number of networks supported         
                                                                                
MEDTABD  DSECT                     ** Media table **                            
MEDTMED  DS    CL(L'STYPE)         Media code                                   
                                                                                
MEDTINDS DS    X                   ** Media indicators **                       
MEDTICAL EQU   X'80'               Post by calendar month                       
MEDTIBRD EQU   X'40'               Post by broadcast month                      
                                                                                
MEDTNAM  DS    CL(L'OUTRMDNM)      Media name                                   
                                                                                
MEDTACAS DS    X                   ** Billable actual or assigned **            
MEDTAACT EQU   1                   Use actual cost for billable                 
MEDTAASS EQU   2                   Use assigned cost for billable               
                                                                                
MEDTDCON DS    0XL(MEDTDLEN)       ** Calendar information **                   
MEDTDCTY DS    X                   Calendar type                                
MEDTDFMO DS    X                   Fiscal base start month                      
MEDTDFDT DS    X                   Fiscal base date number                      
MEDTDFDY DS    X                   Fiscal base day number                       
MEDTDLEN EQU   *-MEDTDCTY                                                       
                                                                                
MEDTACAL DS    AL3                 A(Calendar) (see CALTABD)                    
                                                                                
MEDTABL  EQU   *-MEDTABD           Length of table entry                        
                                                                                
CALTABD  DSECT                     ** Calendar table **                         
                                                                                
CALTSDTE DS    CL6                 Start date (EBCDIC)                          
CALTSDTC DS    XL2                 Start date (compressed)                      
                                                                                
CALTBYMB DS    0XL2                ** Before MOS (binary) **                    
CALTBYYR DS    X                   Year                                         
CALTBYMO DS    X                   Period number (1-13)                         
                                                                                
CALTEDTE DS    CL6                 End date (EBDCID)                            
CALTEDTC DS    XL2                 End date (compressed)                        
                                                                                
CALTAYMB DS    0XL2                ** After MOS (binary) **                     
CALTAYYR DS    X                   Year                                         
CALTAYMO DS    X                   Period number (1-13)                         
                                                                                
CALTMNTH DS    0X                  ** Period entry **                           
CALTMEDC DS    XL2                 Period end date (compressed)                 
                                                                                
CALTMYMB DS    0XL2                ** Period MOS (binary) **                    
CALTMBYR DS    X                   Year                                         
CALTMBMO DS    X                   Period number (1-13)                         
                                                                                
CALTMLEN EQU   *-CALTMNTH                                                       
                                                                                
PCTTABD  DSECT                     ** Product percent table **                  
PCTTPRD  DS    CL(L'NUPDEPR)       Product code                                 
PCTTPLEQ EQU   FF                  Last entry (remainder)                       
PCTTPCT  DS    XL(L'NUPDEPCT)      Product percentage                           
PCTTPAID DS    0PL8                Paid amount                                  
PCTTORDR DS    PL8                 Ordered amount                               
PCTTASSN DS    PL8                 Assigned amount                              
PCTTABL  EQU   *-PCTTABD           Length of table entry                        
PCTTABM  EQU   16                  Maximum number of products supported         
                                                                                
PRDTABD  DSECT                     ** Product table **                          
PRDTPRD  DS    CL(L'PKEYPRD)       Product code                                 
PRDTNAM  DS    CL(L'PNAME)         Product name                                 
PRDTNUM  DS    X                   Product number                               
PRDTABL  EQU   *-PRDTABD           Length of table entry                        
PRDTABM  EQU   1024                Maximum number of products supported         
                                                                                
ESTTABD  DSECT                     ** Estimate table **                         
ESTTNAM  DS    CL(L'EDESC)         Estimate description                         
ESTTTYP  DS    CL(L'ETYPE)         Estimate type                                
ESTTABL  EQU   *-ESTTABD           Length of table entry                        
ESTTABM  EQU   255                 Maximum number of estimates                  
                                                                                
BUFFRECD DSECT                     ** Buffer record **                          
                                                                                
BUFFKEY  DS    0X                  ** Buffer record key **                      
BUFFMED  DS    CL(L'NETTMED)       Sub-media code                               
BUFFPRD  DS    CL(L'PKEYPRD)       Product code                                 
BUFFEST  DS    XL(L'EKEYEST)       Estimate number                              
BUFFYEAR DS    AL1                 Year                                         
BUFFKEYL EQU   *-BUFFRECD          Length of buffer key                         
                                                                                
BUFFCOLS DS    0PL8                ** Buffer record accumulators **             
                                                                                
BUFFORDR DS    (MONYEAR)PL8        Ordered amounts                              
BUFFPAID DS    (MONYEAR)PL8        Paid amounts                                 
BUFFBILL DS    (MONYEAR)PL8        Billed amounts                               
BUFFASSN DS    (MONYEAR)PL8        Assigned amounts                             
                                                                                
BUFFCOLL EQU   *-BUFFCOLS                                                       
BUFFCOLN EQU   (*-BUFFCOLS)/L'BUFFCOLS                                          
                                                                                
BUFFRECL EQU   *-BUFFRECD          Length of buffer record                      
                                                                                
OUTTABD  DSECT                     ** Output record builder table **            
                                                                                
OUTTIBLK DS    AL1                 ** Input block **                            
OUTTIEOT EQU   0                   End of table indicator                       
OUTTIBSD EQU   1                   Value in SPWORKD                             
OUTTIBGV EQU   2                   Value in GLOBALS                             
OUTTIBBU EQU   3                   Value in BUFFRECD                            
OUTTIBMT EQU   4                   Value in MEDTABD                             
OUTTIBPT EQU   5                   Value in PRDTABD                             
OUTTIBET EQU   6                   Value in ESTTABD                             
                                                                                
OUTTITYP DS    AL1                 ** Data type **                              
OUTTITCF EQU   1                   Character field                              
OUTTITB1 EQU   2                   Binary length 1                              
OUTTITP8 EQU   3                   Packed accumulator                           
OUTTITM1 EQU   4                   Posting month 1                              
OUTTITM2 EQU   5                   Posting month 2                              
OUTTITM3 EQU   6                   Posting month 3                              
OUTTITM4 EQU   7                   Posting month 4                              
OUTTITM5 EQU   8                   Posting month 5                              
OUTTITM6 EQU   9                   Posting month 6                              
OUTTITM7 EQU   10                  Posting month 7                              
OUTTITM8 EQU   11                  Posting month 8                              
OUTTITM9 EQU   12                  Posting month 9                              
OUTTITMA EQU   13                  Posting month 10                             
OUTTITMB EQU   14                  Posting month 11                             
OUTTITMC EQU   15                  Posting month 12                             
                                                                                
OUTTIDSP DS    AL2                 Displacement to input value                  
                                                                                
OUTTIILN DS    AL1                 Input data length                            
OUTTIOLN DS    AL1                 Output data length                           
                                                                                
OUTTABL  EQU   *-OUTTABD           Length of table entry                        
                                                                                
OUTRECD  DSECT                     ** Output record **                          
                                                                                
OUTRAGY  DS    CL(L'AGY)           Agency code                                  
OUTRMED  DS    CL(L'MEDTMED)       Media letter                                 
OUTRAGMD DS    XL(L'BAGYMD)        Agency/media                                 
OUTRCLT  DS    CL(L'CLTVCLT)       Client code                                  
OUTRPRD  DS    CL(L'PRDTPRD)       Product code                                 
OUTREST  DS    CL3                 Estimate number                              
OUTRTYPE DS    CL(L'ESTTTYP)       Estimate type                                
OUTRMDOF DS    CL(L'CLTVMDOF)      Client media office code                     
OUTRACOF DS    CL(L'CLTVACOF)      Client accounting office                     
OUTRCACS DS    CL(L'CLTVCACS)      Client access code                           
OUTRACAS DS    XL(L'MEDTACAS)      Bill actual or assigned cost                 
                                                                                
OUTRFILL DS    XL15                Undefined                                    
                                                                                
OUTRMDNM DS    CL24                Media name                                   
OUTRCLNM DS    CL24                Client name                                  
OUTRPRNM DS    CL24                Product name                                 
OUTRESNM DS    CL24                Estimate name                                
                                                                                
OUTRMTH  DS    0X                  ** Month values **                           
OUTRMYM  DS    CL4                 Year/month                                   
OUTRMOAM DS    PL(L'BUFFORDR)      Ordered amount                               
OUTRMPAM DS    PL(L'BUFFPAID)      Paid amount                                  
OUTRMBAM DS    PL(L'BUFFBILL)      Billed amount                                
OUTRMAAM DS    PL(L'BUFFASSN)      Assigned amount                              
OUTRMTHL EQU   *-OUTRMTH                                                        
         DS    (MONYEAR-1)XL(OUTRMTHL)                                          
                                                                                
OUTRECL  EQU   *-OUTRECD           Length of output record                      
                                                                                
* Other included books follow                                                   
****     PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
                                                                                
         ORG   P                                                                
TLINE    DS    0CL(L'P)            ** Unit trace line **                        
TLCLT    DS    CL3                                                              
         DS    C                                                                
TLDATE   DS    CL8                                                              
         DS    C                                                                
TLTIME   DS    CL3                                                              
         DS    C                                                                
TLNET    DS    CL(L'NUKNET)                                                     
         DS    C                                                                
TLMED    DS    CL(L'NETVMED)                                                    
         DS    C                                                                
TLPROG   DS    CL(L'NUKPROG)                                                    
         DS    C                                                                
TLEST    DS    CL3                                                              
         DS    C                                                                
TLSUB    DS    CL3                                                              
         DS    C                                                                
TLDP     DS    CL(L'NUKDP)                                                      
         DS    C                                                                
TLDA     DS    CL8                                                              
         DS    C                                                                
TLSRTC   DS    CL(L'UNTVSRTC)                                                   
         DS    C                                                                
TLACTUAL DS    CL12                                                             
         DS    C                                                                
TLASSIGN DS    CL12                                                             
         DS    C                                                                
TLINTEG  DS    CL12                                                             
                                                                                
         ORG   P                                                                
OLINE    DS    0CL(L'P)            ** Ordered/Assigned trace line **            
         DS    CL4                                                              
OLLABEL  DS    CL8                                                              
         DS    C                                                                
OLPRD    DS    CL(L'PCTTPRD)                                                    
         DS    C                                                                
OLPCT    DS    CL6                                                              
         DS    C                                                                
OLMONTH  DS    CL4                                                              
         DS    C                                                                
OLORDR   DS    CL12                                                             
         DS    C                                                                
OLASSN   DS    CL12                                                             
                                                                                
         ORG   P                                                                
PLINE    DS    0CL(L'P)            ** Pay trace line **                         
         DS    CL4                                                              
PLLABEL  DS    CL8                                                              
         DS    C                                                                
PLPRD    DS    CL(L'PCTTPRD)                                                    
         DS    C                                                                
PLTYPE   DS    C                                                                
         DS    CL6                                                              
PLMONTH  DS    CL4                                                              
         DS    C                                                                
PLPAID   DS    CL12                                                             
                                                                                
         ORG   P                                                                
ILINE    DS    0CL(L'P)            ** Integration trace line **                 
         DS    CL4                                                              
ILLABEL  DS    CL8                                                              
         DS    C                                                                
ILPRD    DS    CL(L'PCTTPRD)                                                    
         DS    CL8                                                              
ILMONTH  DS    CL4                                                              
         DS    C                                                                
ILORDR   DS    CL12                                                             
                                                                                
         ORG   P                                                                
SLINE    DS    0CL(L'P)            ** Special rate trace line **                
         DS    CL4                                                              
SLLABEL  DS    CL8                                                              
         DS    C                                                                
SLPRD    DS    CL(L'PCTTPRD)                                                    
         DS    C                                                                
SLRTYP   DS    CL(L'UNTVRTYP)                                                   
         DS    CL6                                                              
SLMONTH  DS    CL4                                                              
         DS    C                                                                
SLORDR   DS    CL12                                                             
         DS    C                                                                
SLASSN   DS    CL12                                                             
         ORG                                                                    
                                                                                
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
         ORG   EKEYEST+L'EKEYEST                                                
EKEYREST DS    XL(EKCNTRL-EKEYEST)                                              
         ORG                                                                    
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE NEGENUNIT                                                      
NUPDED   DSECT                                                                  
NUPDEDL  EQU   *-NUPDED                                                         
NUPRDD   DSECT                                                                  
NUPRPLEN EQU   *-NUPRDPR                                                        
NUPRDDL  EQU   *-NUPRDD                                                         
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDGETPROFD                                                     
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPAN02 08/05/14'                                      
         END                                                                    
