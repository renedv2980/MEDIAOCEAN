*          DATA SET CTAGYDEL   AT LEVEL 006 AS OF 01/07/21                      
*PHASE CAGYDELA                                                                 
*INCLUDE BUFFERIN                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
AGYDEL   TITLE '- CONTROL SYSTEM AGENCY DELETER'                                
AGYDEL   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,**AGYDEL,WORK=AWORKC,CLEAR=YES                             
         USING WORKD,RC                                                         
         BASR  RA,0                                                             
         AHI   RA,LITERALS-*                                                    
         USING LITERALS,RA                                                      
         USING SSBD,SSB                                                         
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVI   CHRULT,X'BF'                                                     
                                                                                
         GOTOR VALPAR              VALIDATE INPUT PARAMETERS                    
                                                                                
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'TITLE2),TITLE2                                           
         ZAP   LINE,PNINETY                                                     
         ZAP   PAGE,PONE                                                        
                                                                                
         GOTOR VDMGR,DMCB,DMOPEN,DMCONSYS,DMFLIST,IO                            
                                                                                
         GOTOR GET                 READ RECORDS AND POST TO BUFFER              
                                                                                
         GOTOR PUT                 WRITE BACK ALL DELETED RECORDS               
                                                                                
         GOTO1 VDMGR,DMCB,DMCLSE,DMCONSYS,0,IO                                  
                                                                                
         CLI   OUTPUT,C'D'                                                      
         BE    AGYDELX                                                          
         CLOSE (CONTAPE,,GENTAPE)                                               
                                                                                
AGYDELX  XBASE ,                                                                
         DROP  RB                                                               
                                                                                
AWORKC   DC    A(WORKC)                                                         
         EJECT                                                                  
***********************************************************************         
* READ ALL AGENCY BASED CONTROL FILE RECORDS                          *         
***********************************************************************         
                                                                                
GET      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR VBUFFRIN,PARM,('BUFFAINI',BUFFER),BUFKEY,COMFACS                 
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    TSTDELS(TSTDELL),TSTDELS                                         
                                                                                
         GOTOR GETACS              GET ACCESS RECORDS                           
         GOTOR GETIDS              GET USER-ID RECORDS                          
         GOTOR GETLST              GET LIST RECORDS                             
         GOTOR GETUPR              GET USER PROFILE RECORDS                     
         GOTOR GETRPR              GET REPORT PROFILE RECORDS                   
         GOTOR GETPWD              GET PASSWORD RECORDS                         
         GOTOR GETSAC              GET ACCESS RECORDS                           
         GOTOR GETSAF              GET FIELD CONTROL RECORDS                    
         GOTOR GETSAO              GET OPTION CONTROL RECORDS                   
         GOTOR GETOFF              GET OFFICE RECORDS                           
         GOTOR GETDEP              GET DEPARTMENT RECORDS                       
         GOTOR GETPER              GET PERSON RECORDS                           
         GOTOR GETPAG              GET PERSON ACCESS GROUP RECORDS              
         GOTOR GETLAG              GET LIMIT ACCESS GROUP RECORDS               
         GOTOR GETLAL              GET LIMIT ACCESS LIST RECORDS                
         GOTOR GETTSA              GET T/S APPROVER GROUP RECORDS               
*&&US*&& GOTOR GETFAX              GET FAX INFORMATION RECORDS                  
         GOTOR GETDRU              GET DRIVER USER RECORDS                      
*&&US*&& GOTOR GETCPP              GET CPP EXTRACT RULES RECORDS                
         GOTOR GETWRI              GET WRITER DEFINITION RECORDS                
         GOTOR GETRFP              GET RFP GROUP RECORDS                        
*&&UK*&& GOTOR GETNAR              GET NARRATIVE RECORDS                        
*&&UK*&& GOTOR GETEXC              GET EXCHANGE RECORDS                         
*&&US*&& GOTOR GETMOF              GET MEDIA OFFICE RECORDS                     
         GOTOR GETBDR              GET ESS BDE REFORM RECORDS                   
         GOTOR GETXTA              GET EXTRACT CONTROL RECORD (AGENCY)          
         GOTOR GETXTS              GET EXTRACT CONTROL RECORD (ESS)             
         GOTOR GETXTF              GET EXTRACT CONTROL RECORD (XFILE)           
         GOTOR GETXSA              GET EXTRACT ESS/AGENCY PASSIVE               
         GOTOR GETXAF              GET EXTRACT AGENCY/FILE PASSIVE              
         GOTOR GETXSF              GET EXTRACT ESS/FILE PASSIVE                 
                                                                                
GETX     J     EXIT                                                             
         DROP  RB                                                               
                                                                                
***********************************************************************         
* READ BACK BUFFERIN FILE AND TRACE/DELETE CTFILE/GENFIL UPDATES      *         
***********************************************************************         
PUT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   P(L'UPDLIT),UPDLIT                                               
         CLI   WRITE,YES                                                        
         BE    *+10                                                             
         MVC   P(L'GETLIT),GETLIT                                               
         GOTOR VPRINTER                                                         
         XC    BUFREC(BUFRECL),BUFREC                                           
         GOTOR VBUFFRIN,PARM,('BUFFARDH',BUFFER),BUFREC,COMFACS                 
         B     PUT04                                                            
PUT02    GOTOR VBUFFRIN,PARM,('BUFFASEQ',BUFFER),BUFREC,COMFACS                 
PUT04    BNE   PUT30                                                            
*                                                                               
         CLI   TRACE,C'Y'                                                       
         BNE   PUT06                                                            
         GOTOR VHEXOUT,DMCB,BUFKEY,P+1,BUFRECL,HEXTOG                           
         GOTOR VPRINTER                                                         
*                                                                               
PUT06    TM    BUFDSTAT,BUFDSRBD   TEST RECORD TO BE DELETED                    
         BZ    PUT02               NO, NEXT                                     
                                                                                
         CLI   OUTPUT,C'T'                                                      
         BE    PUT08                                                            
         CLI   WRITE,YES                                                        
         BNE   PUT08                                                            
         AP    COMTALLY,=P'1'                                                   
         CP    COMTALLY,COMLIMIT                                                
         BL    PUT08                                                            
         GOTOR VDMGR,DMCB,DMCOMMIT,0                                            
         ZAP   COMTALLY,=P'0'                                                   
                                                                                
PUT08    CLI   BUFKTYPE,BUFKTGEN   TEST FILE TYPE                               
         BH    PUT20                                                            
                                                                                
PUT10    MVC   IO(L'CT5KEY),BUFDRKEY                                            
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),CTFILE,IO,IO                           
         BE    PUT12                                                            
         TM    BUFDSTAT,BUFDSPSV                                                
         BO    PUT02                                                            
         DC    H'0'                                                             
PUT12    AP    CTFILED,PONE                                                     
         CLI   OUTPUT,C'D'                                                      
         BE    PUT14                                                            
         ICM   R1,3,IO+CT5LEN-CT5REC                                            
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,IOL                                                           
         PUT   CONTAPE,IOL                                                      
         CLI   OUTPUT,C'T'                                                      
         BE    PUT02                                                            
PUT14    OI    IO+(CT5STAT-CT5KEY),X'80'                                        
         CLI   UNDO,YES                                                         
         BNE   *+8                                                              
         NI    IO+(CT5STAT-CT5KEY),X'7F'                                        
         CLI   WRITE,YES                                                        
         BNE   PUT02                                                            
         GOTOR VDMGR,DMCB,DMWRT,CTFILE,IO,IO                                    
         BE    PUT02                                                            
         DC    H'0'                                                             
                                                                                
PUT20    MVC   IO(L'GRPKEY),BUFDRKEY                                            
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),GENDIR,IO,IO                           
         JNE   *+2                                                              
         CLI   OUTPUT,C'T'                                                      
         BE    PUT22                                                            
         OI    IO+(GRPKSTAT-GRPKEY),X'80'                                       
         CLI   UNDO,YES                                                         
         BNE   *+8                                                              
         NI    IO+(GRPKSTAT-GRPKEY),X'7F'                                       
         AP    GENDIRD,PONE                                                     
         CLI   WRITE,YES                                                        
         BNE   PUT22                                                            
         GOTOR VDMGR,DMCB,DMWRT,GENDIR,IO,IO                                    
         JNE   *+2                                                              
                                                                                
PUT22    TM    BUFDSTAT,BUFDSPSV                                                
         BO    PUT02                                                            
                                                                                
         MVC   DUB(L'GRPKDA),IO+(GRPKDA-GRPKEY)                                 
         GOTOR VDMGR,DMCB,(DMIND,DMGETREC),GENFIL,DUB,IO,WORK                   
         JNE   *+2                                                              
         CLI   OUTPUT,C'D'                                                      
         BE    PUT24                                                            
         ICM   R1,3,IO+GERLEN-GEXCD                                             
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,IOL                                                           
         PUT   GENTAPE,IOL                                                      
         CLI   OUTPUT,C'T'                                                      
         BE    PUT02                                                            
PUT24    OI    IO+(GRPSTAT-GRPKEY),X'80'                                        
         CLI   UNDO,C'Y'                                                        
         BNE   *+8                                                              
         NI    IO+(GRPSTAT-GRPKEY),X'7F'                                        
         AP    GENFILD,PONE                                                     
         CLI   WRITE,YES                                                        
         BNE   PUT02                                                            
         GOTOR VDMGR,DMCB,DMPUTREC,GENFIL,DUB,IO,WORK                           
         BE    PUT02                                                            
         DC    H'0'                                                             
                                                                                
PUT30    ZAP   LINE,PNINETY                                                     
         GOTOR VPRINTER                                                         
         MVC   P(L'CTFILELD),CTFILELD                                           
         OI    CTFILED+L'CTFILED-1,X'0F'                                        
         UNPK  P+L'CTFILELD(7),CTFILED                                          
         GOTOR VPRINTER                                                         
         MVC   P(L'GENDIRLD),GENDIRLD                                           
         OI    GENDIRD+L'GENDIRD-1,X'0F'                                        
         UNPK  P+L'GENDIRLD(7),GENDIRD                                          
         GOTOR VPRINTER                                                         
         MVC   P(L'GENFILLD),GENFILLD                                           
         OI    GENFILD+L'GENFILD-1,X'0F'                                        
         UNPK  P+L'GENFILLD(7),GENFILD                                          
         GOTOR VPRINTER                                                         
                                                                                
PUTX     J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ ACCESS RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETACS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON01),RECCON01                                           
         GOTOR VPRINTER                                                         
                                                                                
         CLI   MODE,C'O'           SKIP IF ORPHAN MODE                          
         JE    GETACS10                                                         
                                                                                
         LA    R2,DELETE           R2=A(DELETE LIST)                            
         J     GETACS04                                                         
                                                                                
GETACS02 CLI   0(R2),C' '          TEST END OF DELETE LIST                      
         JE    EXIT                                                             
                                                                                
         USING CT5REC,IO                                                        
GETACS04 XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R2)                                                   
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),CTFILE,CT5REC,CT5REC                   
         JE    GETACS05                                                         
         CLI   UNDO,C'Y'                                                        
         JNE   GETACS06                                                         
         TM    DMCB+8,X'02'                                                     
         JNO   GETACS06                                                         
*                                                                               
GETACS05 GOTOR PUTBUF,DMCB,('BUFDSRBD',CT5KALPH),CT5KEY,               *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
         MVC   P(L'ACSLIT),ACSLIT                                               
         MVC   P+L'ACSLIT+1(L'CT5KALPH),CT5KALPH                                
         MVC   P+L'ACSLIT+L'CT5KALPH+2(L'WILDEL),WILDEL                         
         GOTOR VPRINTER                                                         
         J     GETACS08                                                         
                                                                                
GETACS06 MVC   P(L'ERROR2),ERROR2  INVALID AGENCY                               
         MVC   P+L'ERROR2(L'CT5KALPH),0(R2)                                     
         GOTOR VPRINTER                                                         
         GOTOR PUTBUF,DMCB,('BUFDSNOF',CT5KALPH),0,                    *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
                                                                                
GETACS08 AHI   R2,L'CT5KALPH+1     BUMP TO NEXT AGENCY                          
         J     GETACS02                                                         
                                                                                
         USING CT5REC,IO                                                        
GETACS10 XC    CT5KEY,CT5KEY       ORPHAN MODE. ADD IDS TO KEEP                 
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CT5REC,CT5REC                   
         J     GETACS14                                                         
                                                                                
GETACS12 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CT5REC,CT5REC                   
                                                                                
GETACS14 JNE   *+2                                                              
         CLI   CT5KTYP,CT5KTYPQ                                                 
         JNE   EXIT                                                             
         GOTOR PUTBUF,DMCB,(0,CT5KALPH),CT5KEY,                        *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
         MVC   P(L'ACSLIT),ACSLIT                                               
         MVC   P+L'ACSLIT+1(L'CT5KALPH),CT5KALPH                                
         MVC   P+L'ACSLIT+L'CT5KALPH+2(L'WILKEPT),WILKEPT                       
         GOTOR VPRINTER                                                         
         J     GETACS12                                                         
                                                                                
***********************************************************************         
* READ ID RECORDS AND POST TO BUFFER                                  *         
***********************************************************************         
GETIDS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON02),RECCON02                                           
         GOTOR VPRINTER                                                         
*                                                                               
         USING CTIKEY,IO                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVI   CTIKID,1                                                         
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTIKEY,CTIKEY                   
         J     GETIDS04                                                         
GETIDS02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTIKEY,CTIKEY                   
GETIDS04 JNE   *+2                                                              
         CLI   CTIKTYP,CTIKTYPQ    C'I' ID Record                               
         JNE   EXIT                                                             
         OC    CTIKSPAR,CTIKSPAR   Binary zeroes                                
         JNZ   GETIDS02                                                         
         MVC   SAVEKEY,IO          Hold ID record key                           
         MVI   FLAG,0              Reset FLAG                                   
*                                                                               
         XC    WORK(2),WORK                                                     
         LA    RF,CTIDATA                                                       
         USING CTAGYD,RF                                                        
GETIDS06 CLI   CTAGYEL,0                                                        
         JE    GETIDS12                                                         
         CLI   CTAGYEL,CTDSCELQ                                                 
         JNE   *+14                                                             
         MVC   WORK(2),CTDSC-CTDSCD(RF)                                         
         J     GETIDS08                                                         
         CLI   CTAGYEL,CTAGYELQ                                                 
         JE    GETIDS10                                                         
GETIDS08 LLC   R0,CTAGYLEN                                                      
         AR    RF,R0                                                            
         J     GETIDS06                                                         
*                                                                               
GETIDS10 MVC   WORK+2(L'CTAGYID),CTAGYID                                        
         MVI   BUFDSTAT,BUFDSRBD   RECORD TO BE DELETED                         
         GOTOR TSTDELA,CTAGYID     TEST AGENCY TO BE DELETED                    
         JNE   GETIDS12            NO                                           
*&&US                                                                           
         BRAS  RE,KREPID           REP SYSTEM ID MAY STILL BE NEEDED            
         JNE   GETIDS13            DO NOT DELETE, CONTAINS DARE INFO            
*&&                                                                             
         J     GETIDS20                                                         
*                                                                               
GETIDS12 CLI   MODE,C'O'           TEST ORPHAN MODE                             
         JNE   GETIDS02            NO, NEXT                                     
GETIDS13 MVI   BUFDSTAT,0          ADD BUT NOT TO DELETE                        
*                                                                               
GETIDS20 GOTOR PUTBUF,DMCB,(BUFDSTAT,CTIKID),CTIKEY,                   *        
               ('BUFKTIDC',L'BUFKUSER-1),WORK+2                                 
*                                                                               
         MVC   WORK+4(L'CTIKID),CTIKID                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         OC    CTIKNUM,WORK                                                     
         JZ    *+2                                                              
         OI    BUFDSTAT,BUFDSPSV   INDICATE PASSIVE                             
         GOTOR PUTBUF,DMCB,(BUFDSTAT,CTIKNUM),CTIKEY,                  *        
               ('BUFKTID#',L'BUFKUSID-1),WORK+2                                 
         USING CT9BKEY,IO                                                       
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,WORK+2                                                  
         MVC   CT9BKNUM,WORK                                                    
         MVC   DUB(2),WORK+2                                                    
         MVC   DUB+2(2),WORK                                                    
         GOTOR PUTBUF,DMCB,(BUFDSTAT,DUB),CT9BKEY,                     *        
               ('BUFKTIDA',L'BUFKUSAG-1),WORK+2                                 
*                                                                               
         MVC   P(L'UIDLIT),UIDLIT                                               
         LA    R1,P+L'UIDLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CTAGYID,R1),WORK+2                                           
         MVI   1+L'CTAGYID(R1),C')'                                             
         AHI   R1,2+L'CTAGYID                                                   
         MVC   0(L'CTIKID,R1),WORK+4                                            
         AHI   R1,L'CTIKID-1                                                    
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         SR    RF,RF                                                            
         ICM   RF,3,WORK                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVI   1(R1),C'/'                                                       
         UNPK  2(5,R1),DUB                                                      
*                                                                               
         MVC   8(L'WILKEPT,R1),WILKEPT                                          
         TM    BUFDSTAT,BUFDSRBD   TEST DELETING                                
         JZ    *+10                NO, SKIP                                     
         MVC   8(L'WILDEL,R1),WILDEL                                            
*&&US                                                                           
         TM    FLAG,FLAGDAPA          DARE Partner found                        
         JZ    *+10                   NO                                        
         MVC   P+55(L'DAPALIT),DAPALIT                                          
*                                                                               
         TM    FLAG,FLAGDAPR          DARE Prefix found                         
         JZ    *+10                   NO                                        
         MVC   P+55(L'DAPRLIT),DAPRLIT                                          
*&&                                                                             
         GOTOR VPRINTER                                                         
         MVC   IO(L'SAVEKEY),SAVEKEY                                            
         J     GETIDS02                                                         
                                                                                
*&&US                                                                           
*----------------------------------------------------------------------         
* Keep REP IDs still being referenced for DARE                                  
*----------------------------------------------------------------------         
         USING CTIKEY,IO                                                        
KREPID   NTR1                                                                   
         MVI   EXITCC,YES            Assume we're deleting                      
         MVI   BYTE,0                BYTE will hold partner ID                  
*                                                                               
         LA    R3,CTIDATA                                                       
KRID10   CLI   0(R3),0                                                          
         JE    KRID100                                                          
         CLI   0(R3),CTUSAELQ        X'33' US Agency Extra Info                 
         JE    KRID30                                                           
KRID20   LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         J     KRID10                                                           
*                                                                               
         USING CTUSAD,R3                                                        
KRID30   CLI   CTUSADPI,C' '         DARE Partner ID                            
         JNH   KRID20                                                           
         MVC   BYTE,CTUSADPI         Hold the DARE Partner ID in byte           
         J     KRID20                                                           
         DROP  R3                                                               
*                                                                               
KRID100  CLI   BYTE,0                DARE Partner ID found?                     
         JE    KRID105               NO: Look for prefix record                 
         OI    FLAG,FLAGDAPA                                                    
         J     KRIDNOX               Do not delete, Dare Partner ID             
*                                                                               
KRID105  LA    R3,CTIKID+L'CTIKID-1  Calculate ID length                        
         LHI   R4,L'CTIKID-1                                                    
KRID110  CLI   0(R3),C' '                                                       
         JH    KRID150                                                          
         AHI   R3,-1                                                            
         AHI   R4,-1                                                            
         JNM   KRID110                                                          
         DC    H'0'                  No ID? Should never happen                 
*                                                                               
         USING CTEQRECD,IO                                                      
KRID150  XC    CTEQKEY,CTEQKEY                                                  
         MVI   CTEQKTYP,CTEQKTYQ     X'00'                                      
         MVI   CTEQKSTY,CTEQKSTQ     X'3A' REP Partner Passive                  
*                                                                               
         EXRL  R4,KRIDEXMI                                                      
         J     KRIDEXMA                                                         
KRIDEXMI MVC   CTEQKPAR(0),SAVEKEY+(CTIKID-CTIKEY)                              
KRIDEXMA OC    CTEQKPAR,SPACES                                                  
*                                                                               
         GOTOR VDMGR,DMCB,DMRDHI,GENDIR,CTEQKEY,CTEQKEY                         
         JNE   *+2                                                              
         CLI   CTEQKTYP,CTEQKTYQ     X'00'                                      
         JNE   KRID180                                                          
         CLI   CTEQKSTY,CTEQKSTQ     X'3A'                                      
         JNE   KRID180                                                          
*                                                                               
         LA    RE,CTEQKPAR+L'CTEQKPAR-1 Calculate prefix length                 
         LHI   RF,L'CTEQKPAR-1                                                  
KRID160  CLI   0(RE),C' '                                                       
         JH    KRID170                                                          
         AHI   RE,-1                                                            
         AHI   RF,-1                                                            
         JNM   KRID160                                                          
         J     KRID180               No prefix, no good, just move on           
*                                                                               
KRID170  EXRL  RF,KRIDEXCI                                                      
         J     KRIDEXCA                                                         
KRIDEXCI CLC   CTEQKPAR(0),SAVEKEY+(CTIKID-CTIKEY)                              
KRIDEXCA JNE   KRID180               No match, keep checking                    
         MVI   EXITCC,NO             Found a match, don't delete ID             
         OI    FLAG,FLAGDAPR                                                    
         J     KRID200                                                          
*                                                                               
KRID180  AHI   R4,-1                                                            
         JNM   KRID150               Still more to check                        
*                                                                               
         USING CTIKEY,IO                                                        
KRID200  MVC   IO(L'SAVEKEY),SAVEKEY Re-read ID Record                          
         GOTOR VDMGR,DMCB,(DMIND,DMREAD),CTFILE,CTIKEY,CTIKEY                   
         JNE   *+2                                                              
         J     KRIDX                 Didn't find a prefix, ok to delete         
*                                                                               
KRIDNOX  MVI   EXITCC,NO             Don't delete ID                            
*                                                                               
KRIDX    CLI   FORCE,YES             Are we forcing deletes?                    
         JNE   *+8                   No: then exit as is                        
         MVI   EXITCC,YES            Yes: exit ok to delete                     
         CLI   EXITCC,YES                                                       
         J     EXIT                                                             
                                                                                
*&&                                                                             
***********************************************************************         
* READ LIST RECORDS AND POST TO BUFFER                                *         
***********************************************************************         
GETLST   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON03),RECCON03                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING CTWREC,IO                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTWKEY,CTWKEY                   
         J     GETLST04                                                         
GETLST02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTWKEY,CTWKEY                   
GETLST04 JNE   *+2                                                              
         CLI   CTWKTYP,CTWKTYPQ                                                 
         JNE   EXIT                                                             
         CLI   CTWKAGY,0           TEST AGENCY BASED LIST                       
         JE    GETLST02                                                         
                                                                                
         GOTOR TSTDELA,CTWKAGY                                                  
         JNE   GETLST02                                                         
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CTWKAGY),CTWKEY,                *        
               ('BUFKTIDG',L'BUFKIDGR-1),0                                      
         MVC   P(L'LSTLIT),LSTLIT                                               
         LA    R1,P+L'LSTLIT+1                                                  
         MVC   0(L'CTWKAGY,R1),CTWKAGY                                          
         AHI   R1,L'CTWKAGY                                                     
         MVI   0(R1),C'/'                                                       
         MVC   1(L'CTWKREC,R1),CTWKREC                                          
         AHI   R1,L'CTWKREC+1                                                   
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         MVC   0(L'CTWKID,R1),CTWKID                                            
         AHI   R1,L'CTWKID-1                                                    
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETLST02                                                         
         EJECT                                                                  
***********************************************************************         
* READ USER PROFILE RECORDS AND POST TO BUFFER                        *         
***********************************************************************         
                                                                                
GETUPR   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON04),RECCON04                                           
         GOTOR VPRINTER                                                         
         USING CTUREC,IO                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTUKEY,CTUKEY                   
         J     GETUPR04                                                         
GETUPR02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTUKEY,CTUKEY                   
GETUPR04 JNE   *+2                                                              
         CLI   CTUKTYP,CTUKTYPQ                                                 
         JNE   EXIT                                                             
         OC    CTUKAGY,CTUKAGY                                                  
         JZ    GETUPR02                                                         
         CLI   CTUKAGY,X'40'       TEST AGENCY LEVEL PROFILE                    
         JNL   GETUPR06                                                         
         GOTOR TSTDELU,CTUKAGY                                                  
         JNE   GETUPR02                                                         
         J     GETUPR08                                                         
                                                                                
GETUPR06 GOTOR TSTDELA,CTUKAGY                                                  
         JNE   GETUPR02                                                         
                                                                                
GETUPR08 GOTOR PUTBUF,DMCB,('BUFDSRBD',CTUKSYS),CTUKEY,                *        
               ('BUFKTUPR',L'BUFKUSPR-1),0                                      
         MVC   P(L'UPRLIT),UPRLIT                                               
         LA    R1,P+L'UPRLIT+1                                                  
         MVI   0(R1),C'('                                                       
         CLI   CTUKAGY,X'40'       TEST AGENCY LEVEL PROFILE                    
         JL    *+22                                                             
         MVC   1(L'CTUKAGY,R1),CTUKAGY                                          
         MVI   1+L'CTUKAGY(R1),C')'                                             
         AHI   R1,L'CTUKAGY+2                                                   
         J     GETUPR12                                                         
         MVC   1(L'TSTUIDAG,R1),TSTUIDAG                                        
         MVI   1+L'TSTUIDAG(R1),C')'                                            
         AHI   R1,L'TSTUIDAG+2                                                  
GETUPR12 MVC   0(L'CTUKSYS,R1),CTUKSYS                                          
         MVI   L'CTUKSYS(R1),C'/'                                               
         AHI   R1,L'CTUKSYS+1                                                   
         MVC   0(L'CTUKPROG,R1),CTUKPROG                                        
         CLI   CTUKPROG,C' '                                                    
         JH    *+14                                                             
         MVC   0(L'CTUKPROG-1,R1),CTUKPROG+1                                    
         SHI   R1,1                                                             
         AHI   R1,L'CTUKPROG                                                    
         MVI   0(R1),C'/'                                                       
         CLI   CTUKAGY,X'40'                                                    
         JNL   GETUPR14                                                         
         SR    R0,R0                                                            
         ICM   R0,3,CTUKAGY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         J     GETUPR16                                                         
GETUPR14 MVC   1(L'CTUKAGY,R1),CTUKAGY                                          
         AHI   R1,L'CTUKAGY+1                                                   
GETUPR16 MVI   0(R1),C'/'                                                       
         CLI   CTUKMED,0                                                        
         JNE   GETUPR18                                                         
         MVC   1(L'ALLLIT,R1),ALLLIT                                            
         AHI   R1,L'ALLLIT+1                                                    
         J     GETUPR20                                                         
GETUPR18 MVC   1(L'CTUKMED,R1),CTUKMED                                          
         AHI   R1,L'CTUKMED+1                                                   
GETUPR20 MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         CLI   CTUKCLT,0                                                        
         JNE   GETUPR22                                                         
         MVC   0(L'ALLLIT,R1),ALLLIT                                            
         AHI   R1,L'ALLLIT+1                                                    
         J     GETUPR24                                                         
GETUPR22 MVC   0(L'CTUKCLT,R1),CTUKCLT                                          
         AHI   R1,L'CTUKCLT-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   1(L'CTUKCLT-1,R1),SPACES                                         
         AHI   R1,2                                                             
GETUPR24 MVC   0(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETUPR02                                                         
         EJECT                                                                  
***********************************************************************         
* READ REPORT PROFILE RECORDS AND POST TO BUFFER                      *         
***********************************************************************         
                                                                                
GETRPR   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON05),RECCON05                                           
         GOTOR VPRINTER                                                         
         USING CTPREC,IO                                                        
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,CTPKTYPQ                                                 
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTPKEY,CTPKEY                   
         J     GETRPR04                                                         
GETRPR02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTPKEY,CTPKEY                   
GETRPR04 JNE   *+2                                                              
         CLI   CTPKTYP,CTPKTYPQ                                                 
         JNE   EXIT                                                             
         OC    CTPKORIG,CTPKORIG                                                
         JZ    GETRPR02                                                         
         GOTOR TSTDELU,CTPKORIG                                                 
         JNE   GETRPR02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CTPKSYS),CTPKEY,                *        
               ('BUFKTRPR',L'BUFKREPR-1),0                                      
         MVC   P(L'RPRLIT),RPRLIT                                               
         LA    R1,P+L'RPRLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'TSTUIDAG,R1),TSTUIDAG                                        
         MVI   1+L'TSTUIDAG(R1),C')'                                            
         AHI   R1,L'TSTUIDAG+2                                                  
         MVC   0(L'CTPKSYS,R1),CTPKSYS                                          
         MVI   L'CTPKSYS(R1),C'/'                                               
         AHI   R1,L'CTPKSYS+1                                                   
         MVC   0(L'CTPKPROG,R1),CTPKPROG                                        
         AHI   R1,L'CTPKPROG                                                    
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,CTPKORIG                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         MVC   7(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETRPR02                                                         
         EJECT                                                                  
***********************************************************************         
* READ PASSWORD RECORD AND POST TO BUFFER                             *         
***********************************************************************         
                                                                                
GETPWD   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON06),RECCON06                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING CT0REC,IO                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
GETPWD02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CT0KEY,CT0KEY                   
         J     GETPWD06                                                         
GETPWD04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CT0KEY,CT0KEY                   
GETPWD06 JNE   *+2                                                              
         CLI   CT0KTYP,CT0KTEQU                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,CT0KAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETPWD08            YES                                          
         ICM   R1,3,CT0KAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         STCM  R1,3,CT0KAGY                                                     
         J     GETPWD02                                                         
                                                                                
GETPWD08 CLI   CT0KOFFC,0          TEST PASSWORD NAME RECORD                    
         JNE   GETPWD12                                                         
         CLI   CT0KCODE,0          TEST PASSWORD RECORD                         
         JNE   GETPWD10                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CT0KAGY),CT0KEY,                *        
               ('BUFKTPW#',L'BUFKPWD#-1),0                                      
         MVC   P(L'PW#LIT),PW#LIT                                               
         LA    R1,P+L'PW#LIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CT0KAGY,R1),CT0KAGY                                          
         MVI   1+L'CT0KAGY(R1),C')'                                             
         AHI   R1,L'CT0KAGY+2                                                   
         SR    R0,R0                                                            
         ICM   R0,3,CT0KNUM                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(5,R1),DUB                                                      
         MVC   6(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETPWD04                                                         
                                                                                
GETPWD10 GOTOR PUTBUF,DMCB,('BUFDSRBD',CT0KAGY),CT0KEY,                *        
               ('BUFKTPWC',L'BUFKPWDC-1),0                                      
         MVC   P(L'PWCLIT),PWCLIT                                               
         LA    R1,P+L'PWCLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CT0KAGY,R1),CT0KAGY                                          
         MVI   1+L'CT0KAGY(R1),C')'                                             
         AHI   R1,L'CT0KAGY+2                                                   
         MVC   0(L'CT0KCODE,R1),CT0KCODE                                        
         AHI   R1,L'CT0KCODE-1                                                  
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETPWD04                                                         
                                                                                
GETPWD12 GOTOR PUTBUF,DMCB,('BUFDSRBD',CT0KAGY),CT0KEY,                *        
               ('BUFKTPWN',L'BUFKPWDN-1),0                                      
         MVC   P(L'PWNLIT),PWNLIT                                               
         LA    R1,P+L'PWNLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CT0KAGY,R1),CT0KAGY                                          
         MVI   1+L'CT0KAGY(R1),C')'                                             
         AHI   R1,L'CT0KAGY+2                                                   
         MVC   0(CT0LEN-CT0KOFFC,R1),CT0KOFFC                                   
         AHI   R1,CT0LEN-CT0KOFFC-1                                             
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETPWD04                                                         
         EJECT                                                                  
***********************************************************************         
* READ ACCESS RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETSAC   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON07),RECCON07                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAASREC,IO                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
GETSAC02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAASKEY,SAASKEY                 
         J     GETSAC06                                                         
GETSAC04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAASKEY,SAASKEY                 
GETSAC06 JNE   *+2                                                              
         CLI   SAASTYP,SAASTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAASSUB,SAASSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAASAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETSAC08            YES                                          
         ICM   R1,3,SAASAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         STCM  R1,3,SAASAGY                                                     
         J     GETSAC02                                                         
                                                                                
GETSAC08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAASAGY),SAASKEY,               *        
               ('BUFKTSAC',L'BUFKSSAC-1),0                                      
         MVC   P(L'SACLIT),SACLIT                                               
         LA    R1,P+L'SACLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAASAGY,R1),SAASAGY                                          
         MVI   1+L'SAASAGY(R1),C')'                                             
         AHI   R1,L'SAASAGY+2                                                   
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,SAASOVS,(R0),L'SAASOVS+L'SAASPGM,HEXTOG             
         LR    R1,R0                                                            
         AHI   R1,(L'SAASOVS+L'SAASPGM)*2                                       
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SAASUID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SAASAGN                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         MVC   7(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETSAC04                                                         
         EJECT                                                                  
***********************************************************************         
* READ FIELD CONTROL RECORDS AND POST TO BUFFER                       *         
***********************************************************************         
                                                                                
GETSAF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON08),RECCON08                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAFCREC,IO                                                       
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
GETSAF02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAFCKEY,SAFCKEY                 
         J     GETSAF06                                                         
GETSAF04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAFCKEY,SAFCKEY                 
GETSAF06 JNE   *+2                                                              
         CLI   SAFCTYP,SAFCTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAFCSUB,SAFCSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAFCAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETSAF08            YES                                          
         ICM   R1,3,SAFCAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         STCM  R1,3,SAFCAGY                                                     
         J     GETSAF02                                                         
                                                                                
GETSAF08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAFCAGY),SAFCKEY,               *        
               ('BUFKTSAF',L'BUFKSSAF-1),0                                      
         MVC   P(L'SAFLIT),SAFLIT                                               
         LA    R1,P+L'SAFLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAFCAGY,R1),SAFCAGY                                          
         MVI   1+L'SAFCAGY(R1),C')'                                             
         AHI   R1,L'SAFCAGY+2                                                   
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,SAFCOVS,(R0),L'SAFCOVS+L'SAFCPGM,HEXTOG             
         LR    R1,R0                                                            
         AHI   R1,(L'SAFCOVS+L'SAFCPGM)*2                                       
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SAFCUID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SAFCAGN                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         MVC   7(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETSAF04                                                         
         EJECT                                                                  
***********************************************************************         
* READ OPTION CONTROL RECORDS AND POST TO BUFFER                      *         
***********************************************************************         
                                                                                
GETSAO   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON09),RECCON09                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAOCREC,IO                                                       
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
GETSAO02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAOCKEY,SAOCKEY                 
         J     GETSAO06                                                         
GETSAO04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAOCKEY,SAOCKEY                 
GETSAO06 JNE   *+2                                                              
         CLI   SAOCTYP,SAOCTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAOCSUB,SAOCSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAOCAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETSAO08            YES                                          
         ICM   R1,3,SAOCAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         STCM  R1,3,SAOCAGY                                                     
         J     GETSAO02                                                         
                                                                                
GETSAO08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAOCAGY),SAOCKEY,               *        
               ('BUFKTSAO',L'BUFKSSAO-1),0                                      
         MVC   P(L'SAOLIT),SAOLIT                                               
         LA    R1,P+L'SAOLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAOCAGY,R1),SAOCAGY                                          
         MVI   1+L'SAOCAGY(R1),C')'                                             
         AHI   R1,L'SAOCAGY+2                                                   
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,SAOCOVS,(R0),L'SAOCOVS+L'SAOCPGM,HEXTOG             
         LR    R1,R0                                                            
         AHI   R1,(L'SAOCOVS+L'SAOCPGM)*2                                       
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SAOCUID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SAOCAGN                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         MVC   7(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETSAO04                                                         
         EJECT                                                                  
***********************************************************************         
* READ OFFICE RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETOFF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON10),RECCON10                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAOFREC,IO                                                       
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
GETOFF02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAOFKEY,SAOFKEY                 
         J     GETOFF06                                                         
GETOFF04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAOFKEY,SAOFKEY                 
GETOFF06 JNE   *+2                                                              
         CLI   SAOFTYP,SAOFTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAOFSUB,SAOFSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAOFAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETOFF08            YES                                          
         ICM   R1,3,SAOFAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         STCM  R1,3,SAOFAGY                                                     
         J     GETOFF02                                                         
                                                                                
GETOFF08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAOFAGY),SAOFKEY,               *        
               ('BUFKTOFF',L'BUFKOFFC-1),0                                      
         MVC   P(L'OFFLIT),OFFLIT                                               
         LA    R1,P+L'OFFLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAOFAGY,R1),SAOFAGY                                          
         MVI   1+L'SAOFAGY(R1),C')'                                             
         AHI   R1,L'SAOFAGY+2                                                   
         MVC   0(L'SAOFOID,R1),SAOFOID                                          
         AHI   R1,L'SAOFOID-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETOFF04                                                         
         EJECT                                                                  
***********************************************************************         
* READ DEPARTMENT RECORDS AND POST TO BUFFER                          *         
***********************************************************************         
                                                                                
GETDEP   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON11),RECCON11                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SADPREC,IO                                                       
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
GETDEP02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SADPKEY,SADPKEY                 
         J     GETDEP06                                                         
GETDEP04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SADPKEY,SADPKEY                 
GETDEP06 JNE   *+2                                                              
         CLI   SADPTYP,SADPTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SADPSUB,SADPSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SADPAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETDEP08            YES                                          
         ICM   R1,3,SADPAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         STCM  R1,3,SADPAGY                                                     
         J     GETDEP02                                                         
                                                                                
GETDEP08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SADPAGY),SADPKEY,               *        
               ('BUFKTDEP',L'BUFKDEPT-1),0                                      
         MVC   P(L'DEPLIT),DEPLIT                                               
         LA    R1,P+L'DEPLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SADPAGY,R1),SADPAGY                                          
         MVI   1+L'SADPAGY(R1),C')'                                             
         AHI   R1,L'SADPAGY+2                                                   
         MVC   0(L'SADPOID,R1),SADPOID                                          
         AHI   R1,L'SADPOID-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
         MVC   2(L'SADPDID,R1),SADPDID                                          
         AHI   R1,1+L'SADPDID                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETDEP04                                                         
         EJECT                                                                  
***********************************************************************         
* READ PERSON RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETPER   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON12),RECCON12                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAPEREC,IO                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
GETPER02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAPEKEY,SAPEKEY                 
         J     GETPER06                                                         
GETPER04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAPEKEY,SAPEKEY                 
GETPER06 JNE   *+2                                                              
         CLI   SAPETYP,SAPETYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAPESUB,SAPESUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAPEAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETPER08            YES                                          
         ICM   R1,3,SAPEAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         STCM  R1,3,SAPEAGY                                                     
         J     GETPER02                                                         
                                                                                
GETPER08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAPEAGY),SAPEKEY,               *        
               ('BUFKTPER',L'BUFKPERS-1),0                                      
         MVC   P(L'PERLIT),PERLIT                                               
         LA    R1,P+L'PERLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAPEAGY,R1),SAPEAGY                                          
         MVI   1+L'SAPEAGY(R1),C')'                                             
         AHI   R1,L'SAPEAGY+2                                                   
         MVC   0(L'SAPEPID,R1),SAPEPID                                          
         AHI   R1,L'SAPEPID-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
         AHI   R1,2                                                             
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,SAPEDEF,(R0),L'SAPEDEF,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,L'SAPEDEF*2                                                   
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETPER04                                                         
         EJECT                                                                  
***********************************************************************         
* READ ACCESS GROUP RECORDS AND POST TO BUFFER                        *         
***********************************************************************         
                                                                                
GETPAG   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON13),RECCON13                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAAGREC,IO                                                       
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
GETPAG02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAAGKEY,SAAGKEY                 
         J     GETPAG06                                                         
GETPAG04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAAGKEY,SAAGKEY                 
GETPAG06 JNE   *+2                                                              
         CLI   SAAGTYP,SAAGTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAAGSUB,SAAGSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAAGAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETPAG08            YES                                          
         ICM   R1,3,SAAGAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         STCM  R1,3,SAAGAGY                                                     
         J     GETPAG02                                                         
                                                                                
GETPAG08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAAGAGY),SAAGKEY,               *        
               ('BUFKTPAG',L'BUFKPAGR-1),0                                      
         MVC   P(L'PAGLIT),PAGLIT                                               
         LA    R1,P+L'PAGLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAAGAGY,R1),SAAGAGY                                          
         MVI   1+L'SAAGAGY(R1),C')'                                             
         AHI   R1,L'SAAGAGY+2                                                   
         MVC   0(L'SAAGAGR,R1),SAAGAGR                                          
         AHI   R1,L'SAAGAGR-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C' '                                                       
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETPAG04                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIMIT ACCESS GROUP RECORDS AND POST TO BUFFER                  *         
***********************************************************************         
                                                                                
GETLAG   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON14),RECCON14                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SALAREC,IO                                                       
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
GETLAG02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SALAKEY,SALAKEY                 
         J     GETLAG06                                                         
GETLAG04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SALAKEY,SALAKEY                 
GETLAG06 JNE   *+2                                                              
         CLI   SALATYP,SALATYPQ                                                 
         JNE   EXIT                                                             
         CLI   SALASUB,SALASUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SALAAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETLAG08            YES                                          
         ICM   R1,3,SALAAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         STCM  R1,3,SALAAGY                                                     
         J     GETLAG02                                                         
                                                                                
GETLAG08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SALAAGY),SALAKEY,               *        
               ('BUFKTLAG',L'BUFKLAGR-1),0                                      
         MVC   P(L'LAGLIT),LAGLIT                                               
         LA    R1,P+L'LAGLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SALAAGY,R1),SALAAGY                                          
         MVI   1+L'SALAAGY(R1),C')'                                             
         AHI   R1,L'SALAAGY+2                                                   
         MVC   0(L'SALAAGR,R1),SALAAGR                                          
         AHI   R1,L'SALAAGR-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C' '                                                       
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETLAG04                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIMIT ACCESS LIST RECORDS AND POST TO BUFFER                   *         
***********************************************************************         
                                                                                
GETLAL   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON15),RECCON15                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SALMREC,IO                                                       
         XC    SALMKEY,SALMKEY                                                  
         MVI   SALMTYP,SALMTYPQ                                                 
         MVI   SALMSUB,SALMSUBQ                                                 
                                                                                
GETLAL02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SALMKEY,SALMKEY                 
         J     GETLAL06                                                         
GETLAL04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SALMKEY,SALMKEY                 
GETLAL06 JNE   *+2                                                              
         CLI   SALMTYP,SALMTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SALMSUB,SALMSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SALMAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETLAL08            YES                                          
         ICM   R1,7,SALMSYS        SYS + AGY                                    
         LA    R1,1(,R1)                                                        
         XC    SALMKEY,SALMKEY                                                  
         MVI   SALMTYP,SALMTYPQ                                                 
         MVI   SALMSUB,SALMSUBQ                                                 
         STCM  R1,7,SALMSYS                                                     
         J     GETLAL02                                                         
                                                                                
GETLAL08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SALMAGY),SALMKEY,               *        
               ('BUFKTLAG',L'BUFKLAGR-1),0                                      
         MVC   P(L'LALLIT),LALLIT                                               
         LA    R1,P+L'LALLIT+1                                                  
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,SALMSYS,(R0),L'SALMSYS,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,L'SALMSYS*2                                                   
         MVC   0(L'SALMAGY,R1),SALMAGY                                          
         AHI   R1,L'SALMAGY                                                     
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SALMLID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETLAL04                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIMIT ACCESS GROUP RECORDS AND POST TO BUFFER                  *         
***********************************************************************         
                                                                                
GETTSA   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON16),RECCON16                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING SAAPREC,IO                                                       
         XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
GETTSA02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,SAAPKEY,SAAPKEY                 
         J     GETTSA06                                                         
GETTSA04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,SAAPKEY,SAAPKEY                 
GETTSA06 JNE   *+2                                                              
         CLI   SAAPTYP,SAAPTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAAPSUB,SAAPSUBQ                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,SAAPAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETTSA08            YES                                          
         ICM   R1,3,SAAPAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         STCM  R1,3,SAAPAGY                                                     
         J     GETTSA02                                                         
                                                                                
GETTSA08 GOTOR PUTBUF,DMCB,('BUFDSRBD',SAAPAGY),SAAPKEY,               *        
               ('BUFKTTSA',L'BUFKTSAG-1),0                                      
         MVC   P(L'TSALIT),TSALIT                                               
         LA    R1,P+L'TSALIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'SAAPAGY,R1),SAAPAGY                                          
         MVI   1+L'SAAPAGY(R1),C')'                                             
         AHI   R1,L'SAAPAGY+2                                                   
         MVC   0(L'SAAPAGR,R1),SAAPAGR                                          
         AHI   R1,L'SAAPAGR-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C' '                                                       
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETTSA04                                                         
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* READ FAX INFORMATION RECORDS AND POST TO BUFFER                     *         
***********************************************************************         
                                                                                
GETFAX   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON17),RECCON17                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING CTFXREC,IO                                                       
         XC    CTFXKEY,CTFXKEY                                                  
         MVI   CTFXKTYP,CTFXEQU                                                 
GETFAX02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTFXKEY,CTFXKEY                 
         J     GETFAX06                                                         
GETFAX04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTFXKEY,CTFXKEY                 
GETFAX06 JNE   *+2                                                              
         CLI   CTFXKTYP,CTFXEQU                                                 
         JNE   EXIT                                                             
         GOTOR TSTDELA,CTFXAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETFAX08            YES                                          
         ICM   R1,3,CTFXAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    CTFXKEY,CTFXKEY                                                  
         MVI   CTFXKTYP,CTFXEQU                                                 
         STCM  R1,3,CTFXAGY                                                     
         J     GETFAX02                                                         
                                                                                
GETFAX08 GOTOR PUTBUF,DMCB,('BUFDSRBD',CTFXAGY),CTFXKEY,               *        
               ('BUFKTFAX',L'BUFKFAXI-1),0                                      
         MVC   P(L'FAXLIT),FAXLIT                                               
         LA    R1,P+L'FAXLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CTFXAGY,R1),CTFXAGY                                          
         MVI   1+L'CTFXAGY(R1),C')'                                             
         AHI   R1,L'CTFXAGY+2                                                   
         MVC   0(L'CTFXCODE,R1),CTFXCODE                                        
         AHI   R1,L'CTFXCODE-1                                                  
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         OC    CTFXSUBC,CTFXSUBC                                                
         JZ    GETFAX10                                                         
         MVI   1(R1),C'/'                                                       
         MVC   2(L'CTFXSUBC,R1),CTFXSUBC                                        
         AHI   R1,L'CTFXSUBC+1                                                  
         CLI   0(R1),C' '                                                       
         JH    GETFAX10                                                         
         BRCT  R1,*-8                                                           
GETFAX10 MVI   1(R1),C' '                                                       
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETFAX04                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* READ DRIVER USER RECORDS AND POST TO BUFFER                         *         
***********************************************************************         
                                                                                
GETDRU   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON19),RECCON19                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING CT02REC,IO                                                       
         XC    CT02KEY,CT02KEY                                                  
         MVI   CT02KTYP,CT02KTYQ                                                
GETDRU02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CT02KEY,CT02KEY                 
         J     GETDRU06                                                         
GETDRU04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CT02KEY,CT02KEY                 
GETDRU06 JNE   *+2                                                              
         CLI   CT02KTYP,CT02KTYQ                                                
         JNE   EXIT                                                             
         GOTOR TSTDELA,CT02KAGY    TEST THIS AGENCY TO BE DELETED               
         JE    GETDRU08            YES                                          
         ICM   R1,3,CT02KAGY                                                    
         LA    R1,1(,R1)                                                        
         XC    CT02KEY,CT02KEY                                                  
         MVI   CT02KTYP,CT02KTYQ                                                
         STCM  R1,3,CT02KAGY                                                    
         J     GETDRU02                                                         
                                                                                
GETDRU08 GOTOR PUTBUF,DMCB,('BUFDSRBD',CT02KAGY),CT02KEY,              *        
               ('BUFKTDRU',L'BUFKDRUS-1),0                                      
         MVC   P(L'DRULIT),DRULIT                                               
         LA    R1,P+L'DRULIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CT02KAGY,R1),CT02KAGY                                        
         MVI   1+L'CT02KAGY(R1),C')'                                            
         AHI   R1,L'CT02KAGY+2                                                  
         MVC   0(L'CT02KCOD,R1),CT02KCOD                                        
         AHI   R1,L'CT02KCOD-1                                                  
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C' '                                                       
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETDRU04                                                         
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* READ CPP EXTRACT RULES RECORDS AND POST TO BUFFER                   *         
***********************************************************************         
GETCPP   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON20),RECCON20                                           
         GOTOR VPRINTER                                                         
         LA    R2,DELETE           R2=A(DELETE LIST)                            
         J     GETCPP04                                                         
                                                                                
GETCPP02 CLI   0(R2),C' '          TEST END OF DELETE LIST                      
         JE    EXIT                                                             
                                                                                
         USING CTXREC,IO                                                        
GETCPP04 XC    CTXKEY,CTXKEY                                                    
         MVI   CTXKTYP,CTXKTYPQ                                                 
         MVC   CTXKAGY,0(R2)                                                    
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CTXKEY,CTXKEY                   
         J     GETCPP08                                                         
GETCPP06 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CTXKEY,CTXKEY                   
GETCPP08 JNE   GETCPP10                                                         
         CLI   CTXKTYP,CTXKTYPQ                                                 
         JNE   GETCPP10                                                         
         CLC   CTXKAGY,0(R2)                                                    
         JNE   GETCPP10                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CTXKAGY),CTXKEY,                *        
               ('BUFKTCPP',L'BUFKCPPX-1),0                                      
         MVC   P(L'CPPLIT),CPPLIT                                               
         LA    R1,P+L'CPPLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CTXKAGY,R1),CTXKAGY                                          
         MVI   1+L'CTXKAGY(R1),C')'                                             
         AHI   R1,L'CTXKAGY+2                                                   
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,CTXKCLT,(R0),CTXLEN-CTXKCLT,HEXTOG                  
         LR    R1,R0                                                            
         AHI   R1,(CTXLEN-CTXKCLT)*2                                            
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETCPP06                                                         
                                                                                
GETCPP10 AHI   R2,L'CT5KALPH+1     BUMP TO NEXT AGENCY                          
         J     GETCPP02                                                         
*&&                                                                             
***********************************************************************         
* READ WRITER DEFINITION RECORDS AND POST TO BUFFER                   *         
***********************************************************************         
GETWRI   NTR1  LABEL=NO                                                         
                                                                                
         MVC   P(L'RECCON21),RECCON21                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING CT$RECD,IO                                                       
         XC    CT$KEY,CT$KEY                                                    
         MVI   CT$KTYPE,CT$KTYPQ                                                
GETWRI02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),CTFILE,CT$KEY,CT$KEY                   
         J     GETWRI06                                                         
GETWRI04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),CTFILE,CT$KEY,CT$KEY                   
GETWRI06 JNE   *+2                                                              
         CLI   CT$KTYPE,CT$KTYPQ                                                
         JNE   EXIT                                                             
         GOTOR TSTDELA,CT$KAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETWRI08            YES                                          
         ICM   R1,3,CT$KAGY                                                     
         LA    R1,1(,R1)                                                        
         XC    CT$KEY,CT$KEY                                                    
         MVI   CT$KTYPE,CT$KTYPQ                                                
         STCM  R1,3,CT$KAGY                                                     
         J     GETWRI02                                                         
                                                                                
GETWRI08 GOTOR PUTBUF,DMCB,('BUFDSRBD',CT$KAGY),CT$KEY,                *        
               ('BUFKTWRI',L'BUFKWRID-1),0                                      
         MVC   P(L'WRILIT),WRILIT                                               
         LA    R1,P+L'WRILIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'CT$KAGY,R1),CT$KAGY                                          
         MVI   1+L'CT$KAGY(R1),C')'                                             
         AHI   R1,L'CT$KAGY+2                                                   
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,CT$KSYS,(R0),CT$KNAME-CT$KSYS,HEXTOG                
         LR    R1,R0                                                            
         AHI   R1,(CT$KNAME-CT$KSYS)*2                                          
         MVI   0(R1),C'/'                                                       
         MVC   1(L'CT$KNAME,R1),CT$KNAME                                        
         AHI   R1,L'CT$KNAME                                                    
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C' '                                                       
         MVC   2(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETWRI04                                                         
         EJECT                                                                  
***********************************************************************         
* READ RFP GROUP RECORDS AND POST TO BUFFER                           *         
***********************************************************************         
                                                                                
GETRFP   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN01),RECGEN01                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GRPKEYD,IO                                                       
         XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
GETRFP02 GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GRPKEY,GRPKEY                   
         J     GETRFP06                                                         
GETRFP04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GRPKEY,GRPKEY                   
GETRFP06 JNE   *+2                                                              
         CLI   GRPKSYS,GRPKSYSQ                                                 
         JNE   EXIT                                                             
         CLI   GRPKSTYP,GRPKSTYQ                                                
         JNE   EXIT                                                             
         GOTOR TSTDELA,GRPKAGY     TEST THIS AGENCY TO BE DELETED               
         JE    GETRFP08            YES                                          
         GOTOR TSTDELU,GRPKUSER    TEST THIS USER TO BE DELETED                 
         JE    GETRFP08            YES                                          
         ICM   R0,7,GRPKSYST       PRESERVE SYS+AGY                             
         ICM   R1,3,GRPKUSER       BUMP USER ID                                 
         LA    R1,1(,R1)                                                        
         XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         STCM  R0,7,GRPKSYST                                                    
         STCM  R1,3,GRPKUSER                                                    
         J     GETRFP02                                                         
                                                                                
GETRFP08 GOTOR PUTBUF,DMCB,('BUFDSRBD',GRPKSYST),GRPKEY,               *        
               ('BUFKTRFP',L'BUFKRFPG-1),0                                      
         MVC   P(L'RFPLIT),RFPLIT                                               
         LA    R1,P+L'RFPLIT+1                                                  
         MVC   0(L'GRPKSYST,R1),GRPKSYST                                        
         MVI   L'GRPKSYST(R1),C'/'                                              
         AHI   R1,L'GRPKSYST+1                                                  
         MVC   0(L'GRPKAGY,R1),GRPKAGY                                          
         AHI   R1,L'GRPKAGY                                                     
         MVI   0(R1),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,GRPKUSER                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         MVI   0(R1),C'/'                                                       
         MVC   1(L'GRPKGRP,R1),GRPKGRP                                          
         AHI   R1,L'GRPKGRP                                                     
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
         AHI   R1,2                                                             
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GRPKELEM,(R0),L'GRPKELEM,HEXTOG                     
         LR    R1,R0                                                            
         AHI   R1,L'GRPKELEM*2                                                  
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETRFP04                                                         
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* READ NARRATIVE RECORDS AND POST TO BUFFER                           *         
***********************************************************************         
                                                                                
GETNAR   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN02),RECGEN02                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GNARD,IO                                                         
         XC    GNKEY,GNKEY                                                      
         MVI   GNKREC,GNKRECQ                                                   
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GNKEY,GNKEY                     
         J     GETNAR04                                                         
GETNAR02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GNKEY,GNKEY                     
GETNAR04 JNE   *+2                                                              
         CLI   GNKREC,GNKRECQ                                                   
         JNE   EXIT                                                             
                                                                                
         OC    GNKAGY,GNKAGY       DON'T DELETE 'ALL' ENTRY                     
         JZ    GETNAR02                                                         
         GOTOR TSTDELU,GNKAGY      TEST THIS AGENCY TO BE DELETED               
         JNE   GETNAR02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GNKAGY),GNKEY,                  *        
               ('BUFKTNAR',L'BUFKNARG-1),0                                      
         MVC   P(L'NARLIT),NARLIT                                               
         LA    R1,P+L'NARLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'TSTUIDAG,R1),TSTUIDAG                                        
         MVI   L'TSTUIDAG+1(R1),C')'                                            
         AHI   R1,L'TSTUIDAG+2                                                  
         SR    R0,R0                                                            
         ICM   R0,3,GNKAGY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(5,R1),DUB                                                      
         MVI   5(R1),C'/'                                                       
         AHI   R1,6                                                             
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GNKLANG,(R0),L'GNKLANG,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GNKLANG(R1),C'/'                                             
         AHI   R1,2*L'GNKLANG+1                                                 
         MVC   0(L'GNKATYP,R1),GNKATYP                                          
         AHI   R1,L'GNKATYP                                                     
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         OC    GNKCLI,GNKCLI                                                    
         JZ    *+10                                                             
         MVC   0(L'GNKCLI,R1),GNKCLI                                            
         AHI   R1,L'GNKCLI                                                      
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         SR    R0,R0                                                            
         ICM   R0,1,GNKPRO                                                      
         JZ    GETNAR20                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(3,R1),DUB                                                      
         AHI   R1,3                                                             
GETNAR20 MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         ICM   R0,3,GNKBCAT                                                     
         JZ    GETNAR22                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(5,R1),DUB                                                      
         AHI   R1,5                                                             
GETNAR22 MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETNAR02                                                         
*&&                                                                             
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* READ EXCHANGE RECORDS AND POST TO BUFFER                            *         
***********************************************************************         
                                                                                
GETEXC   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN03),RECGEN03                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GEXCD,IO                                                         
         XC    GEKAGY,GEKAGY                                                    
GETEXC02 ICM   R1,3,GEKAGY         NEXT AGY (AND SKIP OVER FT)                  
         LA    R1,1(,R1)                                                        
         XC    GEKEY,GEKEY                                                      
         MVI   GEKREC,GEKRECQ                                                   
         STCM  R1,3,GEKAGY                                                      
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GEKEY,GEKEY                     
         J     GETEXC06                                                         
GETEXC04 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GEKEY,GEKEY                     
GETEXC06 JNE   *+2                                                              
         CLI   GEKREC,GEKRECQ                                                   
         JNE   EXIT                                                             
         GOTOR TSTDELA,GEKAGY      TEST THIS AGENCY TO BE DELETED               
         JNE   GETEXC02                                                         
                                                                                
         CLI   GEKSYS,X'FE'        MAKE SURE DON'T DELETE FT RATES              
         JNL   GETEXC04            BUT SHOULD HAVE SKIPPED THEM                 
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GEKAGY),GEKEY,                  *        
               ('BUFKTEXC',L'BUFKEXCG-1),0                                      
         MVC   P(L'EXCLIT),EXCLIT                                               
         LA    R1,P+L'EXCLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'GEKAGY,R1),GEKAGY                                            
         MVI   L'GEKAGY+1(R1),C')'                                              
         AHI   R1,L'GEKAGY+2                                                    
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GEKSYS,(R0),L'GEKSYS,HEXTOG                         
         LR    R1,R0                                                            
         MVI   2*L'GEKSYS(R1),C'/'                                              
         AHI   R1,2*L'GEKSYS+1                                                  
         MVC   0(L'GEKCURF,R1),GEKCURF                                          
         MVI   L'GEKCURF(R1),C'/'                                               
         AHI   R1,L'GEKCURF+1                                                   
         MVC   0(L'GEKCURT,R1),GEKCURT                                          
         MVI   L'GEKCURT(R1),C'/'                                               
         AHI   R1,L'GEKCURT+1                                                   
         MVC   0(L'GEKCTYP,R1),GEKCTYP                                          
         MVI   L'GEKCTYP(R1),C'/'                                               
         AHI   R1,L'GEKCTYP+1                                                   
         CLI   GEKSYS,4                                                         
         JNE   GETEXC08                                                         
         MVC   0(L'GEKCLI,R1),GEKCLI                                            
         CLI   GEKCLI+1,X'40'                                                   
         JL    *+16                                                             
         MVI   L'GEKCLI(R1),C'/'                                                
         AHI   R1,L'GEKCLI+1                                                    
         J     GETEXC12                                                         
         SR    R0,R0                                                            
         ICM   R0,3,GEKCLI+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(4,R1),DUB                                                      
         MVI   5(R1),C'/'                                                       
         AHI   R1,5+1                                                           
         J     GETEXC12                                                         
GETEXC08 CLI   GEKSYS,6                                                         
         JNE   GETEXC10                                                         
         MVC   0(L'GEKACT,R1),GEKACT                                            
         MVI   L'GEKACT(R1),C'/'                                                
         AHI   R1,L'GEKACT+1                                                    
         J     GETEXC12                                                         
GETEXC10 MVC   0(L'GEKKEY,R1),GEKKEY                                            
         MVI   L'GEKKEY(R1),C'/'                                                
         AHI   R1,L'GEKKEY+1                                                    
GETEXC12 LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GEKPEND,(R0),4,HEXTOG                               
         LR    R1,R0                                                            
         ICM   R0,15,4(R1)                                                      
         MVI   4(R1),C'-'                                                       
         STCM  R0,15,5(R1)                                                      
         MVC   10(L'WILDEL,R1),WILDEL                                           
         GOTOR VPRINTER                                                         
         J     GETEXC04                                                         
*&&                                                                             
*&&US                                                                           
***********************************************************************         
* READ MEDIA OFFICE RECORDS AND POST TO BUFFER                        *         
***********************************************************************         
GETMOF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN04),RECGEN04                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING MOFRECD,IO                                                       
         XC    MOFKEY,MOFKEY                                                    
         MVI   MOFKTYP,MOFKTYPQ                                                 
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,MOFKEY,MOFKEY                   
         J     GETMOF04                                                         
GETMOF02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,MOFKEY,MOFKEY                   
GETMOF04 JNE   *+2                                                              
         CLI   MOFKMAJ,0           MAJOR SYSTEM (00)                            
         JNE   EXIT                                                             
         CLI   MOFKMIN,0           MINOR SYSTEM (00)                            
         JNE   EXIT                                                             
         CLI   MOFKTYP,MOFKTYPQ    C'O' MEDIA OFFICE RECORD                     
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,MOFKAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETMOF02                                                         
                                                                                
         LHI   R0,BUFDSRBD         MARK FOR DELETION                            
         CLI   MOFKSUB,MOFKS1Q     CHECK FOR PRIMARY KEY X'01'                  
         JE    *+8                                                              
         AHI   R0,BUFDSPSV         IF NOT, MARK AS A PASSIVE                    
                                                                                
         GOTOR PUTBUF,DMCB,((R0),MOFKAGY),MOFKEY,                      +        
               ('BUFKTMOF',L'BUFKMOFA-1),0                                      
         MVC   P(L'MOFLIT),MOFLIT                                               
         LA    R1,P+L'MOFLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'MOFKAGY,R1),MOFKAGY                                          
         MVI   L'MOFKAGY+1(R1),C')'                                             
         AHI   R1,L'MOFKAGY+2                                                   
                                                                                
         CLI   MOFKSUB,MOFKS1Q     CHECK FOR PRIMARY KEY X'01'                  
         JE    GETMOF06                                                         
                                                                                
         MVC   0(L'MOFK2OF,R1),MOFK2OF                                          
         MVI   L'MOFK2OF(R1),C'/'                                               
         AHI   R1,L'MOFK2OF+1                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,MOFKC1OF,(R0),L'MOFKC1OF,HEXTOG                     
         LR    R1,R0                                                            
         MVI   2*L'MOFKC1OF(R1),C'/'                                            
         AHI   R1,2*L'MOFKC1OF+1                                                
         J     GETMOF08                                                         
                                                                                
GETMOF06 LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,MOFK1OF,(R0),L'MOFK1OF,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'MOFK1OF(R1),C'/'                                             
         AHI   R1,2*L'MOFK1OF+1                                                 
                                                                                
         MVC   0(L'MOFKC2OF,R1),MOFKC2OF                                        
         MVI   L'MOFKC2OF(R1),C'/'                                              
         AHI   R1,L'MOFKC2OF+1                                                  
                                                                                
GETMOF08 MVC   0(L'MOFKSYS,R1),MOFKSYS                                          
         AHI   R1,L'MOFKSYS                                                     
                                                                                
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETMOF02                                                         
*&&                                                                             
***********************************************************************         
* READ CONTROL ESS BDE REFORM RECORDS                                 *         
***********************************************************************         
GETBDR   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN05),RECGEN05                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GBDRD,IO                                                         
         XC    GBDRKEY,GBDRKEY                                                  
         MVI   GBDRKREC,GBDRRECQ                                                
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GBDRKEY,GBDRKEY                 
         J     GETBDR04                                                         
GETBDR02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GBDRKEY,GBDRKEY                 
GETBDR04 JNE   *+2                                                              
         CLI   GBDRKMAJ,0                                                       
         JNE   EXIT                                                             
         CLI   GBDRKMIN,0                                                       
         JNE   EXIT                                                             
         CLI   GBDRKREC,GBDRRECQ                                                
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GBDRAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETBDR02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GBDRAGY),GBDRKEY,               *        
               ('BUFKTBDR',L'BUFKBDRG-1),0                                      
         MVC   P(L'BDRLIT),BDRLIT                                               
         LA    R1,P+L'BDRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GBDRAGY,R1),GBDRAGY                                          
         MVI   L'GBDRAGY+1(R1),C')'                                             
         AHI   R1,L'GBDRAGY+2                                                   
                                                                                
         MVC   0(L'GBDEID,R1),GBDEID                                            
         AHI   R1,L'GBDEID                                                      
                                                                                
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETBDR02                                                         
                                                                                
***********************************************************************         
* READ EXTRACT CONTROL RECORD (AGENCY)                                          
***********************************************************************         
GETXTA   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN06),RECGEN06                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GXTRD,IO                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAKRECQ     X'10' AGENCY EXTRACT RECORD                  
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GXKEY,GXKEY                     
         J     GETXTA04                                                         
GETXTA02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GXKEY,GXKEY                     
GETXTA04 JNE   *+2                                                              
         CLI   GXKMAJ,0                                                         
         JNE   EXIT                                                             
         CLI   GXKMIN,0                                                         
         JNE   EXIT                                                             
         CLI   GXKREC,GXAKRECQ     X'10' AGENCY EXTRACT RECORD                  
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GXAKAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETXTA02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GXAKAGY),GXKEY,                 *        
               ('BUFKTXTA',L'BUFKXTRA-1),0                                      
         MVC   P(L'XTRLIT),XTRLIT                                               
         LA    R1,P+L'XTRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GXAKAGY,R1),GXAKAGY                                          
         MVI   L'GXAKAGY+1(R1),C')'                                             
         AHI   R1,L'GXAKAGY+2                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXAKSYS,(R0),L'GXAKSYS,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXAKSYS(R1),C'/'                                             
         AHI   R1,2*L'GXAKSYS+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXAKSUB,(R0),L'GXAKSUB,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,2*L'GXAKSUB                                                   
                                                                                
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETXTA02                                                         
                                                                                
***********************************************************************         
* READ EXTRACT CONTROL RECORD (ESS SERVER)                                      
***********************************************************************         
GETXTS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN07),RECGEN07                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GXTRD,IO                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSKRECQ     X'20' ESS SERVER CONTROL RECORD              
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GXKEY,GXKEY                     
         J     GETXTS04                                                         
GETXTS02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GXKEY,GXKEY                     
GETXTS04 JNE   *+2                                                              
         CLI   GXKMAJ,0                                                         
         JNE   EXIT                                                             
         CLI   GXKMIN,0                                                         
         JNE   EXIT                                                             
         CLI   GXKREC,GXSKRECQ                                                  
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GXSKAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETXTS02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GXSKAGY),GXKEY,                 *        
               ('BUFKTXTS',L'BUFKXTRS-1),0                                      
         MVC   P(L'XTRLIT),XTRLIT                                               
         LA    R1,P+L'XTRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GXSKAGY,R1),GXSKAGY                                          
         MVI   L'GXSKAGY+1(R1),C')'                                             
         AHI   R1,L'GXSKAGY+2                                                   
                                                                                
         MVC   0(L'GXSKEID,R1),GXSKEID                                          
         MVI   L'GXSKEID(R1),C'/'                                               
         AHI   R1,L'GXSKEID+1                                                   
                                                                                
         MVC   0(L'GXSKAGY,R1),GXSKAGY                                          
         MVI   L'GXSKAGY(R1),C'/'                                               
         AHI   R1,L'GXSKAGY+1                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSKSYS,(R0),L'GXSKSYS,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXSKSYS(R1),C'/'                                             
         AHI   R1,2*L'GXSKSYS+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSKSUB,(R0),L'GXSKSUB,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,2*L'GXSKSUB                                                   
                                                                                
         MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETXTS02                                                         
                                                                                
***********************************************************************         
* READ EXTRACT CONTROL RECORD (FILE DEFINITION)                                 
***********************************************************************         
GETXTF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN08),RECGEN08                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GXTRD,IO                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXFKRECQ     X'30' EXTRACT FILE DEFINITION RECORD         
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GXKEY,GXKEY                     
         J     GETXTF04                                                         
GETXTF02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GXKEY,GXKEY                     
GETXTF04 JNE   *+2                                                              
         CLI   GXKMAJ,0                                                         
         JNE   EXIT                                                             
         CLI   GXKMIN,0                                                         
         JNE   EXIT                                                             
         CLI   GXKREC,GXFKRECQ                                                  
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GXFKAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETXTF02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GXFKAGY),GXKEY,                 *        
               ('BUFKTXTF',L'BUFKXTRF-1),0                                      
         MVC   P(L'XTRLIT),XTRLIT                                               
         LA    R1,P+L'XTRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GXFKAGY,R1),GXFKAGY                                          
         MVI   L'GXFKAGY+1(R1),C')'                                             
         AHI   R1,L'GXFKAGY+2                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXFKDAT,(R0),L'GXFKDAT,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXFKDAT(R1),C'/'                                             
         AHI   R1,2*L'GXFKDAT+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXFKTIM,(R0),L'GXFKTIM,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXFKTIM(R1),C'/'                                             
         AHI   R1,2*L'GXFKTIM+1                                                 
                                                                                
         MVC   0(L'GXFKAGY,R1),GXFKAGY                                          
         MVI   L'GXFKAGY(R1),C'/'                                               
         AHI   R1,L'GXFKAGY+1                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXFKSYS,(R0),L'GXFKSYS,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXFKSYS(R1),C'/'                                             
         AHI   R1,2*L'GXFKSYS+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXFKSUB,(R0),L'GXFKSUB,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,2*L'GXFKSUB                                                   
                                                                                
GETXTF22 MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETXTF02                                                         
                                                                                
***********************************************************************         
* READ EXTRACT CONTROL ESS SERVER / AGENCY PASSIVE RECORDS                      
***********************************************************************         
GETXSA   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN09),RECGEN09                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GXTRD,IO                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSARECQ     X'21' ESS SERVER / AGENCY PASSIVE            
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GXKEY,GXKEY                     
         J     GETXSA04                                                         
GETXSA02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GXKEY,GXKEY                     
GETXSA04 JNE   *+2                                                              
         CLI   GXKMAJ,0                                                         
         JNE   EXIT                                                             
         CLI   GXKMIN,0                                                         
         JNE   EXIT                                                             
         CLI   GXKREC,GXSARECQ                                                  
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GXSAAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETXSA02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD+BUFDSPSV',GXSAAGY),GXKEY,        *        
               ('BUFKTXSA',L'BUFKXTSA-1),0                                      
         MVC   P(L'XTRLIT),XTRLIT                                               
         LA    R1,P+L'XTRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GXSAAGY,R1),GXSAAGY                                          
         MVI   L'GXSAAGY+1(R1),C')'                                             
         AHI   R1,L'GXSAAGY+2                                                   
                                                                                
         MVC   0(L'GXSAAGY,R1),GXSAAGY                                          
         MVI   L'GXSAAGY(R1),C'/'                                               
         AHI   R1,L'GXSAAGY+1                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSASYS,(R0),L'GXSASYS,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,2*L'GXSASYS                                                   
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSASUB,(R0),L'GXSASUB,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,2*L'GXSASUB                                                   
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
                                                                                
         MVC   0(L'GXSAEID,R1),GXSAEID                                          
         MVI   L'GXSAEID(R1),C'/'                                               
         AHI   R1,L'GXSAEID+1                                                   
                                                                                
         MVC   0(L'GXSAAPID,R1),GXSAAPID                                        
         AHI   R1,L'GXSAAPID                                                    
                                                                                
GETXSA22 MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETXSA02                                                         
                                                                                
***********************************************************************         
* READ EXTRACT CONTROL ESS AGENCY / FILE PASSIVE RECORDS                        
***********************************************************************         
GETXAF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN10),RECGEN10                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GXTRD,IO                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAFRECQ     X'31' ESS AGENCY / FILE PASSIVE              
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GXKEY,GXKEY                     
         J     GETXAF04                                                         
GETXAF02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GXKEY,GXKEY                     
GETXAF04 JNE   *+2                                                              
         CLI   GXKMAJ,0                                                         
         JNE   EXIT                                                             
         CLI   GXKMIN,0                                                         
         JNE   EXIT                                                             
         CLI   GXKREC,GXAFRECQ                                                  
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GXAFAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETXAF02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD+BUFDSPSV',GXAFAGY),GXKEY,        *        
               ('BUFKTXAF',L'BUFKXTAF-1),0                                      
         MVC   P(L'XTRLIT),XTRLIT                                               
         LA    R1,P+L'XTRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GXAFAGY,R1),GXAFAGY                                          
         MVI   L'GXAFAGY+1(R1),C')'                                             
         AHI   R1,L'GXAFAGY+2                                                   
                                                                                
         MVC   0(L'GXAFAGY,R1),GXAFAGY                                          
         MVI   L'GXAFAGY(R1),C'/'                                               
         AHI   R1,L'GXAFAGY+1                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXAFSYS,(R0),L'GXAFSYS,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXAFSYS(R1),C'/'                                             
         AHI   R1,2*L'GXAFSYS+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXAFSUB,(R0),L'GXAFSUB,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXAFSUB(R1),C'/'                                             
         AHI   R1,2*L'GXAFSUB+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXAFDAT,(R0),L'GXAFDAT,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXAFDAT(R1),C'/'                                             
         AHI   R1,2*L'GXAFDAT+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXAFTIM,(R0),L'GXAFTIM,HEXTOG                       
         LR    R1,R0                                                            
         AHI   R1,2*L'GXAFTIM                                                   
                                                                                
GETXAF22 MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETXAF02                                                         
                                                                                
***********************************************************************         
* READ EXTRACT CONTROL ESS ID / FILE PASSIVE RECORDS                            
***********************************************************************         
GETXSF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN11),RECGEN11                                           
         GOTOR VPRINTER                                                         
                                                                                
         USING GXTRD,IO                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSFRECQ     X'32' ESS / FILE PASSIVE                     
         GOTOR VDMGR,DMCB,(DMIND,DMRDHI),GENDIR,GXKEY,GXKEY                     
         J     GETXSF04                                                         
GETXSF02 GOTOR VDMGR,DMCB,(DMIND,DMRSEQ),GENDIR,GXKEY,GXKEY                     
GETXSF04 JNE   *+2                                                              
         CLI   GXKMAJ,0                                                         
         JNE   EXIT                                                             
         CLI   GXKMIN,0                                                         
         JNE   EXIT                                                             
         CLI   GXKREC,GXSFRECQ                                                  
         JNE   EXIT                                                             
                                                                                
         GOTOR TSTDELA,GXSFAGY     TEST THIS AGENCY TO BE DELETED               
         JNE   GETXSF02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD+BUFDSPSV',GXSFAGY),GXKEY,        *        
               ('BUFKTXSF',L'BUFKXTSF-1),0                                      
         MVC   P(L'XTRLIT),XTRLIT                                               
         LA    R1,P+L'XTRLIT+1                                                  
                                                                                
         MVI   0(R1),C'('                                                       
         MVC   1(L'GXSFAGY,R1),GXSFAGY                                          
         MVI   L'GXSFAGY+1(R1),C')'                                             
         AHI   R1,L'GXSFAGY+2                                                   
                                                                                
         MVC   0(L'GXSFEID,R1),GXSFEID                                          
         MVI   L'GXSFEID(R1),C'/'                                               
         AHI   R1,L'GXSFEID+1                                                   
                                                                                
         MVC   0(L'GXSFAGY,R1),GXSFAGY                                          
         MVI   L'GXSFAGY(R1),C'/'                                               
         AHI   R1,L'GXSFAGY+1                                                   
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSFSYS,(R0),L'GXSFSYS,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXSFSYS(R1),C'/'                                             
         AHI   R1,2*L'GXSFSYS+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSFSUB,(R0),L'GXSFSUB,HEXTOG                       
         LR    R1,R0                                                            
         MVI   2*L'GXSFSUB(R1),C'/'                                             
         AHI   R1,2*L'GXSFSUB+1                                                 
                                                                                
         LR    R0,R1                                                            
         GOTOR VHEXOUT,DMCB,GXSFFNUM,(R0),L'GXSFFNUM,HEXTOG                     
         LR    R1,R0                                                            
         AHI   R1,2*L'GXSFFNUM                                                  
                                                                                
GETXSF22 MVC   1(L'WILDEL,R1),WILDEL                                            
         GOTOR VPRINTER                                                         
         J     GETXSF02                                                         
                                                                                
***********************************************************************         
* TEST IF AGENCY ALPHA IS A CANDIDATE FOR DELETION                    *         
***********************************************************************         
TSTDELA  NTR1  LABEL=NO                                                         
                                                                                
         CLC   TSTAGY,0(R1)                                                     
         MVC   TSTAGY,0(R1)                                                     
         JNE   TSTDELA1                                                         
         CLI   TSTAGYKD,C'D'                                                    
         JE    TSTDELAD                                                         
         J     TSTDELAK                                                         
                                                                                
TSTDELA1 CLI   MODE,C'O'                                                        
         JE    TSTDELA3                                                         
                                                                                
         LA    RE,DELETE           DELETE MODE, SCAN ALPHA ID LIST              
TSTDELA2 CLI   0(RE),C' '                                                       
         JE    TSTDELAK            DONT DELETE IF NOT IN DELETE LIST            
         CLC   0(L'CTAGYID,R1),0(RE)                                            
         JE    TSTDELAD                                                         
         AHI   RE,L'CTAGYID+1                                                   
         J     TSTDELA2                                                         
                                                                                
TSTDELA3 XC    BUFKEY(BUFKEYL),BUFKEY ORPHAN MODE, SEE IF ALPHA IN BUFF         
         MVI   BUFKTYPE,BUFKTACS                                                
         MVC   BUFKAGYA,0(R1)                                                   
         GOTOR BUFGET                                                           
         JNE   TSTDELAD            NOT FOUND, DELETE RECORD                     
                                                                                
TSTDELAK MVI   TSTAGYKD,C'K'                                                    
         LTR   RD,RD               KEEP   (CC NE/NZ)                            
         J     EXIT                                                             
TSTDELAD MVI   TSTAGYKD,C'D'                                                    
         CR    RD,RD               DELETE (CC EQ/ZERO)                          
         J     EXIT                                                             
                                                                                
***********************************************************************         
* TEST IF USER ID IS A CANDIDATE FOR DELETION                         *         
***********************************************************************         
TSTDELU  NTR1  LABEL=NO                                                         
                                                                                
         CLC   TSTUID,0(R1)                                                     
         MVC   TSTUID,0(R1)                                                     
         JNE   TSTDELU1                                                         
         CLI   TSTUIDKD,C'D'                                                    
         JE    TSTDELUD                                                         
         J     TSTDELUK                                                         
                                                                                
TSTDELU1 XC    BUFKEY(BUFKEYL),BUFKEY SEE IF USERID IN BUFF                     
         MVI   BUFKTYPE,BUFKTID#                                                
         MVC   BUFKUSID,0(R1)                                                   
         GOTOR BUFGET                                                           
         JNE   TSTDELU2            NOT FOUND, SKIP                              
                                                                                
         MVC   TSTUIDAG,BUFDAGY    FOUND, SAVE ALPHA ID                         
         CLI   MODE,C'O'           TEST ORPHAN MODE                             
         JNE   TSTDELUD            NO, DELETE                                   
         TM    BUFDSTAT,BUFDSRBD   YES, IS USERID TO BE DELETED                 
         JNZ   TSTDELUD            YES, DELETE                                  
         J     TSTDELUK            ELSE KEEP                                    
                                                                                
TSTDELU2 MVC   TSTUIDAG,=C'  '     NOT FOUND, SET UNKNOWN ALPHA                 
         CLI   MODE,C'O'           TEST ORPHAN MODE                             
         JNE   TSTDELUK            NO, KEEP                                     
                                                                                
TSTDELUD MVI   TSTUIDKD,C'D'                                                    
         CR    RD,RD               DELETE (CC EQ/ZERO)                          
         J     EXIT                                                             
TSTDELUK MVI   TSTUIDKD,C'K'                                                    
         LTR   RD,RD               KEEP   (CC NE/NZ)                            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUFFERIN ROUTINES                                                   *         
***********************************************************************         
                                                                                
PUTBUF   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         XC    BUFREC(BUFRECL),BUFREC                                           
         MVC   BUFKTYPE,8(R2)                                                   
         SR    RF,RF                                                            
         ICM   RF,7,9(R2)                                                       
         L     R1,0(R2)                                                         
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   BUFKDATA(0),0(R1)                                                
         MVC   BUFDSTAT,0(R2)      SET RECORD STATUS                            
         ICM   RE,15,12(R2)                                                     
         JZ    *+10                                                             
         MVC   BUFDAGY,0(RE)                                                    
         ICM   RE,15,4(R2)                                                      
         JZ    PUTBUF02                                                         
         CLI   BUFKTYPE,BUFKTGEN   TEST FILE TYPE                               
         JH    *+14                                                             
         MVC   BUFDRKEY(L'CTIKEY),0(RE)                                         
         J     PUTBUF02                                                         
         MVC   BUFDRKEY(L'GRPKEY),0(RE)                                         
PUTBUF02 GOTOR BUFPUT                                                           
         J     EXIT                                                             
                                                                                
BUFGET   LR    R0,RE                                                            
         GOTOR VBUFFRIN,PARM,('BUFFAGET',BUFFER),BUFREC,COMFACS                 
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
BUFPUT   LR    R0,RE                                                            
         GOTOR VBUFFRIN,PARM,('BUFFAPUT',BUFFER),BUFREC,COMFACS                 
         LR    RE,R0                                                            
         BER   RE                                                               
         DC    H'0'                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
***********************************************************************         
                                                                                
VALPAR   NTR1  BASE=*,LABEL=*                                                   
         XC    RUNVALS(RUNVALL),RUNVALS                                         
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'TITLE1),TITLE1                                           
         ZAP   LINE,PNINETY                                                     
         ZAP   PAGE,PONE                                                        
         MVI   CTFILEW,C'N'                                                     
         MVI   GENFILW,C'N'                                                     
         MVI   GENDIRW,C'N'                                                     
         MVI   CTRCVRW,C'X'                                                     
                                                                                
VALPAR02 GOTOR VCARDS,DMCB,C,RE00                                               
         MVC   P(L'C),C                                                         
         GOTOR VPRINTER                                                         
         LA    R1,PARAMS           R1=A(PARAMETER TABLE)                        
         SR    RE,RE                                                            
VALPAR04 ICM   RE,1,0(R1)          TEST FOR END OF TABLE                        
         BZ    VALPAR08            YES - NOT A VALID CONTROL CARD               
         EX    RE,*+8              MATCH CARD DATA TO TABLE ENTRY               
         BE    VALPAR06                                                         
         CLC   C(0),4(R1)                                                       
         AHI   R1,L'PARAMS         BUMP TO NEXT TABLE ENTRY                     
         B     VALPAR04                                                         
                                                                                
VALPAR06 SR    RF,RF               PROCESS PARAMETER CARD                       
         ICM   RF,7,1(R1)          RF=A(PROCESS/VALIDATION ROUTINE)             
         LA    R1,C+1(RE)          R1=A(DATA VALUE)                             
         GOTOR (RF),(R1)           CALL PROCESS/VALIDATE ROUTINE                
         B     VALPAR02                                                         
                                                                                
VALPAR08 MVC   P(L'ERROR3),ERROR3                                               
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
                                                                                
VALPAR10 CLI   MODE,C'O'                                                        
         BE    VALPAR12                                                         
                                                                                
         CLI   DELETE,C' '                                                      
         BH    VALPAR12                                                         
         MVC   P(L'ERROR1),ERROR1  DELETE CARD MISSING                          
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
                                                                                
VALPAR12 CLI   OUTPUT,C' '                                                      
         BH    VALPAR14                                                         
         MVC   P(L'ERROR4),ERROR4  OUTPUT CARD MISSING                          
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
                                                                                
VALPAR14 CLI   OUTPUT,C'D'                                                      
         BE    VALPAR16                                                         
         OPEN  (CONTAPE,OUTPUT,GENTAPE,OUTPUT)                                  
         CLI   OUTPUT,C'T'                                                      
         BNE   VALPAR16                                                         
         CLI   WRITE,YES                                                        
         BNE   VALPAR16                                                         
         MVI   WRITE,NO                                                         
         MVI   CTFILEW,C'N'                                                     
         MVI   GENFILW,C'N'                                                     
         MVI   GENDIRW,C'N'                                                     
         MVC   P(L'WARN1),WARN1    WRITE=Y IGNORED                              
         GOTOR VPRINTER                                                         
VALPAR16 CLI   WRITE,C' '                                                       
         BH    *+8                                                              
         MVI   WRITE,NO                                                         
         CLI   TRACE,C' '                                                       
         BH    *+8                                                              
         MVI   TRACE,C'N'                                                       
         J     EXIT                                                             
         DROP  RB                                                               
                                                                                
PARAMS   DS    0XL24               ** TABLE OF PARAMETER CARDS **               
                                                                                
         DC    AL1(05),AL3(PARDDIO),CL20'DDSIO='                                
         DC    AL1(06),AL3(PARDSPA),CL20'DSPACE='                               
         DC    AL1(05),AL3(PARTRAC),CL20'TRACE='                                
         DC    AL1(07),AL3(PARRCVR),CL20'RECOVER='                              
         DC    AL1(08),AL3(PARRCVR),CL20'RECOVERY='                             
         DC    AL1(05),AL3(PARWRIT),CL20'WRITE='                                
         DC    AL1(06),AL3(PARDELS),CL20'DELETE='                               
         DC    AL1(04),AL3(UNDODEL),CL20'UNDO='                                 
         DC    AL1(06),AL3(PAROUTP),CL20'OUTPUT='                               
         DC    AL1(05),AL3(FORCEP),CL20'FORCE='                                 
         DC    AL1(01),AL3(VALPAR10),CL20'/*'                                   
                                                                                
PARAMSX  DC    AL1(0)                                                           
                                                                                
***********************************************************************         
* PARAMETER VALIDATION ROUTINES                                       *         
***********************************************************************         
PARDDIO  L     RF,VDDSIO           DDSIO=                                       
         MVC   0(8,RF),0(R1)                                                    
         BR    RE                                                               
                                                                                
PARDSPA  MVC   SSODSPAC,0(R1)      DSPACE=                                      
         BR    RE                                                               
                                                                                
PARTRAC  MVC   TRACE,0(R1)         TRACE=                                       
         BR    RE                                                               
                                                                                
PARDELS  MVC   DELETE,0(R1)        DELETE=                                      
         MVI   MODE,C'L'            =A1,A2,A3,...ETC                            
         CLC   DELETE(6),=C'ORPHAN'                                             
         BNER  RE                                                               
         MVI   MODE,C'O'            =ORPHAN                                     
         XC    DELETE,DELETE                                                    
         BR    RE                                                               
                                                                                
PAROUTP  MVC   OUTPUT,0(R1)        OUTPUT=                                      
         CLI   0(R1),C'D'           =DISK                                       
         BER   RE                                                               
         CLI   0(R1),C'T'           =TAPE                                       
         BER   RE                                                               
         CLI   0(R1),C'B'           =BOTH                                       
         BER   RE                                                               
         J     VALPAR08                                                         
                                                                                
PARWRIT  MVC   WRITE,0(R1)         WRITE=                                       
         CLI   WRITE,NO                                                         
         BER   RE                                                               
         MVI   CTFILEW,C'U'                                                     
         MVI   GENFILW,C'U'                                                     
         MVI   GENDIRW,C'U'                                                     
         OI    SSOSTAT2,SSOSGALO+SSOSLOCK                                       
         CLI   WRITE,YES            =Y IF LIVE RUN                              
         BER   RE                                                               
         J     VALPAR08                                                         
                                                                                
PARRCVR  OI    SSOSTAT2,SSOSNRCV   RECOVERY=                                    
         CLI   0(R1),C'N'                                                       
         BER   RE                                                               
         MVI   CTRCVRW,C'U'                                                     
         NI    SSOSTAT2,X'FF'-(SSOSNRCV+SSOSROLC)                               
         CLI   0(R1),C'Y'                                                       
         BER   RE                                                               
         OI    SSOSTAT2,SSOSROLC                                                
         CLI   0(R1),C'C'          TEST RECOVER COPIES TOO                      
         BER   RE                                                               
         CLI   0(R1),C'F'          TEST FULL RECOVERY                           
         JNE   VALPAR08                                                         
         OI    SSOFLAG1,SSOFRCVR                                                
         BR    RE                                                               
                                                                                
UNDODEL  MVC   UNDO,0(R1)          UNDO=Y/N                                     
         CLI   UNDO,C'N'                                                        
         BER   RE                                                               
         MVC   WILDEL,WILRES       Change message                               
         MVI   DMIND,X'08'         read for deleted records                     
         BR    RE                                                               
                                                                                
FORCEP   MVC   FORCE,0(R1)         Force delete of records w/ warnings          
         BR    RE                                                               
                                                                                
***********************************************************************         
* Constants and variables                                                       
***********************************************************************         
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
LITERALS DS    0D                                                               
                                                                                
         LTORG                                                                  
                                                                                
COMFACS  DS    0F                                                               
VDMGR    DC    V(DATAMGR)                                                       
                                                                                
VBUFFRIN DC    V(BUFFERIN)                                                      
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDDSIO   DC    V(DDSIO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINTER DC    V(PRINTER)                                                       
                                                                                
AIDWORK  DC    A(IDWORK)                                                        
                                                                                
CONTAPE  DCB   DSORG=PS,MACRF=(PM),RECFM=VB,BLKSIZE=27648,LRECL=8200,  X        
               DDNAME=CONTAPE                                                   
                                                                                
GENTAPE  DCB   DSORG=PS,MACRF=(PM),RECFM=VB,BLKSIZE=27648,LRECL=8200,  X        
               DDNAME=GENTAPE                                                   
                                                                                
DMOPEN   DC    C'OPEN    '                                                      
DMCLSE   DC    C'DMCLSE  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMGETREC DC    C'GETREC  '                                                      
DMPUTREC DC    C'PUTREC  '                                                      
DMCONSYS DC    C'CONTROL '                                                      
DMCOMMIT DC    C'COMMIT  '                                                      
                                                                                
DMFLIST  DS    0C                                                               
CTFILEW  DC    C'?'                                                             
CTFILE   DC    C'CTFILE '                                                       
GENDIRW  DC    C'?'                                                             
GENDIR   DC    C'GENDIR '                                                       
GENFILW  DC    C'?'                                                             
GENFIL   DC    C'GENFIL '                                                       
CTRCVRW  DC    C'?'                                                             
CTRCVR   DC    C'CTRCVR '                                                       
         DC    C'X'                                                             
                                                                                
RECCON01 DC    C'Reading Access records (CT5REC)'                               
RECCON02 DC    C'Reading ID records (CTIREC)'                                   
RECCON03 DC    C'Reading List records (CTWREC)'                                 
RECCON04 DC    C'Reading User Profile records (CTUREC)'                         
RECCON05 DC    C'Reading Report Profile records (CTPREC)'                       
RECCON06 DC    C'Reading Password records (CT0REC)'                             
RECCON07 DC    C'Reading Access records (SAASSUBQ)'                             
RECCON08 DC    C'Reading Field Control records (SAFCSUBQ)'                      
RECCON09 DC    C'Reading Option Control records (SAOCSUBQ)'                     
RECCON10 DC    C'Reading Office records (SAOFSUBQ)'                             
RECCON11 DC    C'Reading Department records (SADPSUBQ)'                         
RECCON12 DC    C'Reading Person records (SAPESUBQ)'                             
RECCON13 DC    C'Reading Person Access Group records (SAAGSUBQ)'                
RECCON14 DC    C'Reading Limit Access Group records (SALASUBQ)'                 
RECCON15 DC    C'Reading Limit Access List records (SALMSUBQ)'                  
RECCON16 DC    C'Reading Time Sheet Approver records (SAAPSUBQ)'                
RECCON17 DC    C'Reading FAX Information records (CTFXREC)'                     
RECCON19 DC    C'Reading DRIVER User records (CT02REC)'                         
RECCON20 DC    C'Reading CPP Extract Rules records (CTXREC)'                    
RECCON21 DC    C'Reading Writer Definition records (CT$REC)'                    
                                                                                
RECGEN01 DC    C'Reading RFP Group records (GRPKSTYQ)'                          
RECGEN02 DC    C'Reading Narrative records (GNKRECQ)'                           
RECGEN03 DC    C'Reading Exchange records (GEKRECQ)'                            
RECGEN04 DC    C'Reading Media Office records (GEGENOFF)'                       
RECGEN05 DC    C'Reading ESS BDE Reform records (GEGENBDR)'                     
RECGEN06 DC    C'Reading Extract Control Agency records (GXAKRECQ)'             
RECGEN07 DC    C'Reading Extract Control ESS records (GXSKRECQ)'                
RECGEN08 DC    C'Reading Extract Control File records (GXFKRECQ)'               
RECGEN09 DC    C'Reading Extract Passive ESS Agy records (GXSARECQ)'            
RECGEN10 DC    C'Reading Extract Passive Agy File records (GXAFRECQ)'           
RECGEN11 DC    C'Reading Extract Passive ESS File records (GXSFRECQ)'           
                                                                                
ACSLIT   DC    C'Access record'                                                 
UIDLIT   DC    C'User-ID record'                                                
LSTLIT   DC    C'List record'                                                   
UPRLIT   DC    C'User Profile record'                                           
RPRLIT   DC    C'Report Profile record'                                         
PW#LIT   DC    C'Password Number record'                                        
PWCLIT   DC    C'Password Code record'                                          
PWNLIT   DC    C'Password Name record'                                          
SACLIT   DC    C'Security Access record'                                        
SAFLIT   DC    C'Field Control record'                                          
SAOLIT   DC    C'Option Control record'                                         
OFFLIT   DC    C'Office record'                                                 
DEPLIT   DC    C'Department record'                                             
PERLIT   DC    C'Person record'                                                 
PAGLIT   DC    C'Person Access Group record'                                    
LAGLIT   DC    C'Limit Access Group record'                                     
LALLIT   DC    C'Limit Access List record'                                      
TSALIT   DC    C'Time Sheet Approver record'                                    
RFPLIT   DC    C'RFP Group record'                                              
NARLIT   DC    C'Narrative record'                                              
EXCLIT   DC    C'Exchange record'                                               
FAXLIT   DC    C'FAX Information record'                                        
DRULIT   DC    C'DRIVER User record'                                            
CPPLIT   DC    C'CPP Extract Rules record'                                      
WRILIT   DC    C'Writer Definition record'                                      
TRMLIT   DC    C'Terminal record'                                               
MOFLIT   DC    C'Media Office record'                                           
BDRLIT   DC    C'ESS BDE Reform record'                                         
XTRLIT   DC    C'Extract Control record'                                        
                                                                                
TITLE1   DC    C'Control Cards'                                                 
TITLE2   DC    C'Agency Deleter Trace'                                          
                                                                                
ERROR1   DC    C'DELETE= card missing - Run aborted'                            
ERROR2   DC    C'Invalid Agency Alpha-ID='                                      
ERROR3   DC    C'Invalid control card'                                          
ERROR4   DC    C'OUTPUT=DISK/TAPE/BOTH card missing - Run aborted'              
                                                                                
WARN1    DC    C'WRITE=YES ignored when OUTPUT=TAPE'                            
                                                                                
WILDEL   DC    C'will be deleted '                                              
WILRES   DC    C'will be restored'                                              
WILKEPT  DC    C'will be kept'                                                  
ALLLIT   DC    C'All'                                                           
UPDLIT   DC    C'Updating files for real'                                       
GETLIT   DC    C'Reading BUFFERIN file to check file keys'                      
DAPALIT  DC    C'DARE Partner Code found'                                       
DAPRLIT  DC    C'DARE Prefix Record found'                                      
                                                                                
CTFILELD DC    C'Total CTFILE records='                                         
CTFILED  DC    PL4'0'                                                           
GENDIRLD DC    C'Total GENDIR records='                                         
GENDIRD  DC    PL4'0'                                                           
GENFILLD DC    C'Total GENFIL records='                                         
GENFILD  DC    PL4'0'                                                           
                                                                                
COMTALLY DC    PL3'0'                                                           
COMLIMIT DC    PL3'100'                                                         
                                                                                
PONE     DC    P'1'                                                             
PNINETY  DC    P'90'                                                            
RE00     DC    C'RE00'                                                          
HEXTOG   DC    C'TOG'                                                           
                                                                                
BUFFER   BUFFD TYPE=D,KEYLEN=BUFKEYL,COMLEN=BUFDTAL,BUFFERS=200                 
                                                                                
***********************************************************************         
* SSB and ULT. Entry points for DMDMGRL (connection to DDSIO)                   
***********************************************************************         
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB*SSB*SSB*SSB*SSB'                              
SSB      DS    0F                                                               
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENED SSB                              
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET NO RECOVERY                              
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'               CONTROL                                      
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'*IDWORK**IDWORK**IDWORK**IDWORK*'                              
IDWORKMX EQU   1000                                                             
IDWORK   DS    (IDWORKMX)XL12      LIST OF DESTINATION/COMPATIBLE IDS           
                                                                                
         DC    C'**WORKC***WORKC***WORKC***WORKC*'                              
WORKC    DS    (200*K)X                                                         
         EJECT                                                                  
WORKD    DSECT                     ** WORK AREA **                              
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    XL256                                                            
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
EXITCC   DS    X                                                                
                                                                                
FLAG     DS    X                                                                
FLAGDAPA EQU   X'80'               Dare Partner Code Found                      
FLAGDAPR EQU   X'40'               Dare Prefix Record Found                     
                                                                                
TSTDELS  DS    0X                                                               
TSTAGY   DS    CL2                                                              
TSTAGYKD DS    C                                                                
TSTUID   DS    XL2                                                              
TSTUIDAG DS    CL2                                                              
TSTUIDKD DS    C                                                                
TSTDELL  EQU   *-TSTDELS                                                        
                                                                                
RUNVALS  DS    0X                  ** RUN TIME PARAMETERS **                    
DELETE   DS    CL72                                                             
MODE     DS    C                                                                
OUTPUT   DS    C                                                                
TRACE    DS    C                                                                
WRITE    DS    C                                                                
UNDO     DS    C                   Default is NO                                
FORCE    DS    C                   Default is NO                                
DMIND    DS    X                                                                
RUNVALL  EQU   *-RUNVALS                                                        
                                                                                
BUFREC   DS    0X                  ** BUFFERIN RECORD **                        
                                                                                
BUFKEY   DS    0X                  ** RECORD KEY **                             
                                                                                
BUFKTYPE DS    X                   RECORD TYPE                                  
BUFKTCON EQU   0                   BASE NUMBER FOR CTFILE RECORDS               
BUFKTGEN EQU   128                 BASE NUMBER FOR GENFIL RECORDS               
                                                                                
BUFKDATA DS    0X                                                               
                                                                                
BUFKTACS EQU   BUFKTCON+1          ACCESS RECORD                                
BUFKAGYA DS    CL(L'CT5KALPH)                                                   
                                                                                
         ORG   BUFKDATA                                                         
BUFKTID# EQU   BUFKTCON+2          USER-ID NUMBER                               
BUFKUSID DS    XL(L'CTIKNUM)                                                    
                                                                                
         ORG   BUFKDATA                                                         
BUFKTIDC EQU   BUFKTCON+3          USER-ID CODE                                 
BUFKUSER DS    CL(L'CTIKID)                                                     
                                                                                
         ORG   BUFKDATA                                                         
BUFKTIDA EQU   BUFKTCON+4          USER-ID / ALPHA ID                           
BUFKUSAG DS    CL(L'CT9BKAGY+L'CT9BKNUM)                                        
                                                                                
         ORG   BUFKDATA                                                         
BUFKTIDG EQU   BUFKTCON+5                                                       
BUFKIDGR DS    CL(CTWKSUB-CTWKAGY) USER ID GROUP RECORD                         
                                                                                
         ORG   BUFKDATA                                                         
BUFKTUPR EQU   BUFKTCON+6                                                       
BUFKUSPR DS    XL(CTULEN-CTUKSYS)  USER PROFILE RECORD                          
                                                                                
         ORG   BUFKDATA                                                         
BUFKTRPR EQU   BUFKTCON+7                                                       
BUFKREPR DS    XL(CTPKSUB-CTPKSYS) USER PROFILE RECORD                          
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPW# EQU   BUFKTCON+8          PASSWORD NUMBER                              
BUFKPWD# DS    XL(CT0LEN-CT0KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPWC EQU   BUFKTCON+9          PASSWORD CODE                                
BUFKPWDC DS    XL(CT0LEN-CT0KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPWN EQU   BUFKTCON+10         PASSWORD NAME                                
BUFKPWDN DS    XL(CT0LEN-CT0KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTSAC EQU   BUFKTCON+11         SECURITY ACCESS RECORD                       
BUFKSSAC DS    XL(SAASEND-SAASAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTSAF EQU   BUFKTCON+12         FIELD CONTROL RECORD                         
BUFKSSAF DS    XL(SAFCEND-SAFCAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTSAO EQU   BUFKTCON+13         OPTION CONTROL RECORD                        
BUFKSSAO DS    XL(SAOCEND-SAOCAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTOFF EQU   BUFKTCON+14         OFFICE RECORD                                
BUFKOFFC DS    XL(SAOFEND-SAOFAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTDEP EQU   BUFKTCON+15         DEPARTMENT RECORD                            
BUFKDEPT DS    XL(SADPEND-SADPAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPER EQU   BUFKTCON+16         PERSON RECORD                                
BUFKPERS DS    XL(SAPEEND-SAPEAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPAG EQU   BUFKTCON+17         PERSON ACCESS GROUP RECORD                   
BUFKPAGR DS    XL(SAAGEND-SAAGAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTLAG EQU   BUFKTCON+18         LIMIT ACCESS GROUP RECORD                    
BUFKLAGR DS    XL(SALAEND-SALAAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTLAL EQU   BUFKTCON+19         LIMIT ACCESS LIST RECORD                     
BUFKLALI DS    XL(SALMEND-SALMSYS)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTTSA EQU   BUFKTCON+20         TIME SHEET APRROVER GROUP RECORD             
BUFKTSAG DS    XL(SAAPEND-SAAPAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTFAX EQU   BUFKTCON+21         FAX INFORMATION RECORD                       
BUFKFAXI DS    XL(CTFXLEN-CTFXAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTDRU EQU   BUFKTCON+22         DRIVER USER RECORDS                          
BUFKDRUS DS    XL(CT02LEN-CT02KAGY)                                             
                                                                                
         ORG   BUFKDATA                                                         
BUFKTCPP EQU   BUFKTCON+23         CPP EXTRACT RULES RECORD                     
BUFKCPPX DS    XL(CTXLEN-CTXKAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTWRI EQU   BUFKTCON+24         WRITER DEFINITION RECORDS                    
BUFKWRID DS    XL(CT$END-CT$KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTRFP EQU   BUFKTGEN+1          RFP GROUP RECORD                             
BUFKRFPG DS    XL(GRPKEND-GRPKSYST)                                             
                                                                                
         ORG   BUFKDATA                                                         
BUFKTNAR EQU   BUFKTGEN+2          NARRATIVE RECORD                             
BUFKNARG DS    XL(GNDSTAT-GNKAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTEXC EQU   BUFKTGEN+3          EXCHANGE RECORD                              
BUFKEXCG DS    XL(GEDSTAT-GEKAGY)                                               
*&&US                                                                           
         ORG   BUFKDATA                                                         
BUFKTMOF EQU   BUFKTGEN+4          MEDIA OFFICE RECORD                          
BUFKMOFA DS    XL(MOFFSTAT-MOFKAGY)                                             
*&&                                                                             
         ORG   BUFKDATA                                                         
BUFKTBDR EQU   BUFKTGEN+5          ESS BDE REFORM RECORD                        
BUFKBDRG DS    XL(GBFSTAT-GBDRAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTXTA EQU   BUFKTGEN+6          EXTRACT CONTROL RECORD (AGENCY)              
BUFKXTRA DS    XL(GXFSTAT-GXAKAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTXTS EQU   BUFKTGEN+7          EXTRACT CONTROL RECORD (ESS)                 
BUFKXTRS DS    XL(GXFSTAT-GXSKAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTXTF EQU   BUFKTGEN+8          EXTRACT CONTROL RECORD (XFILE)               
BUFKXTRF DS    XL(GXFSTAT-GXFKAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTXSA EQU   BUFKTGEN+9          EXTRACT PASSIVE ESS / AGENCY                 
BUFKXTSA DS    XL(GXDSTAT-GXSAAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTXAF EQU   BUFKTGEN+9          EXTRACT PASSIVE AGENCY / FILE                
BUFKXTAF DS    XL(GXDSTAT-GXAFAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTXSF EQU   BUFKTGEN+10         EXTRACT PASSIVE ESS / FILE                   
BUFKXTSF DS    XL(GXDSTAT-GXSFAGY)                                              
                                                                                
         ORG                                                                    
BUFKEYL  EQU   *-BUFKEY                                                         
                                                                                
BUFDTA   DS    0X                  ** RECORD DATA **                            
                                                                                
BUFDAGY  DS    CL(L'CTWKAGY)       OPTIONAL AGENCY CODE                         
BUFDRKEY DS    XL32                CTFILE/GENDIR RECORD KEY                     
                                                                                
BUFDSTAT DS    X                   ** RECORD STATUS **                          
BUFDSRBD EQU   X'80'               RECORD TO BE DELETED                         
BUFDSNOF EQU   X'40'               RECORD NOT ON FILE                           
BUFDSPSV EQU   X'20'               'PASSIVE' STYLE RECORD                       
                                                                                
BUFDTAL  EQU   *-BUFDTA                                                         
                                                                                
BUFRECL  EQU   *-BUFREC                                                         
                                                                                
LASTBUFK DS    XL(BUFKEYL)         SAVED BUFFERIN KEY                           
                                                                                
C        DS    CL80                                                             
                                                                                
SAVEKEY  DS    CL42                saved key                                    
                                                                                
IOL      DS    F                                                                
IO       DS    XL2000                                                           
                                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
CT$RECD  DSECT                     ** WRITER DEFINITION RECORDS **              
CT$KEY   DS    0XL25                                                            
CT$KTYPE DS    X                                                                
CT$KTYPQ EQU   X'01'                                                            
         DS    XL11                                                             
CT$KAGY  DS    CL2                                                              
CT$KSYS  DS    X                                                                
CT$KPRG  DS    X                                                                
CT$KPHAS DS    X                                                                
CT$KNAME DS    CL8                                                              
CT$END   DS    0X                                                               
       ++INCLUDE CTGENRFP                                                       
       ++INCLUDE GEGENNAR                                                       
       ++INCLUDE GEGENEXC                                                       
       ++INCLUDE GEGENBDR                                                       
       ++INCLUDE GEGENXTR                                                       
       ++INCLUDE GEGENREF                                                       
*&&US                                                                           
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE GEGENOFF                                                       
*&&                                                                             
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTIDC DS    CL(L'CTIKID)                                                     
CTLSTID# DS    XL(L'CTIKNUM)                                                    
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDDPRINTL                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTAGYDEL  01/07/21'                                      
         END                                                                    
