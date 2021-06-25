*          DATA SET CTFILFIX   AT LEVEL 002 AS OF 02/08/06                      
*PHASE CFILFIXA                                                                 
*INCLUDE BUFFERIN                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
FILFIX   TITLE '- CONTROL SYSTEM AGENCY FIXER'                                  
FILFIX   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         NBASE WORKL,**FILFIX,WORK=AWORKC,CLEAR=YES                             
         USING WORKD,RC                                                         
         BASR  RA,0                                                             
         AHI   RA,LITERALS-*                                                    
         USING LITERALS,RA                                                      
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVI   CHRULT,X'BF'                                                     
                                                                                
         GOTOR VALPAR              VALIDATE INPUT PARAMETERS                    
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'TITLE2),TITLE2                                           
         ZAP   LINE,PNINETY                                                     
         ZAP   PAGE,PONE                                                        
                                                                                
         GOTOR GET                 READ RECORDS AND POST TO BUFFER              
                                                                                
         GOTOR PUT                 WRITE BACK ALL DELETED RECORDS               
                                                                                
FILFIXX  XBASE ,                                                                
         DROP  RB                                                               
                                                                                
AWORKC   DC    A(WORKC)                                                         
         EJECT                                                                  
GET      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR VBUFFRIN,PARM,('BUFFAINI',BUFFER),BUFKEY,COMFACS                 
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR VDMGR,DMCB,DMOPEN,CONTROL,DMFLIST,IO                             
                                                                                
         GOTOR RESACS              GET ACCESS RECORDS                           
         GOTOR RESIDS              GET USER-ID RECORDS                          
         GOTOR RESLST              GET LIST RECORDS                             
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
*&&US*&& GOTOR GETRFP              GET RFP GROUP RECORDS                        
                                                                                
GETX     J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ BACK BUFFERIN FILE AND TRACE/DELETE FILE UPDATES               *         
***********************************************************************         
                                                                                
PUT      NTR1  BASE=*,LABEL=*                                                   
         MVC   P(L'UPDLIT),UPDLIT                                               
         CLI   WRITE,C'Y'                                                       
         BE    *+10                                                             
         MVC   P(L'GETLIT),GETLIT                                               
         GOTOR VPRINTER                                                         
         XC    BUFREC(BUFRECL),BUFREC                                           
         GOTOR VBUFFRIN,PARM,('BUFFARDH',BUFFER),BUFREC,COMFACS                 
         B     PUT04                                                            
PUT02    GOTOR VBUFFRIN,PARM,('BUFFASEQ',BUFFER),BUFREC,COMFACS                 
PUT04    BNE   PUT12                                                            
         TM    BUFDSTAT,BUFDSRBD   TEST RECORD TO BE DELETED                    
         BZ    PUT02                                                            
                                                                                
         CLI   TRACE,C'Y'                                                       
         BNE   PUT06                                                            
         GOTOR VHEXOUT,DMCB,BUFKEY,P+1,BUFRECL,HEXTOG                           
         GOTOR VPRINTER                                                         
                                                                                
PUT06    CLI   BUFKTYPE,BUFKTGEN   TEST FILE TYPE                               
         BH    PUT08                                                            
                                                                                
         MVC   IO(L'CT5KEY),BUFDRKEY                                            
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,IO,IO                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    IO+(CT5STAT-CT5KEY),X'80'                                        
         AP    CTFILED,PONE                                                     
         CLI   WRITE,C'Y'                                                       
         BNE   PUT02                                                            
         GOTOR VDMGR,DMCB,DMWRT,CTFILE,IO,IO                                    
         BE    PUT02                                                            
         DC    H'0'                                                             
                                                                                
PUT08    MVC   IO(L'GRPKEY),BUFDRKEY                                            
         GOTOR VDMGR,DMCB,DMREAD,GENDIR,IO,IO                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    IO+(GRPKSTAT-GRPKEY),X'80'                                       
         AP    GENDIRD,PONE                                                     
         CLI   WRITE,C'Y'                                                       
         BNE   PUT10                                                            
         GOTOR VDMGR,DMCB,DMWRT,GENDIR,IO,IO                                    
         BE    PUT10                                                            
         DC    H'0'                                                             
                                                                                
PUT10    MVC   DUB(L'GRPKDA),IO+(GRPKDA-GRPKEY)                                 
         GOTOR VDMGR,DMCB,DMGETREC,GENFIL,DUB,IO,WORK                           
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    IO+(GRPSTAT-GRPKEY),X'80'                                        
         AP    GENFILD,PONE                                                     
         CLI   WRITE,C'Y'                                                       
         BNE   PUT02                                                            
         GOTOR VDMGR,DMCB,DMPUTREC,GENFIL,DUB,IO,WORK                           
         BE    PUT02                                                            
         DC    H'0'                                                             
                                                                                
PUT12    ZAP   LINE,PNINETY                                                     
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
                                                                                
RESACS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON01),RECCON01                                           
         GOTOR VPRINTER                                                         
         USING CT5REC,IO                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CT5REC,CT5REC                           
         J     RESACS04                                                         
RESACS02 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CT5REC,CT5REC                           
RESACS04 JE    *+6                                                              
         DC    H'0'                                                             
         CLI   CT5KTYP,CT5KTYPQ                                                 
         JNE   EXIT                                                             
         GOTOR PUTBUF,DMCB,CT5KALPH,CT5KEY,                            *        
               ('BUFKTACS',L'BUFKAGYA-1),0                                      
         J     RESACS02                                                         
         EJECT                                                                  
***********************************************************************         
* READ ID RECORDS AND POST TO BUFFER                                  *         
***********************************************************************         
                                                                                
RESIDS   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON02),RECCON02                                           
         GOTOR VPRINTER                                                         
         USING CTIKEY,IO                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVI   CTIKID,1                                                         
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                           
         J     RESIDS04                                                         
RESIDS02 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY                           
RESIDS04 JE    *+6                                                              
         DC    H'0'                                                             
         CLI   CTIKTYP,CTIKTYPQ                                                 
         JNE   EXIT                                                             
                                                                                
         XC    WORK(L'CTIKNUM),WORK                                             
         LA    RF,CTIDATA                                                       
         USING CTAGYD,RF                                                        
         SR    R0,R0                                                            
RESIDS06 CLI   CTAGYEL,0                                                        
         JE    RESIDS02                                                         
         CLI   CTAGYEL,CTDSCELQ                                                 
         JNE   *+14                                                             
         MVC   WORK(L'CTIKNUM),CTDSC-CTDSCD(RF)                                 
         J     *+12                                                             
         CLI   CTAGYEL,CTAGYELQ                                                 
         JE    *+14                                                             
         IC    R0,CTAGYLEN                                                      
         AR    RF,R0                                                            
         J     RESIDS06                                                         
         OC    WORK(L'CTIKNUM),WORK                                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+2(L'CTAGYID),CTAGYID                                        
                                                                                
         GOTOR TSTACS,WORK+2                                                    
                                                                                
         GOTOR PUTBUF,DMCB,(STATUS,CTIKID),CTIKEY,                     *        
               ('BUFKTIDC',L'BUFKUSER-1),WORK+2                                 
         MVC   WORK+4(L'CTIKID),CTIKID                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         OC    CTIKNUM,WORK                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTBUF,DMCB,CTIKNUM,CTIKEY,                             *        
               ('BUFKTID#',L'BUFKUSID-1),WORK+2                                 
         J     RESIDS02                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIST RECORDS AND POST TO BUFFER                                *         
***********************************************************************         
                                                                                
RESLST   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON03),RECCON03                                           
         GOTOR VPRINTER                                                         
         USING CTWREC,IO                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CTWKEY,CTWKEY                           
         J     RESLST04                                                         
RESLST02 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CTWKEY,CTWKEY                           
RESLST04 JE    *+6                                                              
         DC    H'0'                                                             
         CLI   CTWKTYP,CTWKTYPQ                                                 
         JNE   EXIT                                                             
         SR    RF,RF                                                            
         CLI   CTWKAGY,0           TEST AGENCY BASED LIST                       
         JE    RESLST06                                                         
                                                                                
         GOTOR TSTACS,CTWKAGY                                                   
                                                                                
RESLST06 GOTOR PUTBUF,DMCB,(STATUS,CTWKAGY),CTWKEY,                    *        
               ('BUFKTIDG',L'BUFKIDGR-1),0                                      
         J     RESLST02                                                         
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
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                           
         J     GETUPR04                                                         
GETUPR02 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CTUKEY,CTUKEY                           
GETUPR04 JE    *+6                                                              
         DC    H'0'                                                             
         CLI   CTUKTYP,CTUKTYPQ                                                 
         JNE   EXIT                                                             
         OC    CTUKAGY,CTUKAGY                                                  
         JZ    GETUPR02                                                         
         CLI   CTUKAGY,X'40'       TEST AGENCY LEVEL PROFILE                    
         JNL   GETUPR06                                                         
         GOTOR TSTID#,CTUKAGY                                                   
         JNE   GETUPR02                                                         
         J     GETUPR08                                                         
                                                                                
GETUPR06 GOTOR TSTACS,CTUKAGY                                                   
         JNE   GETUPR02                                                         
                                                                                
GETUPR08 GOTOR PUTBUF,DMCB,('BUFDSRBD',CTUKSYS),CTUKEY,                *        
               ('BUFKTUPR',L'BUFKUSPR-1),0                                      
         MVC   P(L'UPRLIT),UPRLIT                                               
         LA    R1,P+L'UPRLIT+1                                                  
         MVI   0(R1),C'('                                                       
         MVC   1(L'BUFDAGY,R1),WORK                                             
         MVI   1+L'BUFDAGY(R1),C')'                                             
         MVC   0(L'CTUKSYS,R1),CTUKSYS                                          
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
         JNL   GETUPR10                                                         
         SR    R0,R0                                                            
         ICM   R0,3,CTUKAGY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R1),DUB                                                      
         AHI   R1,6                                                             
         J     GETUPR12                                                         
GETUPR10 MVC   1(L'CTUKAGY,R1),CTUKAGY                                          
         AHI   R1,L'CTUKAGY+1                                                   
GETUPR12 MVI   0(R1),C'/'                                                       
         CLI   CTUKMED,0                                                        
         JNE   GETUPR14                                                         
         MVC   1(L'ALLLIT,R1),ALLLIT                                            
         AHI   R1,L'ALLLIT+1                                                    
         J     GETUPR16                                                         
GETUPR14 MVC   1(L'CTUKMED,R1),CTUKMED                                          
         AHI   R1,L'CTUKMED+1                                                   
GETUPR16 MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         CLI   CTUKCLT,0                                                        
         JNE   GETUPR18                                                         
         MVC   0(L'ALLLIT,R1),ALLLIT                                            
         AHI   R1,L'ALLLIT+1                                                    
         J     GETUPR20                                                         
GETUPR18 MVC   0(L'CTUKCLT,R1),CTUKCLT                                          
         AHI   R1,L'CTUKCLT-1                                                   
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   1(L'CTUKCLT-1,R1),SPACES                                         
         AHI   R1,2                                                             
GETUPR20 MVC   0(L'WILBEDEL,R1),WILBEDEL                                        
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
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CTPKEY,CTPKEY                           
         J     GETRPR04                                                         
GETRPR02 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CTPKEY,CTPKEY                           
GETRPR04 JE    *+6                                                              
         DC    H'0'                                                             
         CLI   CTPKTYP,CTPKTYPQ                                                 
         JNE   EXIT                                                             
         OC    CTPKORIG,CTPKORIG                                                
         JZ    GETRPR02                                                         
         GOTOR TSTID#,CTPKORIG                                                  
         JNE   GETRPR02                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CTPKSYS),CTPKEY,                *        
               ('BUFKTRPR',L'BUFKREPR-1),0                                      
         MVC   P(L'RPRLIT),RPRLIT                                               
         LA    R1,P+L'RPRLIT+1                                                  
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
         MVC   7(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETRPR02                                                         
         EJECT                                                                  
***********************************************************************         
* READ PASSWORD RECORDS AND POST TO BUFFER                            *         
***********************************************************************         
                                                                                
GETPWD   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON06),RECCON06                                           
         GOTOR VPRINTER                                                         
         USING CT0REC,IO                                                        
         XC    LASTAGY,LASTAGY                                                  
                                                                                
GETPWD04 XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,CT0KAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CT0KEY,CT0KEY                           
         J     GETPWD08                                                         
GETPWD06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CT0KEY,CT0KEY                           
GETPWD08 JNE   EXIT                                                             
         CLI   CT0KTYP,CT0KTEQU                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,CT0KAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETPWD04                                                         
                                                                                
         CLI   CT0KOFFC,0          TEST PASSWORD NAME RECORD                    
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
         MVC   6(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETPWD06                                                         
                                                                                
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETPWD06                                                         
                                                                                
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETPWD06                                                         
         EJECT                                                                  
***********************************************************************         
* READ ACCESS RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETSAC   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON07),RECCON07                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAASREC,IO                                                       
GETSAC04 XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAASAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAASKEY,SAASKEY                         
         J     GETSAC08                                                         
GETSAC06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAASKEY,SAASKEY                         
GETSAC08 JNE   EXIT                                                             
         CLI   SAASTYP,SAASTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAASSUB,SAASSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAASAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETSAC04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAASAGY),SAASKEY,               *        
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
         MVC   7(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETSAC06                                                         
         EJECT                                                                  
***********************************************************************         
* READ FIELD CONTROL RECORDS AND POST TO BUFFER                       *         
***********************************************************************         
                                                                                
GETSAF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON08),RECCON08                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAFCREC,IO                                                       
GETSAF04 XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAFCAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAFCKEY,SAFCKEY                         
         J     GETSAF08                                                         
GETSAF06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAFCKEY,SAFCKEY                         
GETSAF08 JNE   EXIT                                                             
         CLI   SAFCTYP,SAFCTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAFCSUB,SAFCSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAFCAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETSAF04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAFCAGY),SAFCKEY,               *        
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
         MVC   7(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETSAF06                                                         
         EJECT                                                                  
***********************************************************************         
* READ OPTION CONTROL RECORDS AND POST TO BUFFER                      *         
***********************************************************************         
                                                                                
GETSAO   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON09),RECCON09                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAOCREC,IO                                                       
GETSAO04 XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAOCAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAOCKEY,SAOCKEY                         
         J     GETSAO08                                                         
GETSAO06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAOCKEY,SAOCKEY                         
GETSAO08 JNE   EXIT                                                             
         CLI   SAOCTYP,SAOCTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAOCSUB,SAOCSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAOCAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETSAO04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAOCAGY),SAOCKEY,               *        
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
         MVC   7(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETSAO06                                                         
         EJECT                                                                  
***********************************************************************         
* READ OFFICE RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETOFF   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON10),RECCON10                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAOFREC,IO                                                       
GETOFF04 XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAOFAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAOFKEY,SAOFKEY                         
         J     GETOFF08                                                         
GETOFF06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAOFKEY,SAOFKEY                         
GETOFF08 JNE   EXIT                                                             
         CLI   SAOFTYP,SAOFTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAOFSUB,SAOFSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAOFAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETOFF04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAOFAGY),SAOFKEY,               *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETOFF06                                                         
         EJECT                                                                  
***********************************************************************         
* READ DEPARTMENT RECORDS AND POST TO BUFFER                          *         
***********************************************************************         
                                                                                
GETDEP   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON11),RECCON11                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SADPREC,IO                                                       
GETDEP04 XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SADPAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SADPKEY,SADPKEY                         
         J     GETDEP08                                                         
GETDEP06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SADPKEY,SADPKEY                         
GETDEP08 JNE   EXIT                                                             
         CLI   SADPTYP,SADPTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SADPSUB,SADPSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SADPAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETDEP04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SADPAGY),SADPKEY,               *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETDEP06                                                         
         EJECT                                                                  
***********************************************************************         
* READ PERSON RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETPER   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON12),RECCON12                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAPEREC,IO                                                       
GETPER04 XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAPEAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAPEKEY,SAPEKEY                         
         J     GETPER08                                                         
GETPER06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAPEKEY,SAPEKEY                         
GETPER08 JNE   EXIT                                                             
         CLI   SAPETYP,SAPETYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAPESUB,SAPESUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAPEAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETPER04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAPEAGY),SAPEKEY,               *        
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
         MVC   1(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETPER06                                                         
         EJECT                                                                  
***********************************************************************         
* READ PERSON RECORDS AND POST TO BUFFER                              *         
***********************************************************************         
                                                                                
GETPAG   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON13),RECCON13                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAAGREC,IO                                                       
GETPAG04 XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAAGAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAAGKEY,SAAGKEY                         
         J     GETPAG08                                                         
GETPAG06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAAGKEY,SAAGKEY                         
GETPAG08 JNE   EXIT                                                             
         CLI   SAAGTYP,SAAGTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAAGSUB,SAAGSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAAGAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETPAG04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAAGAGY),SAAGKEY,               *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETPAG06                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIMIT ACCESS GROUP RECORDS AND POST TO BUFFER                  *         
***********************************************************************         
                                                                                
GETLAG   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON14),RECCON14                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SALAREC,IO                                                       
GETLAG04 XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SALAAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SALAKEY,SALAKEY                         
         J     GETLAG08                                                         
GETLAG06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SALAKEY,SALAKEY                         
GETLAG08 JNE   EXIT                                                             
         CLI   SALATYP,SALATYPQ                                                 
         JNE   EXIT                                                             
         CLI   SALASUB,SALASUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SALAAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETLAG04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SALAAGY),SALAKEY,               *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETLAG06                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIMIT ACCESS LIST RECORDS AND POST TO BUFFER                   *         
***********************************************************************         
                                                                                
GETLAL   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON15),RECCON15                                           
         GOTOR VPRINTER                                                         
         MVI   LASTSYS,0                                                        
                                                                                
         USING SALMREC,IO                                                       
GETLAL02 XC    SALMKEY,SALMKEY                                                  
         MVI   SALMTYP,SALMTYPQ                                                 
         MVI   SALMSUB,SALMSUBQ                                                 
         SR    R0,R0                                                            
         IC    R0,LASTSYS                                                       
         AHI   R0,1                                                             
         STC   R0,SALMSYS                                                       
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SALMKEY,SALMKEY                         
         JNE   EXIT                                                             
         CLI   SALMTYP,SALMTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SALMSUB,SALMSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTSYS,SALMSYS                                                  
         XC    LASTAGY,LASTAGY                                                  
                                                                                
GETLAL04 XC    SALMKEY,SALMKEY                                                  
         MVI   SALMTYP,SALMTYPQ                                                 
         MVI   SALMSUB,SALMSUBQ                                                 
         MVC   SALMSYS,LASTSYS                                                  
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SALMAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SALMKEY,SALMKEY                         
         J     GETLAL08                                                         
GETLAL06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SALMKEY,SALMKEY                         
GETLAL08 JNE   GETLAL02                                                         
         CLI   SALMTYP,SALMTYPQ                                                 
         JNE   GETLAL02                                                         
         CLI   SALMSUB,SALMSUBQ                                                 
         JNE   GETLAL02                                                         
         CLC   SALMSYS,LASTSYS                                                  
         JNE   GETLAL02                                                         
         MVC   LASTAGY,SALMAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETLAL04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SALMAGY),SALMKEY,               *        
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
         MVC   1(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETLAL06                                                         
         EJECT                                                                  
***********************************************************************         
* READ LIMIT ACCESS GROUP RECORDS AND POST TO BUFFER                  *         
***********************************************************************         
                                                                                
GETTSA   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON16),RECCON16                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING SAAPREC,IO                                                       
GETTSA04 XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,SAAPAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,SAAPKEY,SAAPKEY                         
         J     GETTSA08                                                         
GETTSA06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,SAAPKEY,SAAPKEY                         
GETTSA08 JNE   EXIT                                                             
         CLI   SAAPTYP,SAAPTYPQ                                                 
         JNE   EXIT                                                             
         CLI   SAAPSUB,SAAPSUBQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,SAAPAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETTSA04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',SAAPAGY),SAAPKEY,               *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETTSA06                                                         
                                                                                
GETTSA10 AHI   R2,L'CT5KALPH+1     BUMP TO NEXT AGENCY                          
         J     GETTSA04                                                         
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* READ FAX INFORMATION RECORDS AND POST TO BUFFER                     *         
***********************************************************************         
                                                                                
GETFAX   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON17),RECCON17                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING CTFXREC,IO                                                       
GETFAX04 XC    CTFXKEY,CTFXKEY                                                  
         MVI   CTFXKTYP,CTFXEQU                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,CTFXAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CTFXKEY,CTFXKEY                         
         J     GETFAX08                                                         
GETFAX06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CTFXKEY,CTFXKEY                         
GETFAX08 JNE   EXIT                                                             
         CLI   CTFXKTYP,CTFXEQU                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,CTFXAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETFAX04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CTFXAGY),CTFXKEY,               *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETFAX06                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* READ DRIVER USER RECORDS AND POST TO BUFFER                         *         
***********************************************************************         
                                                                                
GETDRU   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON19),RECCON19                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING CT02REC,IO                                                       
GETDRU04 XC    CT02KEY,CT02KEY                                                  
         MVI   CT02KTYP,CT02KTYQ                                                
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,CT02KAGY                                                    
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CT02KEY,CT02KEY                         
         J     GETDRU08                                                         
GETDRU06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CT02KEY,CT02KEY                         
GETDRU08 JNE   EXIT                                                             
         CLI   CT02KTYP,CT02KTYQ                                                
         JNE   EXIT                                                             
         MVC   LASTAGY,CT02KAGY                                                 
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETDRU04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CT02KAGY),CT02KEY,              *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETDRU06                                                         
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* READ CPP EXTRACT RULES RECORDS AND POST TO BUFFER                   *         
***********************************************************************         
                                                                                
GETCPP   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON20),RECCON20                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING CTXREC,IO                                                        
GETCPP04 XC    CTXKEY,CTXKEY                                                    
         MVI   CTXKTYP,CTXKTYPQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,CTXKAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CTXKEY,CTXKEY                           
         J     GETCPP08                                                         
GETCPP06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CTXKEY,CTXKEY                           
GETCPP08 JNE   EXIT                                                             
         CLI   CTXKTYP,CTXKTYPQ                                                 
         JNE   EXIT                                                             
         MVC   LASTAGY,CTXKAGY                                                  
         GOTOR TSTACS,CTXKAGY                                                   
         JNE   GETCPP04                                                         
                                                                                
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
         MVC   1(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETCPP06                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* READ WRITER DEFINITION RECORDS AND POST TO BUFFER                   *         
***********************************************************************         
                                                                                
GETWRI   NTR1  LABEL=NO                                                         
         MVC   P(L'RECCON21),RECCON21                                           
         GOTOR VPRINTER                                                         
         XC    LASTAGY,LASTAGY                                                  
                                                                                
         USING CT$RECD,IO                                                       
GETWRI04 XC    CT$KEY,CT$KEY                                                    
         MVI   CT$KTYPE,CT$KTYPQ                                                
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,CT$KAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,CTFILE,CT$KEY,CT$KEY                           
         J     GETWRI08                                                         
GETWRI06 GOTOR VDMGR,DMCB,DMRSEQ,CTFILE,CT$KEY,CT$KEY                           
GETWRI08 JNE   EXIT                                                             
         CLI   CT$KTYPE,CT$KTYPQ                                                
         JNE   EXIT                                                             
         MVC   LASTAGY,CT$KAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETWRI04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',CT$KAGY),CT$KEY,                *        
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
         MVC   2(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETWRI06                                                         
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* READ RFP GROUP RECORDS AND POST TO BUFFER                           *         
***********************************************************************         
                                                                                
GETRFP   NTR1  LABEL=NO                                                         
         MVC   P(L'RECGEN01),RECGEN01                                           
         GOTOR VPRINTER                                                         
         MVI   LASTSYS,0                                                        
                                                                                
         USING GRPKEYD,IO                                                       
GETRFP02 XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         SR    R0,R0                                                            
         IC    R0,LASTSYS                                                       
         AHI   R0,1                                                             
         STC   R0,GRPKSYST                                                      
         GOTOR VDMGR,DMCB,DMRDHI,GENDIR,GRPKEY,GRPKEY                           
         JNE   EXIT                                                             
         CLI   GRPKSYS,GRPKSYSQ                                                 
         JNE   EXIT                                                             
         CLI   GRPKSTYP,GRPKSTYQ                                                
         JNE   EXIT                                                             
         MVC   LASTSYS,GRPKSYST                                                 
         XC    LASTAGY,LASTAGY                                                  
                                                                                
GETRFP04 XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVC   GRPKSYST,LASTSYS                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LASTAGY                                                     
         AHI   R0,1                                                             
         STCM  R0,3,LASTAGY                                                     
         STCM  R0,3,GRPKAGY                                                     
         GOTOR VDMGR,DMCB,DMRDHI,GENDIR,GRPKEY,GRPKEY                           
         J     GETRFP08                                                         
GETRFP06 GOTOR VDMGR,DMCB,DMRSEQ,GENDIR,GRPKEY,GRPKEY                           
GETRFP08 JNE   GETRFP02                                                         
         CLI   GRPKSYS,GRPKSYSQ                                                 
         JNE   GETRFP02                                                         
         MVI   GRPKSTYP,GRPKSTYQ                                                
         JNE   GETRFP02                                                         
         CLC   GRPKSYST,LASTSYS                                                 
         JNE   GETRFP02                                                         
         MVC   LASTAGY,GRPKAGY                                                  
         GOTOR TSTACS,LASTAGY                                                   
         JNE   GETRFP04                                                         
                                                                                
         GOTOR PUTBUF,DMCB,('BUFDSRBD',GRPKSYST),GRPKEY,               *        
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
         MVC   1(L'WILBEDEL,R1),WILBEDEL                                        
         GOTOR VPRINTER                                                         
         J     GETRFP06                                                         
*&&                                                                             
         EJECT                                                                  
TSTACS   NTR1  LABEL=NO                                                         
         MVI   STATUS,0                                                         
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFKTYPE,BUFKTACS                                                
         MVC   BUFKAGYA,0(R1)                                                   
         GOTOR BUFGET                                                           
         JE    *+8                                                              
         MVI   STATUS,BUFDSRBD                                                  
         CLI   STATUS,BUFDSRBD                                                  
         J     EXIT                                                             
                                                                                
TSTID#   NTR1  LABEL=NO                                                         
         MVI   STATUS,0                                                         
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFKTYPE,BUFKTID#                                                
         MVC   BUFKUSID,0(R1)                                                   
         GOTOR BUFGET                                                           
         JE    *+12                                                             
         MVI   STATUS,BUFDSRBD                                                  
         J     *+10                                                             
         MVC   STATUS,BUFDSTAT                                                  
         CLI   STATUS,BUFDSRBD                                                  
         J     EXIT                                                             
                                                                                
TSTIDC   NTR1  LABEL=NO                                                         
         MVI   STATUS,0                                                         
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFKTYPE,BUFKTIDC                                                
         MVC   BUFKUSER,0(R1)                                                   
         GOTOR BUFGET                                                           
         JE    *+12                                                             
         MVI   STATUS,BUFDSRBD                                                  
         J     *+10                                                             
         MVC   STATUS,BUFDSTAT                                                  
         CLI   STATUS,BUFDSRBD                                                  
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
                                                                                
VALPAR08 MVC   P(L'ERROR1),ERROR1                                               
         GOTOR VPRINTER                                                         
         ABEND 1                                                                
         DROP  RB                                                               
                                                                                
PARAMS   DS    0XL24               ** TABLE OF PARAMETER CARDS **               
                                                                                
         DC    AL1(05),AL3(PARDDIO),CL20'DDSIO='                                
         DC    AL1(05),AL3(PARTRAC),CL20'TRACE='                                
         DC    AL1(07),AL3(PARRCVR),CL20'RECOVER='                              
         DC    AL1(08),AL3(PARRCVR),CL20'RECOVERY='                             
         DC    AL1(05),AL3(PARWRIT),CL20'WRITE='                                
         DC    AL1(06),AL3(PARGETI),CL20'GETIDS='                               
         DC    AL1(01),AL3(EXIT),CL20'/*'                                       
                                                                                
PARAMSX  DC    AL1(0)                                                           
         EJECT                                                                  
PARDDIO  L     RF,VDDSIO           DDSIO=                                       
         MVC   0(8,RF),0(R1)                                                    
         BR    RE                                                               
                                                                                
PARTRAC  MVC   TRACE,0(R1)         TRACE=                                       
         BR    RE                                                               
                                                                                
PARGETI  MVC   GETIDS,0(R1)        GETIDS=                                      
         XC    AGETIDS,AGETIDS                                                  
         BR    RE                                                               
                                                                                
PARRCVR  OI    SSOSTAT2,SSOSNRCV   RECOVERY=                                    
         CLI   0(R1),C'N'                                                       
         BER   RE                                                               
         MVI   CTRCVRW,C'U'                                                     
         NI    SSOSTAT2,X'FF'-(SSOSNRCV+SSOSROLC)                               
         CLI   0(R1),C'C'          TEST RECOVER COPIES TOO                      
         BNER  RE                                                               
         OI    SSOSTAT2,SSOSROLC                                                
         BR    RE                                                               
                                                                                
PARWRIT  MVC   WRITE,0(R1)         WRITE=                                       
         CLI   WRITE,C'Y'          TEST LIVE RUN                                
         BNER  RE                                                               
         MVI   CTFILEW,C'U'                                                     
         MVI   GENFILW,C'U'                                                     
         MVI   GENDIRW,C'U'                                                     
         BR    RE                                                               
         EJECT                                                                  
ONEK     EQU   1024                                                             
                                                                                
LITERALS DS    0D                                                               
                                                                                
         LTORG                                                                  
                                                                                
RECCON01 DC    C'Reading Access records (CT5REC)'                               
RECCON02 DC    C'Reading ID records (CTIREC)'                                   
RECCON03 DC    C'Reading List records (CTWREC)'                                 
RECCON04 DC    C'Reading User Profile records (CTUREC)'                         
RECCON05 DC    C'Reading Report Profile records (CTPREC)'                       
RECCON06 DC    C'Reading Password records (CT0REC)'                             
RECCON07 DC    C'Reading Access records (SAASSUBQ)'                             
RECCON08 DC    C'Reading Field control records (SAFCSUBQ)'                      
RECCON09 DC    C'Reading Option control records (SAOCSUBQ)'                     
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
                                                                                
ACSLIT   DC    C'Access record'                                                 
UIDLIT   DC    C'User-ID records'                                               
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
FAXLIT   DC    C'FAX Information record'                                        
DRULIT   DC    C'DRIVER User record'                                            
CPPLIT   DC    C'CPP Extract Rules record'                                      
WRILIT   DC    C'Writer Definition record'                                      
TRMLIT   DC    C'Terminal record'                                               
                                                                                
TITLE1   DC    C'Control Cards'                                                 
TITLE2   DC    C'Control System Fixer'                                          
                                                                                
ERROR1   DC    C'Invalid control card'                                          
                                                                                
ALLLIT   DC    C'All'                                                           
WILBEDEL DC    C'will be deleted'                                               
UPDLIT   DC    C'Updating files for real'                                       
GETLIT   DC    C'Reading back BUFFERIN file to check keys'                      
                                                                                
CTFILELD DC    C'Total CTFILE records='                                         
CTFILED  DC    PL4'0'                                                           
GENDIRLD DC    C'Total GENDIR records='                                         
GENDIRD  DC    PL4'0'                                                           
GENFILLD DC    C'Total GENFIL records='                                         
GENFILD  DC    PL4'0'                                                           
                                                                                
COMFACS  DS    0F                                                               
VDMGR    DC    V(DATAMGR)                                                       
                                                                                
VBUFFRIN DC    V(BUFFERIN)                                                      
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDDSIO   DC    V(DDSIO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINTER DC    V(PRINTER)                                                       
                                                                                
AIDWORK  DC    A(IDWORK)                                                        
                                                                                
DMOPEN   DC    CL8'OPEN    '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
DMGETREC DC    CL8'GETREC  '                                                    
DMPUTREC DC    CL8'PUTREC  '                                                    
CONTROL  DC    CL8'CONTROL '                                                    
                                                                                
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
                                                                                
PNINETY  DC    P'90'                                                            
PONE     DC    P'1'                                                             
RE00     DC    C'RE00'                                                          
HEXTOG   DC    C'TOG'                                                           
                                                                                
BUFFER   BUFFD TYPE=D,KEYLEN=BUFKEYL,COMLEN=BUFDTAL,BUFFERS=200                 
                                                                                
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB*SSB*SSB*SSB*SSB'                              
SSB      DS    0F                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSB                                                              
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOSTAT2                                                         
         DC    AL1(SSOSNRCV)       SET NO RECOVERY                              
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED SSB                             
         ORG                                                                    
                                                                                
         DS    0D                                                               
         DC    C'*IDWORK**IDWORK**IDWORK**IDWORK*'                              
IDWORKMX EQU   1000                                                             
IDWORK   DS    (IDWORKMX)XL12      LIST OF DESTINATION/COMPATIBLE IDS           
                                                                                
         DC    C'**WORKC***WORKC***WORKC***WORKC*'                              
WORKC    DS    (200*ONEK)X                                                      
         EJECT                                                                  
WORKD    DSECT                     ** WORK AREA **                              
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    XL256                                                            
                                                                                
FGETIDS  DS    0F                                                               
AGETIDS  DS    A                                                                
GETIDS   DS    CL8                                                              
                                                                                
RUNVALS  DS    0X                                                               
TRACE    DS    C                                                                
WRITE    DS    C                                                                
RUNVALL  EQU   *-RUNVALS                                                        
                                                                                
LASTSYS  DS    XL(L'SALMSYS)                                                    
LASTAGY  DS    XL(L'CT0KAGY)                                                    
                                                                                
STATUS   DS    XL(L'BUFDSTAT)                                                   
                                                                                
BUFREC   DS    0X                                                               
                                                                                
BUFKEY   DS    0X                                                               
                                                                                
BUFKTYPE DS    X                   RECORD TYPE                                  
BUFKTCON EQU   X'00'               BASE NUMBER FOR CONTROL FILE                 
BUFKTGEN EQU   X'80'               BASE NUMBER FOR GENDIR/GENFIL                
                                                                                
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
BUFKTIDG EQU   BUFKTCON+4                                                       
BUFKIDGR DS    CL(CTWKSUB-CTWKAGY) USER ID GROUP RECORD                         
                                                                                
         ORG   BUFKDATA                                                         
BUFKTUPR EQU   BUFKTCON+5                                                       
BUFKUSPR DS    XL(CTULEN-CTUKSYS)  USER PROFILE RECORD                          
                                                                                
         ORG   BUFKDATA                                                         
BUFKTRPR EQU   BUFKTCON+6                                                       
BUFKREPR DS    XL(CTPKSUB-CTPKSYS) USER PROFILE RECORD                          
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPW# EQU   BUFKTCON+7          PASSWORD NUMBER                              
BUFKPWD# DS    XL(CT0LEN-CT0KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPWC EQU   BUFKTCON+8          PASSWORD CODE                                
BUFKPWDC DS    XL(CT0LEN-CT0KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPWN EQU   BUFKTCON+9          PASSWORD NAME                                
BUFKPWDN DS    XL(CT0LEN-CT0KAGY)                                               
                                                                                
         ORG   BUFKDATA                                                         
BUFKTSAC EQU   BUFKTCON+10         SECURITY ACCESS RECORD                       
BUFKSSAC DS    XL(SAASEND-SAASAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTSAF EQU   BUFKTCON+11         FIELD CONTROL RECORD                         
BUFKSSAF DS    XL(SAFCEND-SAFCAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTSAO EQU   BUFKTCON+12         OPTION CONTROL RECORD                        
BUFKSSAO DS    XL(SAOCEND-SAOCAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTOFF EQU   BUFKTCON+13         OFFICE RECORD                                
BUFKOFFC DS    XL(SAOFEND-SAOFAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTDEP EQU   BUFKTCON+14         DEPARTMENT RECORD                            
BUFKDEPT DS    XL(SADPEND-SADPAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPER EQU   BUFKTCON+15         PERSON RECORD                                
BUFKPERS DS    XL(SAPEEND-SAPEAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTPAG EQU   BUFKTCON+16         PERSON ACCESS GROUP RECORD                   
BUFKPAGR DS    XL(SAAGEND-SAAGAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTLAG EQU   BUFKTCON+17         LIMIT ACCESS GROUP RECORD                    
BUFKLAGR DS    XL(SALAEND-SALAAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTLAL EQU   BUFKTCON+18         LIMIT ACCESS LIST RECORD                     
BUFKLALI DS    XL(SALMEND-SALMSYS)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTTSA EQU   BUFKTCON+19         TIME SHEET APRROVER GROUP RECORD             
BUFKTSAG DS    XL(SAAPEND-SAAPAGY)                                              
                                                                                
         ORG   BUFKDATA                                                         
BUFKTFAX EQU   BUFKTCON+20         FAX INFORMATION RECORD                       
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
                                                                                
         ORG                                                                    
BUFKEYL  EQU   *-BUFKEY                                                         
                                                                                
BUFDTA   DS    0X                                                               
                                                                                
BUFDAGY  DS    CL(L'CTWKAGY)       OPTIONAL AGENCY CODE                         
BUFDRKEY DS    XL32                RECORD KEY                                   
                                                                                
BUFDSTAT DS    X                   ** RECORD STATUS **                          
BUFDSRBD EQU   X'80'               RECORD TO BE DELETED                         
BUFDSNOF EQU   X'40'               RECORD NOT ON FILE                           
                                                                                
BUFDTAL  EQU   *-BUFDTA                                                         
                                                                                
BUFRECL  EQU   *-BUFREC                                                         
                                                                                
LASTBUFK DS    XL(BUFKEYL)         SAVED BUFFERIN KEY                           
                                                                                
C        DS    CL80                                                             
                                                                                
IO       DS    XL2000                                                           
                                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
CT$RECD  DSECT                                                                  
CT$KEY   DS    0XL25                                                            
CT$KTYPE DS    XL1                                                              
CT$KTYPQ EQU   X'01'                                                            
         DS    CL11                                                             
CT$KAGY  DS    CL2                                                              
CT$KSYS  DS    XL1                                                              
CT$KPRG  DS    XL1                                                              
CT$KPHAS DS    XL1                                                              
CT$KNAME DS    CL8                                                              
CT$END   DS    0X                                                               
*&&US                                                                           
       ++INCLUDE CTGENRFP                                                       
*&&                                                                             
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTIDC DS    CL(L'CTIKID)                                                     
CTLSTID# DS    XL(L'CTIKNUM)                                                    
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDDPRINTL                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTFILFIX  02/08/06'                                      
         END                                                                    
