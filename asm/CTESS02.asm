*          DATA SET CTESS02    AT LEVEL 074 AS OF 11/11/08                      
*PHASE TA0E02A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*INCLUDE SCINKEY                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*                                                                               
         TITLE 'CTESS02 - ESS CONTROL MAINTENANCE - ESSID RECORDS'              
ESS02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ES02**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GESSD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ESSID RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GESSD,R2            R2=A(ESSID RECORD KEY)                       
         XC    GSKEY,GSKEY                                                      
         MVI   GSKREC,GSKRECQ      RECORD TYPE                                  
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,ESSIDH        VALIDATE ESS ID                              
         BNE   VALKEYX                                                          
         LA    RF,ESSIDH                                                        
         GOTO1 =V(NUMVAL),APPARM,ESSID,(X'01',0),RR=APRELO                      
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'65535'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,3,GEKNUM                                                      
*                                                                               
         MVC   APRECKEY(GSKEYL),GSKEY                                           
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK010                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK010    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  EQU   *                                                                
         OI    ESSIDH+(FVOIND-FVIHDR),FVOXMT                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ESSID RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GSKEY,APRECKEY                                                   
         LA    R3,APELEM                                                        
         CLI   APACTN,ACTADD                                                    
         BNE   VRCHG                                                            
*                                                                               
VRADD    MVC   GSKEY,APRECKEY                                                   
         MVC   GEFLEN,=AL2(GSFIRST)                                             
         XC    GEFSTAT(GSFIRST-GSKEYL),GEFSTAT                                  
VRADDX   B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         USING GESSEL,R3                                                        
         LA    R3,APELEM                                                        
         MVI   GESSEL,GESSELQ                                                   
         MVI   GESSELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GESSD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCALU                                                           
         GOTO1 ADELELS,GESSD       DELETE EXISTING ESSID ELEMENT                
*                                                                               
         USING GELUEL,R3                                                        
VRCALU   LA    R3,APELEM                                                        
         MVI   GELUEL,GELUELQ                                                   
         MVI   GELUELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GESSD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCCON                                                           
         GOTO1 ADELELS,GESSD                                                    
*                                                                               
         USING GECOEL,R3                                                        
VRCCON   LA    R3,APELEM                                                        
         MVI   GECOEL,GECOELQ                                                   
         MVI   GECOELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GESSD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCNFS                                                           
         GOTO1 ADELELS,GESSD                                                    
*                                                                               
         USING GENFSEL,R3                                                       
VRCNFS   LA    R3,APELEM                                                        
         MVI   GENFSEL,GENFSELQ                                                 
         MVI   GENFSELL,0                                                       
         DROP  R3                                                               
         GOTO1 AGETELS,GESSD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCHGX                                                           
         GOTO1 ADELELS,GESSD                                                    
*                                                                               
VRCHGX   B     VRDATA                                                           
         EJECT                                                                  
VRDATA   EQU   *                   PROCESS ELEMENT DATA                         
*                                                                               
         USING GESSEL,R3                                                        
VRDDEF   LA    R3,APELEM           PROCESS ESSID DEFINITION ELEMENT             
         XC    APELEM,APELEM                                                    
         MVI   GESSEL,GESSELQ                                                   
         MVI   GESSELL,GESSELLQ                                                 
         MVI   GESSLEV,1                                                        
         MVI   GESSVER,0                                                        
         MVI   GESSSLIM,1                                                       
*                                                                               
VRDLEV   GOTO1 AFVAL,ESSLEVH                                                    
         BNE   VRDVER                                                           
         GOTO1 =V(NUMVAL),APPARM,ESSLEV,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GESSLEV                                                       
*                                                                               
VRDVER   GOTO1 AFVAL,ESSVERH                                                    
         BNE   VRDHCOD                                                          
         GOTO1 =V(NUMVAL),APPARM,ESSVER,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GESSVER                                                       
*                                                                               
VRDHCOD  GOTO1 AFVAL,ESSHCODH                                                   
         BNE   VRDINH                                                           
         MVC   GESSHCOD,FVIFLD                                                  
VRDINH   GOTO1 AFVAL,ESSINHH                                                    
         BNE   VRDNFTP                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDNFTP                                                          
         OI    GESSFLG1,GESSFINQ                                                
VRDNFTP  GOTO1 AFVAL,ESSNFTPH                                                   
         BNE   VRDRCOM                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDRCOM                                                          
         OI    GESSFLG1,GESSFNFQ                                                
VRDRCOM  GOTO1 AFVAL,ESSRCOMH                                                   
         BNE   VRDENID                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDENID                                                          
         OI    GESSFLG1,GESSFRCQ                                                
VRDENID  GOTO1 AFVAL,ESSENIDH                                                   
         BNE   VRDELU                                                           
         MVC   GESSENID,FVIFLD                                                  
VRDELU   GOTO1 AFVAL,ESSELUH                                                    
         BNE   VRDPWD                                                           
         MVC   GESSELU,FVIFLD                                                   
VRDPWD   GOTO1 AFVAL,ESSPWDH                                                    
         BNE   VRDEKEY                                                          
         MVC   GESSPWD,FVIFLD                                                   
VRDEKEY  GOTO1 AFVAL,ESSEKEYH                                                   
         BNE   VRDFAC                                                           
         MVC   GESSEKEY,FVIFLD                                                  
VRDFAC   GOTO1 AFVAL,ESSFACH                                                    
         BNE   VRDCOM                                                           
         MVC   GESSFAC,FVIFLD                                                   
VRDCOM   GOTO1 AFVAL,ESSCOMH                                                    
         BNE   VRDREC                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDREC                                                           
         OI    GESSFLG1,GESSFCOQ                                                
VRDREC   GOTO1 AFVAL,ESSCORH                                                    
         BNE   VRDRBL                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDRBL                                                           
         OI    GESSFLG1,GESSFCRQ                                                
VRDRBL   GOTO1 AFVAL,ESSRBLH                                                    
         BNE   VRDTST                                                           
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'START'                                            
         BNE   VRDRBL02                                                         
         OI    GESSFLG1,GESSFRSQ                                                
         B     VRDTST                                                           
VRDRBL02 ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'RESTART'                                          
         BNE   EIIF                                                             
         OI    GESSFLG1,GESSFRRQ                                                
         B     VRDTST                                                           
VRDTST   GOTO1 AFVAL,ESSTSTH                                                    
         BNE   VRDMETH                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDMETH                                                          
         OI    GESSFLG1,GESSFTSQ                                                
VRDMETH  EQU   *                                                                
         MVI   GESSMETH,C'N'                                                    
*                                                                               
VRDBIN   GOTO1 AFVAL,ESSBINH                                                    
         BNE   VRDSLIM                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDSLIM                                                          
         OI    GESSFLG2,GESSFBIQ                                                
*                                                                               
VRDSLIM  GOTO1 AFVAL,ESSSLIMH                                                   
         BNE   VRDFTPS                                                          
         GOTO1 =V(NUMVAL),APPARM,ESSSLIM,(X'01',0),RR=APRELO                    
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         C     R1,=F'0'                                                         
         BNE   *+8                                                              
         LA    R1,1                                                             
         STC   R1,GESSSLIM                                                      
*                                                                               
VRDFTPS  GOTO1 AFVAL,ESSFTPSH                                                   
         BNE   VRDLOCN                                                          
         GOTO1 =V(NUMVAL),APPARM,ESSFTPS,(X'01',0),RR=APRELO                    
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,15,GESSFTPS                                                   
*                                                                               
VRDLOCN  GOTO1 AFVAL,ESSLOCNH                                                   
         BNE   VRDEPU                                                           
         MVC   GESSLOCN,FVIFLD                                                  
*                                                                               
VRDEPU   GOTO1 AFVAL,ESSEPUH                                                    
         BNE   VRDLINE                                                          
         MVC   GESSEPU,FVIFLD                                                   
*                                                                               
VRDLINE  GOTO1 AFVAL,ESSLINEH                                                   
         BNE   VRDLINEX                                                         
         MVC   GESSLINE,FVIFLD                                                  
VRDLINEX EQU   *                                                                
*                                                                               
VRDATIM  GOTO1 AFVAL,ESSATIMH                                                   
         BNE   VRDATIMX                                                         
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,15,GESSATIM                                                   
VRDATIMX EQU   *                                                                
*                                                                               
VRDTRAC  GOTO1 AFVAL,ESSTRACH                                                   
         BNE   VRDTRACX                                                         
         GOTO1 =V(HEXIN),APPARM,FVIFLD,BYTE,2,0,RR=APRELO                       
         CLC   12(4,R1),=F'1'                                                   
         BNE   EIIF                                                             
         MVC   GESSTRAC,BYTE                                                    
VRDTRACX EQU   *                                                                
*                                                                               
VRDSIM   GOTO1 AFVAL,ESSSIMH                                                    
         BNE   VRDSIMX                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRDSIMX                                                          
         OI    GESSFLG2,GESSFSIQ                                                
VRDSIMX  EQU   *                                                                
*                                                                               
VRD100   GOTO1 AADDELS,GESSD                                                    
*                                                                               
         USING GECOEL,R3                                                        
VRDCON   LA    R3,APELEM           PROCESS ESSID CONTACT ELEMENT                
         XC    APELEM,APELEM                                                    
         MVI   GECOEL,GECOELQ                                                   
         MVI   GECOELL,GECOELLQ                                                 
*                                                                               
VRDPHO   GOTO1 AFVAL,ESSCPHOH                                                   
         BNE   VRDNAME                                                          
         MVC   GECOPHON,FVIFLD                                                  
VRDNAME  GOTO1 AFVAL,ESSCNAMH                                                   
         BNE   VRDCONX                                                          
         MVC   GECONAME,FVIFLD                                                  
*                                                                               
VRDCONX  GOTO1 AADDELS,GESSD                                                    
         DROP  R3                                                               
*                                                                               
         USING GENFSEL,R3                                                       
VRDNFS   LA    R3,APELEM           PROCESS ESSID CONTACT ELEMENT                
         XC    APELEM,APELEM                                                    
         MVI   GENFSEL,GENFSELQ                                                 
         MVI   GENFSELL,GENFELLQ                                                
*                                                                               
VRDNFFR  GOTO1 AFVAL,ESSNFFRH                                                   
         BNE   VRDNFTO                                                          
         MVC   GENFSFR,FVIFLD                                                   
VRDNFTO  GOTO1 AFVAL,ESSNFTOH                                                   
         BNE   VRDNFIP                                                          
         MVC   GENFSTO,FVIFLD                                                   
VRDNFIP  GOTO1 AFVAL,ESSNFIPH                                                   
         BNE   VRDNFSX                                                          
         MVC   GENFSIP,FVIFLD                                                   
*                                                                               
VRDNFSX  GOTO1 AADDELS,GESSD                                                    
         DROP  R3                                                               
*                                                                               
VRD200   EQU   *                                                                
         EJECT                                                                  
*                                  UPDATE RECORD                                
VRUPD    GOTO1 ASETACT,GESSD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APACTN,ACTADD       DON'T UPDATE GENDIR ON ADD                   
         BE    VALRECOK                                                         
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECOK MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ESSID RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         SR    RF,RF                                                            
         ICM   RF,3,GEKNUM                                                      
         EDIT  (RF),(5,ESSID),ZERO=NOBLANK,FILL=0                               
         OI    ESSIDH+(FVOIND-FVIHDR),FVOXMT                                    
         MVI   ESSIDH+FHILD,5                                                   
*                                                                               
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY AN ESSID RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         TWAXC ESSVERH                                                          
         LA    R3,GSFIRST(R2)      GET ELEMENT DATA                             
         MVI   FLDCNT,0                                                         
         LA    R4,BLOCK                                                         
*                                                                               
DR010    CLI   0(R3),0                                                          
         BE    DR100               END OF RECORD                                
         CLI   0(R3),GESSELQ                                                    
         BE    DR030                                                            
         CLI   0(R3),GECOELQ                                                    
         BE    DR050                                                            
         CLI   0(R3),GENFSELQ                                                   
         BE    DR060                                                            
*                                                                               
DR020    SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR010                                                            
*                                                                               
         USING GESSEL,R3                                                        
DR030    XC    APELEM,APELEM                                                    
         MVI   APELEM,GESSELQ                                                   
         GOTO1 AGETELS,GESSD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DR020                                                            
         EDIT  GESSLEV,(3,ESSLEV),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GESSVER,(3,ESSVER),ZERO=NOBLANK,ALIGN=LEFT                       
         MVC   ESSHCOD,GESSHCOD                                                 
         MVC   ESSENID,GESSENID                                                 
         MVC   ESSELU,GESSELU                                                   
         MVC   ESSPWD,GESSPWD                                                   
         MVC   ESSEKEY,GESSEKEY                                                 
         MVC   ESSFAC,GESSFAC                                                   
         MVI   ESSCOM,C'N'                                                      
         TM    GESSFLG1,GESSFCOQ                                                
         BZ    *+8                                                              
         MVI   ESSCOM,C'Y'                                                      
         MVI   ESSCOR,C'N'                                                      
         TM    GESSFLG1,GESSFCRQ                                                
         BZ    *+8                                                              
         MVI   ESSCOR,C'Y'                                                      
         MVI   ESSTST,C'N'                                                      
         TM    GESSFLG1,GESSFTSQ                                                
         BZ    *+8                                                              
         MVI   ESSTST,C'Y'                                                      
         MVI   ESSINH,C'N'                                                      
         TM    GESSFLG1,GESSFINQ                                                
         BZ    *+8                                                              
         MVI   ESSINH,C'Y'                                                      
         MVI   ESSNFTP,C'N'                                                     
         TM    GESSFLG1,GESSFNFQ                                                
         BZ    *+8                                                              
         MVI   ESSNFTP,C'Y'                                                     
         MVI   ESSRCOM,C'N'                                                     
         TM    GESSFLG1,GESSFRCQ                                                
         BZ    *+8                                                              
         MVI   ESSRCOM,C'Y'                                                     
         XC    ESSRBL,ESSRBL                                                    
         TM    GESSFLG1,GESSFRSQ                                                
         BZ    *+10                                                             
         MVC   ESSRBL,=CL8'START'                                               
         TM    GESSFLG1,GESSFRRQ                                                
         BZ    *+10                                                             
         MVC   ESSRBL,=CL8'RESTART'                                             
         MVI   ESSBIN,C'N'                                                      
         TM    GESSFLG2,GESSFBIQ                                                
         BZ    *+8                                                              
         MVI   ESSBIN,C'Y'                                                      
         EDIT  GESSSLIM,(3,ESSSLIM),ZERO=NOBLANK,ALIGN=LEFT                     
         EDIT  GESSFTPS,(8,ESSFTPS),ZERO=NOBLANK,ALIGN=LEFT                     
         MVC   ESSLOCN,GESSLOCN                                                 
         MVC   ESSEPU,GESSEPU                                                   
         MVC   ESSLINE,GESSLINE                                                 
         EDIT  GESSATIM,(8,ESSATIM),ZERO=NOBLANK,ALIGN=LEFT                     
         GOTO1 =V(HEXOUT),APPARM,GESSTRAC,ESSTRAC,1,=C'TOG',RR=APRELO           
         MVI   ESSSIM,C'N'                                                      
         TM    GESSFLG2,GESSFSIQ                                                
         BZ    *+8                                                              
         MVI   ESSSIM,C'Y'                                                      
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
         USING GECOEL,R3                                                        
DR050    EQU   *                   ESS CONTACT ELEMENT                          
         MVC   ESSCPHO,GECOPHON                                                 
         MVC   ESSCNAM,GECONAME                                                 
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
         USING GENFSEL,R3                                                       
DR060    EQU   *                   ESS NFS MOUNT INFO ELEMENT                   
         MVC   ESSNFFR,GENFSFR                                                  
         MVC   ESSNFTO,GENFSTO                                                  
         MVC   ESSNFIP,GENFSIP                                                  
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
DR100    EQU   *                                                                
*                                                                               
DR200    GOTO1 ADISACT,GESSD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                   SAVE LAST DISPLAYED SYSTEM                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN ESSID RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OI    GSDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GESSD                                                    
         OI    GEFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED ESSID RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GSDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GESSD                                                    
         NI    GEFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    GSKEY,GSKEY                                                      
         MVI   GSKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GSKREC,GSKRECQ                                                   
         XC    SELKEY,SELKEY                                                    
*                                                                               
VSELEID  EQU   *                                                                
         GOTO1 AFVAL,LSTEIDH       VALIDATE ESS ID                              
         BNE   VSEIDX                                                           
         LA    RF,LSTEIDH                                                       
         GOTO1 =V(NUMVAL),APPARM,LSTEID,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'65535'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,3,GEKNUM                                                      
VSEIDX   EQU   *                                                                
*                                                                               
VSELHCO  EQU   *                                                                
         GOTO1 AFVAL,LSTHCODH                                                   
         BNE   VSHCOX                                                           
         MVC   SELHCOD,FVIFLD                                                   
VSHCOX   EQU   *                                                                
*                                                                               
VSELINH  EQU   *                                                                
         GOTO1 AFVAL,LSTINHH                                                    
         BNE   VSINHX                                                           
         MVC   SELINH,FVIFLD                                                    
VSINHX   EQU   *                                                                
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   GSKEY,APRECKEY                                                   
         CLI   GSKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GSKMAJ,0                                                         
         B     GETSEL6             READ HIGH                                    
GETSEL2  TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         LA    R2,IOKEY                                                         
         CLI   GSKREC,GSKRECQ      CHECK STILL ESSID RECORD                     
         BNE   GETSELN                                                          
* ??     CLI   GXSKAGY,0           CHECK STILL ESSID RECORD                     
         BNE   GETSEL8                                                          
         SPACE 1                                                                
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
GSELEM   EQU   *                   GET ELEMENT DATA                             
         LA    R3,GSFIRST(R2)                                                   
         SR    RF,RF                                                            
         USING GESSEL,R3                                                        
GSEM010  CLI   GESSEL,0                                                         
         BE    GSEMX                                                            
         CLI   GESSEL,GESSELQ                                                   
         BE    GSSEL                                                            
GSEM020  IC    RF,GESSELL                                                       
         AR    R3,RF                                                            
         B     GSEM010                                                          
*                                                                               
GSSEL    EQU   *                                                                
GSHCOD   EQU   *                                                                
         OC    SELHCOD,SELHCOD                                                  
         BZ    GSHCOX                                                           
         CLC   SELHCOD,GESSHCOD                                                 
         BNE   GETSEL8                                                          
GSHCOX   EQU   *                                                                
*                                                                               
GSINH    EQU   *                                                                
         OC    SELINH,SELINH                                                    
         BZ    GSINHX                                                           
         CLI   SELINH,C'Y'                                                      
         BNE   GSINH02                                                          
         TM    GESSFLG1,GESSFINQ                                                
         BZ    GETSEL8                                                          
         B     GSINHX                                                           
GSINH02  CLI   SELINH,C'N'                                                      
         BNE   GSINHX                                                           
         TM    GESSFLG1,GESSFINQ                                                
         BNZ   GETSEL8                                                          
         B     GSINHX                                                           
GSINHX   EQU   *                                                                
*                                                                               
GSEMX    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GETSELY  MVC   APRECKEY(L'GSKEY),GSKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                  GET DATA FROM RECORD KEY                     
         SR    RF,RF                                                            
         ICM   RF,3,GEKNUM                                                      
         EDIT  (RF),(5,LISTENUM),ZERO=NOBLANK,FILL=0                            
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GSFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GESSELQ                                                    
         BE    DSLDEF                                                           
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GESSEL,R3                                                        
DSLDEF   EQU   *                                                                
         MVC   LISTELU,GESSELU                                                  
         MVC   LISTLOCN,GESSLOCN                                                
         MVC   LISTHCOD,GESSHCOD                                                
         MVI   LISTINH,C'N'                                                     
         TM    GESSFLG1,GESSFINQ                                                
         BZ    *+8                                                              
         MVI   LISTINH,C'Y'                                                     
         B     DSLP1A                                                           
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    SELKEY,SELKEY       SELECTION CRITERION                          
         XC    APRECKEY,APRECKEY                                                
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
*                                                                               
VRQ50    LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   GSKREC,GSKRECQ                                                   
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE MESSAGE REPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
PR010    LA    R1,IOHI+IOGENDIR+IO1                                             
         B     *+8                                                              
PR020    LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         CLI   GSKREC,GSKRECQ                                                   
         BNE   PRTREPX                                                          
*                                                                               
PR040    GOTO1 VREPORT,REPD                                                     
         B     PR020                                                            
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         EJECT                                                                  
*                                  ERROR EXITS                                  
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ERAE     MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
ESYS     MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  SYSTEM NAME ERROR                            
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'MESSAGE LIST'                                                  
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'MESSAGE RECORD LIST'                                     
         SPEC  H2,57,C'-------------------'                                     
         SPEC  M1,24,C'MESSAGE'                                                 
         SPEC  M2,1,C'SYSTEM  LANGUAGE      REFERENCE MESSAGE TEXT'             
         SPEC  M3,1,C'------  --------      --------- ------------'             
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTESSWRK                                                                      
       ++INCLUDE CTESSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSFDD                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSDDD                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSBDD                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT DISPLAYED SYSTEM                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTENUM DS    CL5                                                              
         DS    CL1                                                              
LISTHCOD DS    CL1                                                              
         DS    CL2                                                              
LISTINH  DS    CL1                                                              
         DS    CL2                                                              
LISTELU  DS    CL8                                                              
         DS    CL1                                                              
LISTLOCN DS    CL30                                                             
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
PRTSYS   DS    CL7                                                              
         DS    CL1                                                              
PRTLANG  DS    CL13                                                             
         DS    CL1                                                              
PRTREF   DS    CL8                                                              
         DS    CL2                                                              
PRTMSG   DS    CL(L'REPP1-(PRTMSG-REPP1))                                       
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
IOKEYSV  DS    XL(L'IOKEY)                                                      
ASE      DS    A                                                                
APGM     DS    A                                                                
BYTE     DS    XL1                                                              
*                                                                               
FLDCNT   DS    XL1                 SCREEN FIELD COUNTER                         
COUNT    DS    XL1                 COUNTER                                      
*                                                                               
SELKEY   DS    0XL32                                                            
SELHCOD  DS    CL1                 HOSTESS CODE                                 
SELINH   DS    CL1                 INHIBIT ON FLAG                              
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
BLOCK    DS    20CL32              SCANNER BLOCKS                               
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074CTESS02   11/11/08'                                      
         END                                                                    
