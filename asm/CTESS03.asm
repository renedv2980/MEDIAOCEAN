*          DATA SET CTESS03    AT LEVEL 067 AS OF 09/21/16                      
*PHASE TA0E03A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*                                                                               
         TITLE 'CTESS03 - FILE MAINTENANCE - EXTRACT AGENCY RECORDS'            
ESS03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ES03**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(RECORD KEY)                             
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
* ROUTINE TO VALIDATE KEY OF XAGENCY DEFINITION RECORD                *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(XAGENCY RECORD KEY)                     
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAKRECQ     RECORD TYPE                                  
         MVI   FVMINL,2                                                         
         GOTO1 VALAGY,XAGAGYH      VALIDATE AGENCY ALPHA ID                     
         BNE   VALKEYX                                                          
         MVC   GXAKAGY,FVIFLD                                                   
*                                                                               
VKSYS    MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,XAGSYSH                                                    
         BNE   VALKEYX                                                          
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VKSY010  CLI   SYSLNUM,0                                                        
         BE    VKSY012                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSY010                                                          
*                                                                               
VKSY012  LA    RE,SYSLEX           CHECK IN EXTENDED SYSTEM TABLE               
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VKSY014  CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSY014                                                          
*                                                                               
VKSY020  MVC   GXAKSYS,SYSLNUM     SET SYSTEM NUMBER FROM LIST                  
         MVC   XAGSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    XAGSYSH+6,X'80'                                                  
*                                                                               
VKSUB    EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 VALSUB,XAGSUBH                                                   
         BNE   EIIF                                                             
         MVC   GXAKSUB,APHALF                                                   
         MVC   XAGSUB,APWORK                                                    
*                                  READ RECORD                                  
VKREAD   MVC   APRECKEY(GXKEYL),GXKEY                                           
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VKR010                                                           
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VKR010   LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  EQU  *                                                                 
*                                  UPDATE SCREEN KEY FIELDS                     
         OI    XAGAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XAGSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XAGSUBH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A XAGENCY RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GXKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BNE   VRCHG                                                            
*                                                                               
VRADD    MVC   GXKEY,APRECKEY                                                   
         MVC   GXFLEN,=AL2(GXFIRST)                                             
         XC    GXFSTAT(GXFIRST-GXKEYL),GXFSTAT                                  
VRADDX   B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         USING GXSDEL,R3                                                        
         LA    R3,APELEM           SAVE ANY EXISTING SYSTEM ELEMENT             
         MVI   GXSDEL,GXSDELQ                                                   
         MVI   GXSDELL,2                                                        
         MVC   GXSDSYS,GXAKSYS                                                  
         MVC   GXSDSUB,GXAKSUB                                                  
         DROP  R3                                                               
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCHG10                                                          
         GOTO1 ADELELS,GXTRD       DELETE EXISTING SYSTEM ELEMENT               
*                                                                               
         USING GXGNEL,R3                                                        
VRCHG10  LA    R3,APELEM                                                        
         MVI   GXGNEL,GXGNELQ                                                   
         MVI   GXGNELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCHG20                                                          
         GOTO1 ADELELS,GXTRD                                                    
*                                                                               
         USING GXPCEL,R3                                                        
VRCHG20  LA    R3,APELEM           GET ANY EXISTING PASSWORD CRYTPT EL          
         MVI   GXPCEL,GXPCELQ                                                   
         MVI   GXPCELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCHG30                                                          
         GOTO1 ADELELS,GXTRD       DELETE EXISTING PWD CRYPT EL                 
*                                                                               
         USING GXBDEL,R3                                                        
VRCHG30  LA    R3,APELEM           GET ANY EXISTING BDE ELEMENT                 
         MVI   GXBDEL,GXBDELQ                                                   
         MVI   GXBDELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCHG40                                                          
         GOTO1 ADELELS,GXTRD       DELETE EXISTING DB ELEMENT                   
*                                                                               
         USING GXDBEL,R3                                                        
VRCHG40  LA    R3,APELEM           GET ANY EXISTING DB ELEMENT                  
         MVI   GXDBEL,GXDBELQ                                                   
         MVI   GXDBELL,0                                                        
         DROP  R3                                                               
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R1,15,APPARM                                                     
         BZ    VRCHGX                                                           
         GOTO1 ADELELS,GXTRD       DELETE EXISTING DB ELEMENT                   
VRCHGX   B     VRDATA                                                           
         EJECT                                                                  
*                                                                               
VRDATA   EQU   *                                                                
*                                  PROCESS SYSTEM ELEMENT                       
         USING GXSDEL,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GXSDEL,GXSDELQ      INITIALISE SYSTEM ELEMENT                    
         MVI   GXSDELL,GXSDEXLQ                                                 
         MVC   GXSDSYS,GXAKSYS                                                  
         MVC   GXSDSUB,GXAKSUB                                                  
         MVI   GXSDLEV,1                                                        
         MVI   GXSDVER,0                                                        
         MVI   GXSDTHD,0                                                        
         MVC   GXSDTYP,=CL3'ALL'                                                
         MVC   GXSDFLT1,SPACES                                                  
         MVC   GXSDFLT2,SPACES                                                  
         MVC   GXSDFLT3,SPACES                                                  
         MVC   GXSDPSZ,=AL3(10)                                                 
         MVC   GXSDSSZ,=AL3(10)                                                 
         MVI   GXSDFCOD,GXSDFCEQ                                                
         CLI   GXAKSYS,X'01'       TEST IF SERVICE SYSTEM                       
         BE    VRD008                                                           
         CLI   GXAKSYS,X'FF'       TEST IF PQ SYSTEM                            
         BE    VRD008                                                           
         CLI   GXAKSYS,X'FE'       TEST IF WORKER SYSTEM                        
         BE    VRD008                                                           
         B     VRD010                                                           
*                                                                               
VRD008   MVI   GXSDFCOD,GXSDFCPQ                                                
*                                                                               
VRD010   GOTO1 AFVAL,XAGLEVH                                                    
         BNE   VRD015                                                           
         GOTO1 =V(NUMVAL),APPARM,XAGLEV,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GXSDLEV                                                       
*                                                                               
VRD015   GOTO1 AFVAL,XAGTHDH                                                    
         BNE   VRD020                                                           
         GOTO1 =V(NUMVAL),APPARM,XAGTHD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         STC   R1,GXSDTHD                                                       
*                                                                               
VRD020   GOTO1 AFVAL,XAGVERH                                                    
         BNE   VRD030                                                           
         GOTO1 =V(NUMVAL),APPARM,XAGVER,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GXSDVER                                                       
*                                                                               
VRD030   GOTO1 AFVAL,XAGTYPH                                                    
         BNE   VRD040                                                           
         MVC   GXSDTYP(3),FVIFLD                                                
*                                                                               
VRD040   GOTO1 AFVAL,XAGFLTSH                                                   
         BNE   VRD050                                                           
         MVC   GXSDFLT1,FVIFLD                                                  
         MVC   GXSDFLT2,FVIFLD+1                                                
         MVC   GXSDFLT3,FVIFLD+2                                                
*                                                                               
VRD050   GOTO1 AFVAL,XAGPSZH                                                    
         BNE   VRD060                                                           
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         SR    RF,RF                                                            
         ICM   RF,7,APALLOC                                                     
         CR    R1,RF                                                            
         BH    EIIF                                                             
         C     R1,=F'1'                                                         
         BL    EIIF                                                             
         STCM  R1,7,GXSDPSZ                                                     
*                                                                               
VRD060   GOTO1 AFVAL,XAGSSZH                                                    
         BNE   VRD070                                                           
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         SR    RF,RF                                                            
         ICM   RF,7,ASALLOC                                                     
         CR    R1,RF                                                            
         BH    EIIF                                                             
         C     R1,=F'1'                                                         
         BL    EIIF                                                             
         STCM  R1,7,GXSDSSZ                                                     
*                                                                               
VRD070   GOTO1 AFVAL,XAGFCODH                                                   
         BNE   VRD080                                                           
         CLI   GXAKSYS,X'01'       TEST IF SERVICE SYSTEM                       
         BE    VRD072                                                           
         CLI   GXAKSYS,X'FF'       TEST IF PQ SYSTEM                            
         BE    VRD072                                                           
         CLI   GXAKSYS,X'FE'       TEST IF WORKER SYSTEM                        
         BE    VRD072                                                           
         CLI   FVIFLD,GXSDFCEQ                                                  
         BE    VRD074                                                           
         CLI   FVIFLD,GXSDFCDQ                                                  
         BE    VRD074                                                           
         CLI   FVIFLD,GXSDFCLQ                                                  
         BE    VRD074                                                           
         CLI   FVIFLD,GXSDFCPQ                                                  
         BE    VRD074                                                           
         CLI   FVIFLD,GXSDFCNQ                                                  
         BE    VRD074                                                           
         B     EIIF                                                             
VRD072   CLI   FVIFLD,GXSDFCPQ                                                  
         BE    VRD074                                                           
         CLI   FVIFLD,GXSDFCNQ                                                  
         BE    VRD074                                                           
         B     EIIF                                                             
VRD074   MVC   GXSDFCOD,FVIFLD                                                  
*                                                                               
VRD080   GOTO1 AFVAL,XAGFTIMH                                                   
         BNE   VRD090                                                           
         CLI   GXSDFCOD,GXSDFCEQ                                                
         BNE   EIIF                                                             
         LA    RF,L'XAGFTIM                                                     
         GOTO1 =V(TIMBER),APPARM,(X'C0',(RF)),(X'02',APHALF),XAGFTIM,  +        
               RR=APRELO                                                        
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         MVC   GXSDFTIM,APHALF                                                  
*                                                                               
VRD090   GOTO1 AFVAL,XAGTSTH                                                    
         BNE   VRD100                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRD100                                                           
         OI    GXSDFLG1,GXSDFTSQ                                                
*                                                                               
VRD100   GOTO1 AFVAL,XAGLOCKH                                                   
         BNE   VRD110                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRD110                                                           
         OI    GXSDFLG1,GXSDFLOQ                                                
*                                                                               
VRD110   GOTO1 AFVAL,XAGDATRH                                                   
         BNE   VRD120                                                           
         LA    R4,WORK                                                          
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         USING PERVALD,R4                                                       
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(0,(R4))                            
         CLI   4(R1),0                                                          
         BNE   EIIF                                                             
         MVC   GXSDFDAT,PVALBSTA                                                
         MVC   GXSDTDAT,PVALBEND                                                
         DROP  R4                                                               
*                                                                               
VRD120   GOTO1 AFVAL,XAGMLOGH                                                   
         BNE   VRD130                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRD130                                                           
         OI    GXSDFLG1,GXSDFMLQ                                                
*                                                                               
VRD130   GOTO1 AFVAL,XAGQTRBH                                                   
         BNE   VRD140                                                           
         GOTO1 =V(NUMVAL),APPARM,XAGQTRB,(X'01',0),RR=APRELO                    
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GXSDQTRB                                                      
*                                                                               
VRD140   GOTO1 AFVAL,XAGQTRFH                                                   
         BNE   VRD150                                                           
         GOTO1 =V(NUMVAL),APPARM,XAGQTRF,(X'01',0),RR=APRELO                    
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GXSDQTRF                                                      
*                                                                               
VRD150   GOTO1 AFVAL,XAGMXFCH                                                   
         BNE   VRD200                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRD200                                                           
         OI    GXSDFLG1,GXSDFXCQ                                                
*                                                                               
VRD200   OI    GXSDHDR,X'01'                                                    
         GOTO1 AADDELS,GXTRD       ADD SYSTEM EL                                
         DROP  R3                                                               
*                                  PROCESS FILE GENERATION NUMBER               
         USING GXGNEL,R3           ELEMENT                                      
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GXGNEL,GXGNELQ      INITIALISE ELEMENT                           
         MVI   GXGNELL,GXGNELLQ                                                 
         XC    GXGNNUM,GXGNNUM                                                  
         CLI   APACTN,ACTADD                                                    
         BE    VRD210                                                           
         GOTO1 AFVAL,XAGGNUMH                                                   
         BNE   VRD210                                                           
         GOTO1 =V(NUMVAL),APPARM,FVIFLD,(X'02',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,3,GXGNNUM                                                     
*                                                                               
VRD210   EQU   *                                                                
         GOTO1 AADDELS,GXTRD       ADD ELEMENT                                  
         DROP  R3                                                               
*                                  PROCESS PASSWORD ENCRYPT ELEMENT             
         USING GXPCEL,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GXPCEL,GXPCELQ      INITIALISE ELEMENT                           
         MVI   GXPCELL,GXPCELLQ                                                 
*                                                                               
         GOTO1 AFVAL,XAGEKEYH                                                   
         BNE   VRD220                                                           
         MVC   GXPCKEY,FVIFLD                                                   
*                                                                               
         GOTO1 AADDELS,GXTRD       ADD PASSWORD ENCRYPT ELEMENT                 
         DROP  R3                                                               
                                                                                
*----------------------------------                                             
* PROCESS BDE ELEMENT                                                           
*----------------------------------                                             
VRD220   GOTO1 AFVAL,XAGBDEEH      IF THERE IS ONE FIELD OF BDE DATA            
         BE    VRD222              THEN WE ALL BDE FIELDS REQUIRED              
         GOTO1 AFVAL,XAGBDEUH                                                   
         BE    VRD222                                                           
         GOTO1 AFVAL,XAGBDEFH                                                   
         BE    VRD222                                                           
         GOTO1 AFVAL,XAGBDESH                                                   
         BNE   VRD230              ELSE, ALL MUST BE EMPTY                      
*                                                                               
         USING GXBDEL,R3                                                        
VRD222   LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GXBDEL,GXBDELQ      INITIALISE ELEMENT                           
         MVI   GXBDELL,GXBDELLQ                                                 
*                                                                               
         GOTO1 AFVAL,XAGBDEEH                                                   
         BNE   EMIF                                                             
         MVC   GXBDEDI,FVIFLD                                                   
*                                                                               
         GOTO1 AFVAL,XAGBDEUH                                                   
         BNE   EMIF                                                             
         MVC   UIDNAME,FVIFLD                                                   
         BRAS  RE,GETUIN                                                        
         BNE   VALKEYX                                                          
         MVC   GXBDUIN,UIDNUM                                                   
*                                                                               
         GOTO1 AFVAL,XAGBDEFH                                                   
         BNE   EMIF                                                             
         MVC   GXBDFIL,FVIFLD                                                   
*                                                                               
         GOTO1 AFVAL,XAGBDESH                                                   
         BNE   EMIF                                                             
         MVC   GXBDSUB,FVIFLD                                                   
*                                                                               
         GOTO1 AADDELS,GXTRD       ADD BDE ELEMENT                              
         DROP  R3                                                               
                                                                                
*----------------------------------                                             
* PROCESS DB ELEMENT                                                            
*----------------------------------                                             
         USING GXDBEL,R3                                                        
VRD230   LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GXDBEL,GXDBELQ      INITIALISE ELEMENT                           
         MVI   GXDBELL,GXDBELLQ                                                 
*                                                                               
         GOTO1 AFVAL,XAGDBLCH                                                   
         BNE   VRD300                                                           
         MVC   GXDBLOC,FVIFLD                                                   
*                                                                               
         GOTO1 AFVAL,XAGDBIDH                                                   
         BNE   VRD300                                                           
         MVC   GXDBID,FVIFLD                                                    
*                                                                               
         GOTO1 AFVAL,XAGDBPWH                                                   
         BNE   VRD300                                                           
         MVC   GXDBPWD,FVIFLD                                                   
*                                                                               
         GOTO1 AADDELS,GXTRD       ADD DB ELEMENT                               
         DROP  R3                                                               
VRD300   EQU   *                                                                
                                                                                
*----------------------------------                                             
* UPDATE RECORD                                                                 
*----------------------------------                                             
VRUPD    GOTO1 ASETACT,GXTRD       DEFINE ACTIVITY ELEMENT                      
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
* ROUTINE TO DISPLAY KEY OF XAGENCY RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   XAGAGY,GXAKAGY                                                   
         GOTO1 ADISSYS,GXAKSYS                                                  
         MVC   XAGSYS,APWORK                                                    
         GOTO1 DISSUB,GXAKSUB                                                   
         MVC   XAGSUB,APWORK                                                    
         OI    XAGAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XAGSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XAGSUBH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   AGYALPH,GXAKAGY                                                  
         MVC   SYSTEM,GXAKSYS                                                   
*                                  GET ACCESS RECORD INTO IOAREA3               
         L     RF,=A(GETACC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   DISKEYX                                                          
         MVC   XAGSEN,SENSAVE                                                   
         OI    XAGSENH+(FVOIND-FVIHDR),FVOXMT                                   
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY XAGENCY RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC XAGVERH                                                          
*                                                                               
         OI    XAGSENH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XAGGNUMH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         OI    XAGGNUMH+(FVATRB-FVIHDR),FVAPROT                                 
         CLI   OPTRST,C'Y'         RESET=Y OPTION - UNPROTECT THIS              
         BNE   *+8                                                              
         NI    XAGGNUMH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
*                                                                               
         LA    R3,APELEM                                                        
         USING GXSDEL,R3                                                        
         MVI   GXSDEL,GXSDELQ                                                   
         MVI   GXSDELL,2                                                        
         MVC   GXSDSYS,GXAKSYS                                                  
         MVC   GXSDSUB,GXAKSUB                                                  
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC020                                                          
         EDIT  GXSDTHD,(3,XAGTHD),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GXSDLEV,(3,XAGLEV),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GXSDVER,(3,XAGVER),ZERO=NOBLANK,ALIGN=LEFT                       
         MVC   XAGTYP,GXSDTYP                                                   
         MVC   XAGFLTS(1),GXSDFLT1                                              
         MVC   XAGFLTS+1(1),GXSDFLT2                                            
         MVC   XAGFLTS+2(1),GXSDFLT3                                            
         XC    APWORK,APWORK                                                    
         EDIT  GXSDPSZ,(8,XAGPSZ),ZERO=BLANK,ALIGN=LEFT                         
         EDIT  GXSDSSZ,(8,XAGSSZ),ZERO=BLANK,ALIGN=LEFT                         
         MVC   XAGFCOD,GXSDFCOD                                                 
         CLI   GXSDFCOD,GXSDFCEQ                                                
         BNE   DREC010                                                          
         B     DREC010                                                          
         LA    RF,L'XAGFTIM                                                     
         GOTO1 =V(TIMBER),APPARM,(X'40',(RF)),(X'02',GXSDFTIM),        +        
               (4,XAGFTIM),RR=APRELO                                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
*                                                                               
DREC010  EQU   *                                                                
         MVI   XAGTST,C'N'                                                      
         MVI   XAGLOCK,C'N'                                                     
         MVI   XAGMLOG,C'N'                                                     
         MVI   XAGMXFC,C'N'                                                     
         CLI   GXSDELL,GXSDEXLQ                                                 
         BNE   DREC020                                                          
         TM    GXSDFLG1,GXSDFTSQ                                                
         BZ    *+8                                                              
         MVI   XAGTST,C'Y'                                                      
         TM    GXSDFLG1,GXSDFLOQ                                                
         BZ    *+8                                                              
         MVI   XAGLOCK,C'Y'                                                     
         TM    GXSDFLG1,GXSDFMLQ                                                
         BZ    *+8                                                              
         MVI   XAGMLOG,C'Y'                                                     
         TM    GXSDFLG1,GXSDFXCQ                                                
         BZ    *+8                                                              
         MVI   XAGMXFC,C'Y'                                                     
         OC    GXSDFDAT(L'GXSDFDAT+L'GXSDTDAT),GXSDFDAT                         
         BZ    DREC015                                                          
         GOTO1 VDATCON,APPARM,(X'13',GXSDFDAT),(X'0A',XAGDATR)                  
*                                                                               
DREC015  OC    GXSDQTRB,GXSDQTRB                                                
         BZ    DREC016                                                          
         EDIT  GXSDQTRB,(2,XAGQTRB),ZERO=NOBLANK,ALIGN=LEFT                     
DREC016  OC    GXSDQTRF,GXSDQTRF                                                
         BZ    DREC020                                                          
         EDIT  GXSDQTRF,(2,XAGQTRF),ZERO=NOBLANK,ALIGN=LEFT                     
*                                                                               
DREC020  EQU   *                                                                
         LA    R3,APELEM                                                        
         USING GXGNEL,R3                                                        
         MVI   GXGNEL,GXGNELQ                                                   
         MVI   GXGNELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC030                                                          
         SR    RF,RF                                                            
         ICM   RF,3,GXGNNUM                                                     
         EDIT  (RF),(5,XAGGNUM),ZERO=NOBLANK,ALIGN=LEFT                         
         DROP  R3                                                               
*                                                                               
DREC030  EQU   *                                                                
         LA    R3,APELEM                                                        
         USING GXPCEL,R3                                                        
         MVI   GXPCEL,GXPCELQ                                                   
         MVI   GXPCELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC040                                                          
         MVC   XAGEKEY,GXPCKEY                                                  
         DROP  R3                                                               
*                                                                               
DREC040  EQU   *                                                                
         LA    R3,APELEM                                                        
         USING GXBDEL,R3                                                        
         MVI   GXBDEL,GXBDELQ                                                   
         MVI   GXBDELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC050                                                          
         MVC   XAGBDEE,GXBDEDI                                                  
         MVC   XAGBDEF,GXBDFIL                                                  
         MVC   XAGBDES,GXBDSUB                                                  
         MVC   UIDNUM,GXBDUIN                                                   
         L     RF,=A(GETUID)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   DISRECX                                                          
         MVC   XAGBDEU,UIDNAME                                                  
         DROP  R3                                                               
*                                                                               
DREC050  EQU   *                                                                
         LA    R3,APELEM                                                        
         USING GXDBEL,R3                                                        
         MVI   GXDBEL,GXDBELQ                                                   
         MVI   GXDBELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC100                                                          
         MVC   XAGDBLC,GXDBLOC                                                  
         MVC   XAGDBID,GXDBID                                                   
         MVC   XAGDBPW,GXDBPWD                                                  
         DROP  R3                                                               
DREC100  EQU   *                                                                
*                                                                               
         GOTO1 ADISACT,GXTRD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN XAGENCY RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
DELREC   EQU   *                                                                
         MVC   AGYALPH,GXAKAGY                                                  
         MVC   SYSTEM,GXAKSYS                                                   
         MVC   SUBSYS,GXAKSUB                                                   
         MVC   IOKEYSV,IOKEY                                                    
         LA    R2,IOKEY            CHECK NO SERVER RECORDS SET UP               
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSARECQ                                                  
         MVC   GXSAAGY,AGYALPH                                                  
         MVC   GXSASYS,SYSTEM                                                   
         MVC   GXSASUB,SUBSYS                                                   
         MVI   GXSAEID+L'GXSAEID-1,X'01'                                        
         GOTO1 AIO,IOGENDIR+IOHIGH+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GXSAAGY,AGYALPH                                                  
         BNE   DELR010                                                          
         CLC   GXSASYS,SYSTEM                                                   
         BNE   DELR010                                                          
         CLC   GXSASUB,SUBSYS                                                   
         BNE   DELR010                                                          
         B     ECDX                                                             
*                                                                               
DELR010  MVC   IOKEY,IOKEYSV                                                    
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GXTRD                                                    
         OI    GXFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED XAGENCY RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GXDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GXTRD                                                    
         NI    GXFSTAT,X'FF'-X'80' UNSET DELETE                                 
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
         XC    GXKEY,GXKEY                                                      
         MVI   GXKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GXKREC,GXAKRECQ                                                  
         XC    SELKEY,SELKEY                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VSELAGY  EQU   *                                                                
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VSAGYX                                                           
         GOTO1 VALAGY,LSTAGYH                                                   
         BNE   EIIF                                                             
         MVC   SELAGY,FVIFLD                                                    
         MVC   GXAKAGY,SELAGY                                                   
VSAGYX   EQU   *                                                                
*                                                                               
VSELTHD  EQU   *                                                                
         MVI   SELTHDC,C'N'                                                     
         GOTO1 AFVAL,LSTTHDH                                                    
         BNE   VSTHDX                                                           
         GOTO1 =V(NUMVAL),APPARM,LSTTHD,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         STC   R1,SELTHD                                                        
         MVI   SELTHDC,C'Y'                                                     
         OI    LSTTHDH+6,X'80'                                                  
VSTHDX   EQU   *                                                                
*                                                                               
*                                                                               
VSELSYS  EQU   *                                                                
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VSSYSX                                                           
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VSSY010  CLI   SYSLNUM,0                                                        
         BE    VSSY012                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VSSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VSSY010                                                          
*                                                                               
VSSY012  LA    RE,SYSLEX           CHECK IN EXTENDED SYSTEM TABLE               
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VSSY014  CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VSSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VSSY014                                                          
*                                                                               
VSSY020  MVC   SELSYS,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   GXAKSYS,SELSYS                                                   
         MVC   LSTSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    LSTSYSH+6,X'80'                                                  
VSSYSX   EQU   *                                                                
*                                                                               
VSELSUB  EQU   *                                                                
         GOTO1 AFVAL,LSTSUBH                                                    
         BNE   VSSUBX                                                           
         GOTO1 VALSUB,LSTSUBH                                                   
         BNE   EIIF                                                             
         MVC   SELSUB,APHALF                                                    
         MVC   GXAKSUB,SELSUB                                                   
         MVC   LSTSUB,APWORK                                                    
         OI    LSTSUBH+6,X'80'                                                  
VSSUBX   EQU   *                                                                
*                                                                               
VSELSEN  EQU   *                                                                
         GOTO1 AFVAL,LSTSENH                                                    
         BNE   VSSENX                                                           
         GOTO1 AVALSE,LSTSENH                                                   
         BNE   VALSELX                                                          
         L     R1,APPARM                                                        
         USING SELISTD,R1                                                       
         MVC   SELSEN,SENAME                                                    
         MVC   LSTSEN(7),SENAME    DISPLAY FULL SE NAME                         
         OI    LSTSENH+6,X'80'                                                  
         DROP  R1                                                               
VSSENX   EQU   *                                                                
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   GETSEQF,0                                                        
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
         MVC   GXKEY,APRECKEY                                                   
         CLI   GXKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GXKMAJ,0                                                         
         B     GETSEL6             READ HIGH                                    
GETSEL2  EQU   *                                                                
         TM    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         BZ    GETSEL2A                                                         
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GETSEL2B                                                         
GETSEL2A TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
GETSEL2B EQU   *                                                                
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
         CLI   GXKREC,GXAKRECQ     CHECK STILL XAGENCY RECORD                   
         BNE   GETSELN                                                          
         SPACE 1                                                                
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
GSAGY    EQU   *                                                                
         OC    SELAGY,SELAGY                                                    
         BZ    GSAGYX                                                           
         CLC   SELAGY,GXAKAGY                                                   
         BNE   GETSEL8                                                          
GSAGYX   EQU   *                                                                
*                                                                               
GSTHD    EQU   *                                                                
         CLI   SELTHDC,C'N'                                                     
         BE    GSTHDX                                                           
*                                                                               
         SR    R0,R0                                                            
         LA    R3,GXFIRST(R2)                                                   
GSTHD10  CLI   0(R3),0             E-O-R                                        
         BE    GSTHDX                                                           
         CLI   0(R3),GXSDELQ                                                    
         BE    GSTHD20                                                          
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     GSTHD10                                                          
*                                                                               
         USING GXSDEL,R3                                                        
GSTHD20  CLI   GXSDELL,GXSDEXLQ    EXTENDED LENGTH?                             
         BE    GSTHD30             YES-TRY MATCH ON THREAD#                     
         CLI   SELTHD,X'00'        NO-CAN ONLY MATCH ON THREAD 0                
         BE    GSTHDX                                                           
         B     GETSEL8                                                          
*                                                                               
GSTHD30  CLC   SELTHD,GXSDTHD      MATCH ON THREAD#                             
         BNE   GETSEL8             NO                                           
         DROP  R3                                                               
GSTHDX   EQU   *                                                                
*                                                                               
GSSYS    EQU   *                                                                
         OC    SELSYS,SELSYS                                                    
         BZ    GSSYSX                                                           
         CLC   SELSYS,GXAKSYS                                                   
         BNE   GETSEL8                                                          
GSSYSX   EQU   *                                                                
*                                                                               
GSSUB    EQU   *                                                                
         OC    SELSUB,SELSUB                                                    
         BZ    GSSUBX                                                           
         CLC   SELSUB,GXAKSUB                                                   
         BNE   GETSEL8                                                          
GSSUBX   EQU   *                                                                
*                                                                               
GSSEN    EQU   *                                                                
         OC    SELSEN,SELSEN                                                    
         BZ    GSSENX                                                           
         MVC   AGYALPH,GXAKAGY                                                  
         MVC   SYSTEM,GXAKSYS                                                   
*                                  GET ACCESS RECORD INTO IOAREA3               
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
*        OI    APINDS,APILRERD                                                  
         L     RF,=A(GETACC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSEL2                                                          
         CLC   SELSEN,SENSAVE                                                   
         BNE   GETSEL2                                                          
GSSENX   EQU   *                                                                
*                                                                               
GETSELY  EQU   *                                                                
         LA    R2,IOKEY                                                         
         MVC   APRECKEY(L'GXKEY),GXKEY                                          
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
         MVC   LISTAGY,GXAKAGY                                                  
         GOTO1 ADISSYS,GXAKSYS                                                  
         MVC   LISTSYS,APWORK                                                   
         GOTO1 DISSUB,GXAKSUB                                                   
         MVC   LISTSUB,APWORK                                                   
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GXFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GXSDELQ                                                    
         BE    DSLAPP                                                           
         CLI   0(R3),GXGNELQ                                                    
         BE    DSLGNM                                                           
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GXSDEL,R3                                                        
DSLAPP   EQU   *                                                                
         CLC   GXSDSYS,GXAKSYS                                                  
         BNE   DSLP1A                                                           
         CLC   GXSDSUB,GXAKSUB                                                  
         BNE   DSLP1A                                                           
         EDIT  GXSDLEV,(3,LISTLEV),ZERO=NOBLANK,ALIGN=LEFT                      
         EDIT  GXSDVER,(3,LISTVER),ZERO=NOBLANK,ALIGN=LEFT                      
         MVC   LISTTYP,GXSDTYP                                                  
         MVC   LISTFLT1,GXSDFLT1                                                
         MVC   LISTFLT2,GXSDFLT2                                                
         MVC   LISTFLT3,GXSDFLT3                                                
         XC    APWORK,APWORK                                                    
         EDIT  GXSDPSZ,(8,LISTPSZ),ZERO=BLANK,ALIGN=LEFT                        
         EDIT  GXSDSSZ,(8,LISTSSZ),ZERO=BLANK,ALIGN=LEFT                        
         MVC   LISTFCOD,GXSDFCOD                                                
         B     DSLP1A                                                           
         DROP  R3                                                               
*                                                                               
         USING GXGNEL,R3                                                        
DSLGNM   EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,GXGNNUM                                                     
         EDIT  (RF),(5,LISTGNUM),ZERO=NOBLANK,ALIGN=LEFT                        
         B     DSLP1A                                                           
         DROP  R3                                                               
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
         MVI   GXKREC,GXAKRECQ                                                  
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
         CLI   GXKREC,GXAKRECQ     TEST STILL A MESSAGE RECORD                  
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
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY ALPHA ID                                 *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         MVI   FVMAXL,L'CT5KALPH                                                
         GOTO1 AFVAL                                                            
         BNE   VAGYNO                                                           
         MVC   IOKEYSV(L'IOKEY),IOKEY                                           
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY       BUILD KEY                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNL   *+14                TEST AIO RETURN CONDITION                    
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VAGYNO                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VAGYNO                                                           
*                                                                               
         B     VAGYOK                                                           
*                                                                               
VAGYNO   B     NO                                                               
VAGYOK   EQU   *                                                                
         MVC   IOKEY(L'IOKEY),IOKEYSV                                           
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE EXTRACT SUB SYSTEM                              *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: APHALF=SUB SYSTEM NUMBER, APWORK=SUB SYSTEM NAME              *         
*       CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALSUB   NTR1                                                                   
         MVI   FVMAXL,L'SUBLNAME                                                
         GOTO1 AFVAL                                                            
         BNE   VSUBNO                                                           
         USING SUBLSTD,RE                                                       
         LA    RE,SUBLST           CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VSUB010  CLI   SUBLNUM,0                                                        
         BE    VSUBNO                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SUBLNAME                                               
         BE    VSUB020                                                          
         LA    RE,SUBLLEN(RE)                                                   
         B     VSUB010                                                          
*                                                                               
VSUB020  MVC   APHALF(L'SUBLNUM),SUBLNUM   SET SYSTEM NUMBER FROM LIST          
         MVC   APWORK(L'SUBLNAME),SUBLNAME  GET FULL SYSTEM NAME                
         B     VSUBOK                                                           
*                                                                               
VSUBNO   B     NO                                                               
VSUBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY EXTRACT SUB SYSTEM                               *         
* NTRY: R1=A(SUB SYSTEM NUMBER)                                       *         
* EXIT: APWORK=SUB SYSTEM NAME                                        *         
*       CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSUB   NTR1                                                                   
         USING SUBLSTD,RE                                                       
         MVC   APWORK(7),=CL7'UNKNOWN'                                          
         LA    RE,SUBLST           CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
*                                                                               
DSUB010  CLI   SUBLNUM,0                                                        
         BE    DSUBNO                                                           
         CLC   0(1,R1),SUBLNUM                                                  
         BE    DSUB020                                                          
         LA    RE,SUBLLEN(RE)                                                   
         B     DSUB010                                                          
*                                                                               
DSUB020  EQU   *                           SET SYSTEM NUMBER FROM LIST          
         MVC   APWORK(L'SUBLNAME),SUBLNAME  GET FULL SYSTEM NAME                
         B     DSUBOK                                                           
*                                                                               
DSUBNO   B     NO                                                               
DSUBOK   B     YES                                                              
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
ECDX     MVC   FVMSGNO,=AL2(CE#CDXTR)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,ESSACTH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     NO                  CANNOT DELETE - EXISTING XTRANS RECS         
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
APALLOC  DC    XL3'001388'                                                      
ASALLOC  DC    XL3'001388'                                                      
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
* DXSYSLEX                                                                      
       ++INCLUDE DXSYSLEX                                                       
         EJECT                                                                  
* DXSUBLST                                                                      
       ++INCLUDE DXSUBLST                                                       
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         DROP  RB,RA                                                            
                                                                                
***********************************************************************         
* ROUTINE TO READ USER ID NAME RECORD INTO IOARE3 AND GET INFO        *         
*                                                                     *         
* ENTRY - UIDNAME= USER ID NAME                                       *         
* EXIT  - UIDNUM = USER ID NUMBER                                     *         
***********************************************************************         
GETUIN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEYSAVE,IOKEY                                                    
         L     R3,AIOAREA3                                                      
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,UIDNAME                                                   
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         DROP  R3                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GUINNO                                                           
         L     R3,AIOAREA3                                                      
         LA    R3,CTIDATA-CTIREC(R3)                                            
*                                  SAVE AGENCY COUNTRY CODE                     
GUIN010  CLI   0(R3),0                                                          
         BE    GUINOK                                                           
         CLI   0(R3),X'02'                                                      
         BE    GUIN020                                                          
GUIN012  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GUIN010                                                          
*                                                                               
GUIN020  CLI   1(R3),4                                                          
         BNE   GUINNO                                                           
         MVC   UIDNUM,2(R3)                                                     
         B     GUINOK                                                           
*                                                                               
GUINOK   SR    RC,RC               RETURN CC EQUAL                              
GUINNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  ,                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO READ ACCESS RECORD INTO IOAREA3 AND EXTRACT INFO         *         
*                                                                     *         
* NTRY - AGAID=AGENCY ALPHA ID                                        *         
*        SYSTEM=SYSTEM MAJOR CODE                                     *         
* EXIT - IDCTRY=COUNTRY CODE                                          *         
*        SENSAVE=SYSTEM EXECUTIVE NAME                                *         
***********************************************************************         
         SPACE 1                                                                
GETACC   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETACC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GAC'    INSERT NAME                                  
*                                                                               
         MVI   IDCTRY,0                                                         
         MVC   SENSAVE,=CL7'XXXXXXX'                                            
         MVC   KEYSAVE,IOKEY                                                    
         L     R3,AIOAREA3                                                      
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALPH                                                 
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         DROP  R3                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GACCNO                                                           
         L     R3,AIOAREA3                                                      
         LA    R3,CT5DATA-CT5REC(R3)                                            
*                                  SAVE AGENCY COUNTRY CODE                     
GACC010  CLI   0(R3),0                                                          
         BE    GACCOK                                                           
         CLI   0(R3),CTAGDELQ                                                   
         BE    GACC020                                                          
         CLI   0(R3),CTSYSELQ                                                   
         BE    GACC030                                                          
GACC012  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GACC010                                                          
*                                                                               
         USING CTAGDD,R3                                                        
GACC020  CLI   CTAGDLEN,CTAGDLNQ                                                
         BNH   GACC012                                                          
         MVC   IDCTRY,CTAGDCTY                                                  
         DROP  R3                                                               
         B     GACC012                                                          
*                                                                               
         USING CTSYSD,R3                                                        
GACC030  EQU   *                                                                
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   GACC012                                                          
         L     R4,ASYSFACS         SEARCH SE LIST FOR SE NUM                    
         L     R4,VSELIST-SYSFACD(R4)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R4                                                       
         CLC   CTSYSSE,SESYS                                                    
         BE    GACC032                                                          
         BXLE  R4,RE,*-10                                                       
         B     GACC012                                                          
*                                                                               
GACC032  EQU   *                                                                
         MVC   SENSAVE,SENAME                                                   
         B     GACC012                                                          
         DROP  R3                                                               
*                                                                               
GACCOK   SR    RC,RC               RETURN CC EQUAL                              
GACCNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  ,                                                                
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO READ USER ID NUMBER RECORD INTO IOARE3 AND GET INFO      *         
*                                                                     *         
* ENTRY - UIDNUM = USER ID NUMBER                                     *         
* EXIT  - UIDNAME= USER ID NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
GETUID   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETUID,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GAC'    INSERT NAME                                  
*                                                                               
         MVC   KEYSAVE,IOKEY                                                    
         L     R3,AIOAREA3                                                      
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID+8(2),UIDNUM                                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         DROP  R3                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GUIDNO                                                           
         L     R3,AIOAREA3                                                      
         LA    R3,CTIDATA-CTIREC(R3)                                            
*                                  SAVE AGENCY COUNTRY CODE                     
GUID010  CLI   0(R3),0                                                          
         BE    GUIDOK                                                           
         CLI   0(R3),X'02'                                                      
         BE    GUID020                                                          
GUID012  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GUID010                                                          
*                                                                               
GUID020  CLI   1(R3),12                                                         
         BNE   GUIDNO                                                           
         MVC   UIDNAME,2(R3)                                                    
         B     GUIDOK                                                           
*                                                                               
GUIDOK   SR    RC,RC               RETURN CC EQUAL                              
GUIDNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CTESSWRK                                                                      
       ++INCLUDE CTESSWRK                                                       
         SPACE 1                                                                
* DXSUBLSTD                                                                     
       ++INCLUDE DXSUBLSTD                                                      
         SPACE 1                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSFCD                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSDCD                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSBCD                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT DISPLAYED SYSTEM                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTAGY  DS    CL2                                                              
         DS    CL1                                                              
LISTSYS  DS    CL7                                                              
         DS    CL1                                                              
LISTSUB  DS    CL7                                                              
         DS    CL1                                                              
LISTVER  DS    CL3                                                              
         DS    CL1                                                              
LISTLEV  DS    CL3                                                              
         DS    CL1                                                              
LISTTYP  DS    CL3                                                              
         DS    CL2                                                              
LISTFLT1 DS    CL1                                                              
LISTFLT2 DS    CL1                                                              
LISTFLT3 DS    CL1                                                              
         DS    CL2                                                              
LISTPSZ  DS    CL8                                                              
         DS    CL1                                                              
LISTSSZ  DS    CL8                                                              
         DS    CL1                                                              
LISTFCOD DS    CL1                                                              
         DS    CL4                                                              
LISTGNUM DS    CL5                                                              
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
FULL     DS    F                                                                
WORK     DS    XL64                                                             
IOKEYSV  DS    XL(L'IOKEY)                                                      
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
LASTSYS  DS    XL1                 CONTROL TOF ON CHANGE OF SYSTEM              
SYSTEM   DS    CL1                 SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
SENSAVE  DS    CL7                                                              
SYSNUMS  DS    XL1                                                              
AGYALPH  DS    CL2                 AGENCY ALPHA ID                              
SUBSYS   DS    CL1                 SUB SYSTEM CODE                              
IDCTRY   DS    XL1                                                              
GETSEQF  DS    XL1                                                              
UIDNUM   DS    XL2                                                              
UIDNAME  DS    CL10                                                             
*                                                                               
SELKEY   DS    0XL32                                                            
SELAGY   DS    CL2                                                              
SELTHDC  DS    CL1                 THREAD SELECTED, Y/N                         
SELTHD   DS    XL1                                                              
SELSYS   DS    CL1                                                              
SELSUB   DS    CL1                                                              
SELSEN   DS    CL7                                                              
         ORG   SELKEY+L'SELKEY                                                  
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067CTESS03   09/21/16'                                      
         END                                                                    
