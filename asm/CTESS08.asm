*          DATA SET CTESS08    AT LEVEL 063 AS OF 05/16/16                      
*PHASE TA0E08A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*                                                                               
         TITLE 'CTESS08 - FILE MAINTENANCE - EXTRACT REFORM RECORDS'            
ESS08    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ES08**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GREFD,R2            R2=A(RECORD KEY)                             
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
* ROUTINE TO VALIDATE KEY OF REFORM RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GREFD,R2            R2=A(REFORM RECORD KEY)                      
         XC    GREFKEY,GREFKEY                                                  
         MVI   GREFKREC,GREFRECQ   REFORM ID RECORD TYPE                        
*                                  VALIDATE AGENCY ALPHA ID                     
         MVI   FVMINL,2                                                         
         GOTO1 VALAGY,REFAGYH                                                   
         BE    VK010                                                            
         CLC   FVMSGNO,=AL2(FVFNONE)                                            
         BNE   VALKEYX                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VK020                                                            
*                                                                               
VK010    MVC   GREFAGY,FVIFLD                                                   
*                                  VALIDATE REFORM ID                           
VK020    MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'GREFID                                                  
         GOTO1 AFVAL,REFTIDH                                                    
         BNE   VALKEYX                                                          
         MVC   GREFID,FVIFLD                                                    
*                                  READ RECORD                                  
VKREAD   MVC   APRECKEY(GREFKEYL),GREFKEY                                       
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
         OI    REFTIDH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A REFORM RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GREFKEY,APRECKEY                                                 
         CLI   APACTN,ACTADD                                                    
         BNE   VRCHG                                                            
*                                                                               
VRADD    MVC   GREFKEY,APRECKEY                                                 
         MVC   GRFLEN,=AL2(GRFIRST)                                             
         XC    GRFSTAT(GRFIRST-GREFKEYL),GRFSTAT                                
VRADDX   B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         USING GRTDEL,R3                                                        
         LA    R3,APELEM                                                        
         MVI   GRTDEL,GRTDELQ                                                   
         MVI   GRTDELL,0                                                        
         GOTO1 ADELELS,GREFD       DELETE EXISTING REF ELEMENT                  
         DROP  R3                                                               
*                                                                               
         USING GRTFEL,R3                                                        
VRCHG10  LA    R3,APELEM                                                        
         MVI   GRTFEL,GRTFELQ                                                   
         MVI   GRTFELL,0                                                        
         GOTO1 ADELELS,GREFD                                                    
         DROP  R3                                                               
VRCHGX   B     VRDATA                                                           
         EJECT                                                                  
*                                                                               
VRDATA   EQU   *                                                                
*                                  PROCESS REF REPORT DEFN. ELEMENT             
         USING GRTDEL,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GRTDEL,GRTDELQ                                                   
         MVI   GRTDELL,GRTDELLQ                                                 
         MVI   GRTDVER,1                                                        
         MVI   GRTDLEV,0                                                        
*                                                                               
VRDVER   GOTO1 AFVAL,REFVERH                                                    
         BNE   VRDVERX                                                          
         GOTO1 =V(NUMVAL),APPARM,REFVER,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GRTDVER                                                       
VRDVERX  EQU   *                                                                
*                                                                               
VRDLEV   GOTO1 AFVAL,REFLEVH                                                    
         BNE   VRDLEVX                                                          
         GOTO1 =V(NUMVAL),APPARM,REFLEV,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GRTDLEV                                                       
VRDLEVX  EQU   *                                                                
*                                                                               
VDSYS    MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,REFSYSH                                                    
         BNE   VRD010                                                           
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VDSY010  CLI   SYSLNUM,0                                                        
         BE    VDSY012                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VDSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VDSY010                                                          
*                                                                               
VDSY012  LA    RE,SYSLEX           CHECK IN EXTENDED SYSTEM TABLE               
         LA    RE,6(RE)                                                         
         ZIC   RF,FVXLEN                                                        
VDSY014  CLI   SYSLNUM,0                                                        
         BE    ESYS                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VDSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VDSY014                                                          
*                                                                               
VDSY020  MVC   GRTDSYS,SYSLNUM     SET SYSTEM NUMBER FROM LIST                  
         MVC   REFSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    REFSYSH+6,X'80'                                                  
*                                                                               
VDSUB    EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 VALSUB,REFSUBH                                                   
         BNE   EIIF                                                             
         MVC   GRTDSUB,APHALF                                                   
         MVC   REFSUB,APWORK                                                    
         OI    REFSUBH+6,X'80'                                                  
*                                                                               
VRD010   GOTO1 AFVAL,REFHSRH                                                    
         BNE   VRD020                                                           
         GOTO1 =V(NUMVAL),APPARM,REFHSR,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'30000'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,15,GRTDHSR                                                    
*                                                                               
VRD020   GOTO1 AFVAL,REFHERH                                                    
         BNE   VRD030                                                           
         GOTO1 =V(NUMVAL),APPARM,REFHER,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'30000'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,15,GRTDHER                                                    
*                                                                               
VRD030   GOTO1 AFVAL,REFDSRH                                                    
         BNE   VRD040                                                           
         GOTO1 =V(NUMVAL),APPARM,REFDSR,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'30000'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,15,GRTDDSR                                                    
*                                                                               
VRD040   GOTO1 AFVAL,REFDERH                                                    
         BNE   VRD050                                                           
         GOTO1 =V(NUMVAL),APPARM,REFDER,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'30000'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,15,GRTDDER                                                    
*                                                                               
VRD050   GOTO1 AFVAL,REFDSCH                                                    
         BNE   VRD060                                                           
         GOTO1 =V(NUMVAL),APPARM,REFDSC,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'30000'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,15,GRTDDSC                                                    
*                                                                               
VRD060   GOTO1 AFVAL,REFDECH                                                    
         BNE   VRD062                                                           
         GOTO1 =V(NUMVAL),APPARM,REFDEC,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'30000'                                                     
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STCM  R1,15,GRTDDEC                                                    
*                                                                               
VRD062   GOTO1 AFVAL,REFDCNH                                                    
         BNE   VRD070                                                           
         GOTO1 =V(NUMVAL),APPARM,REFDCN,(X'01',0),RR=APRELO                     
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'255'                                                       
         BH    EIIF                                                             
         C     R1,=F'0'                                                         
         BL    EIIF                                                             
         STC   R1,GRTDDCN                                                       
*                                                                               
VRD070   GOTO1 AFVAL,REFTABH                                                    
         BNE   VRD072                                                           
         MVC   GRTDTAB,FVIFLD                                                   
*                                                                               
VRD072   GOTO1 AFVAL,REFTABNH                                                   
         BNE   VRD080                                                           
         MVC   GRTDTABN,FVIFLD                                                  
*                                                                               
VRD080   GOTO1 AFVAL,REFACTH                                                    
         BNE   VRD090                                                           
         MVC   GRTDACT,FVIFLD                                                   
*                                                                               
VRD090   GOTO1 AFVAL,REFDTMH                                                    
         BNE   VRD110                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VRD092                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VRD092                                                           
         B     EIIF                                                             
VRD092   MVC   GRTDDTM,FVIFLD                                                   
*                                                                               
VRD110   GOTO1 AFVAL,REFPLFMH                                                   
         BNE   VRD120                                                           
         MVC   GRTDPLFM,FVIFLD                                                  
*                                                                               
VRD120   GOTO1 AFVAL,REFDELMH                                                   
         BNE   VRD130                                                           
         MVC   GRTDDELM,FVIFLD                                                  
*                                                                               
VRD130   GOTO1 AFVAL,REFEORH                                                    
         BNE   VRD140                                                           
         MVC   GRTDEOR,FVIFLD                                                   
*                                                                               
VRD140   GOTO1 AFVAL,REFTEXTH                                                   
         BNE   VRD150                                                           
         MVC   GRTDTEXT,FVIFLD                                                  
*                                                                               
VRD150   GOTO1 AFVAL,REFNULLH                                                   
         BNE   VRD160                                                           
         MVC   GRTDNULL,FVIFLD                                                  
*                                                                               
VRD160   GOTO1 AFVAL,REFMONYH                                                   
         BNE   VRD162                                                           
         MVC   GRTDMONY,FVIFLD                                                  
*                                                                               
VRD162   GOTO1 AFVAL,REFDATFH                                                   
         BNE   VRD166                                                           
         MVC   GRTDDATF,FVIFLD                                                  
*                                                                               
VRD166   GOTO1 AFVAL,REFSPCH                                                    
         BNE   VRD169                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VRD168                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VRD168                                                           
         B     EIIF                                                             
VRD168   MVC   GRTDSPC,FVIFLD                                                   
*                                                                               
VRD169   EQU   *                                                                
         XC    GRTDBDE,GRTDBDE                                                  
         XC    GRTDBDEI,GRTDBDEI                                                
         GOTO1 AFVAL,REFBDEH                                                    
         BNE   VRD170                                                           
         MVC   GRTDBDEI,FVIFLD                                                  
         MVC   GRTDBDE,FVIFLD                                                   
*                                                                               
VRD170   GOTO1 AFVAL,REFSQLBH                                                   
         BNE   VRD172                                                           
         MVC   GRTDSQLB,FVIFLD                                                  
*                                                                               
VRD172   GOTO1 AFVAL,REFSQLAH                                                   
         BNE   VRD174                                                           
         MVC   GRTDSQLA,FVIFLD                                                  
*                                                                               
VRD174   GOTO1 AFVAL,REFSQLIH                                                   
         BNE   VRD190                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VRD176                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VRD176                                                           
         CLI   FVIFLD,C'E'                                                      
         BE    VRD176                                                           
         B     EIIF                                                             
VRD176   MVC   GRTDSQLI,FVIFLD                                                  
*                                                                               
VRD190   GOTO1 AFVAL,REFKEYH                                                    
         BNE   VRD200                                                           
         MVC   GRTDKEY,FVIFLD                                                   
*                                                                               
VRD200   EQU   *                                                                
         GOTO1 AADDELS,GREFD       ADD REFORM DEFINITION ELEMENT                
         DROP  R3                                                               
*                                  PROCESS TRANSFORM FORMULA LINES              
         LA    R0,6                NUMBER OF SCREEN LINES                       
         LA    R1,REFFORMH                                                      
         BAS   RE,VALTFORM                                                      
         EJECT                                                                  
*                                  UPDATE RECORD                                
VRUPD    GOTO1 ASETACT,GREFD       DEFINE ACTIVITY ELEMENT                      
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
* ROUTINE TO VALIDATE TRANSFORM FORMULA LINES AND BUILD ELEMENTS      *         
* R0 - # LINES  R1 - FIELD HEADER  R2 - A(RECORD)                     *         
***********************************************************************         
         SPACE 1                                                                
         USING GRTFEL,R3                                                        
VALTFORM NTR1                                                                   
         LA    R4,1                                                             
         LA    R3,APELEM                                                        
VFOR010  XC    GRTFEL(GRTFELLQ),GRTFEL                                          
         MVI   GRTFEL,GRTFELQ                                                   
         MVI   GRTFELL,GRTFELLQ                                                 
         STC   R4,GRTFLIN                                                       
         LR    R8,R1                                                            
         GOTO1 AFVAL                                                            
         BNE   VFORX                                                            
         ZIC   R1,GRTFELL                                                       
         LA    RE,GRTFEL(R1)                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FVIFLD                                                   
         LA    RF,1(RF,R1)                                                      
         STC   RF,GRTFELL                                                       
         GOTO1 AADDELS,(R2)                                                     
         ZIC   RF,0(R8)                                                         
         LR    R1,R8                                                            
         AR    R1,RF                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,VFOR010                                                       
VFORX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF REFORM RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         XC    REFAGY,REFAGY                                                    
         OC    GREFAGY,GREFAGY                                                  
         BZ    *+10                                                             
         MVC   REFAGY,GREFAGY                                                   
         MVC   REFTID,GREFID                                                    
         OI    REFTIDH+(FVOIND-FVIHDR),FVOXMT                                   
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY REFORM RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC REFVERH                                                          
*                                                                               
         LA    R3,APELEM                                                        
         USING GRTDEL,R3                                                        
         MVI   GRTDEL,GRTDELQ                                                   
         MVI   GRTDELL,0                                                        
         GOTO1 AGETELS,GREFD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC100                                                          
         OC    GRTDSYS,GRTDSYS                                                  
         BZ    DREC010                                                          
         GOTO1 ADISSYS,GRTDSYS                                                  
         MVC   REFSYS,APWORK                                                    
         GOTO1 DISSUB,GRTDSUB                                                   
         MVC   REFSUB,APWORK                                                    
*                                                                               
DREC010  EQU   *                                                                
         EDIT  GRTDLEV,(3,REFLEV),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GRTDVER,(3,REFVER),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GRTDHSR,(6,REFHSR),ALIGN=LEFT                                    
         EDIT  GRTDHER,(6,REFHER),ALIGN=LEFT                                    
         EDIT  GRTDDSR,(6,REFDSR),ALIGN=LEFT                                    
         EDIT  GRTDDER,(6,REFDER),ALIGN=LEFT                                    
         EDIT  GRTDDSC,(6,REFDSC),ALIGN=LEFT                                    
         EDIT  GRTDDEC,(6,REFDEC),ALIGN=LEFT                                    
         EDIT  GRTDDCN,(1,REFDCN),ALIGN=LEFT                                    
         MVC   REFTAB,GRTDTAB                                                   
         MVC   REFTABN,GRTDTABN                                                 
         MVC   REFACT,GRTDACT                                                   
         MVC   REFDTM,GRTDDTM                                                   
         MVC   REFPLFM,GRTDPLFM                                                 
         MVC   REFDELM,GRTDDELM                                                 
         MVC   REFEOR,GRTDEOR                                                   
         MVC   REFTEXT,GRTDTEXT                                                 
         MVC   REFNULL,GRTDNULL                                                 
         MVC   REFMONY,GRTDMONY                                                 
         MVC   REFDATF,GRTDDATF                                                 
         MVC   REFSQLB,GRTDSQLB                                                 
         MVC   REFSQLA,GRTDSQLA                                                 
         MVC   REFSQLI,GRTDSQLI                                                 
         MVC   REFKEY,GRTDKEY                                                   
         MVC   REFSPC,GRTDSPC                                                   
         MVC   REFBDE,GRTDBDE                                                   
         OC    GRTDBDEI,GRTDBDEI                                                
         BZ    DREC020                                                          
         CLC   GRTDBDEI,SPACES                                                  
         BE    DREC020                                                          
         MVC   REFBDE,GRTDBDEI                                                  
DREC020  EQU   *                                                                
         DROP  R3                                                               
*                                  DISPLAY TRANSFORM FORMULA LINES              
DREC100  EQU   *                                                                
         LA    R0,6                  NUMBER OF COMMENT LINES                    
         LA    R1,REFFORMH           ADDRESS OF FIRST LINE                      
         BAS   RE,DISTFORM           DO DISPLAY                                 
*                                                                               
         GOTO1 ADISACT,GREFD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY TRANSFORM FORMULA ELEMENTS                       *         
* R0 - # LINES  R1 - FIELD HEADER  R2 - A(RECORD)                     *         
***********************************************************************         
         SPACE 1                                                                
         USING GRTFEL,R3                                                        
DISTFORM NTR1                                                                   
         LA    R4,1                                                             
         LA    R3,APELEM                                                        
DFOR010  XC    APELEM,APELEM                                                    
         MVI   APELEM,GRTFELQ     FIND NEXT COMMENT ELEMENT                     
         MVI   APELEM+1,2                                                       
         STH   R4,APELEM+2                                                      
         LR    R8,R1                                                            
         GOTO1 AGETELS,(R2)                                                     
         L     R3,APPARM           APPARM =A(ELEMENT) IF FOUND                  
         LTR   R3,R3               ZEROS IF NOT                                 
         BZ    DFOR100                                                          
         SR    RE,RE                                                            
         IC    RE,GRTFELL                                                       
         LA    RF,GRTFELLQ+1                                                    
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R8),GRTFDAT                                                  
*                                                                               
DFOR100  ZIC   RF,0(R8)                                                         
         LR    R1,R8                                                            
         AR    R1,RF                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,DFOR010                                                       
DFORX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN REFORM RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OI    GRDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GREFD                                                    
         OI    GRFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED REFORM RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GRDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GREFD                                                    
         NI    GRFSTAT,X'FF'-X'80' UNSET DELETE                                 
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
         XC    GREFKEY,GREFKEY                                                  
         MVI   GREFKMAJ,X'FF'      FLAG FOR FIRST PASS                          
         MVI   GREFKREC,GREFRECQ                                                
         XC    SELKEY,SELKEY                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VSELAGY  EQU   *                                                                
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VSAGYX                                                           
         MVC   SELAGY,FVIFLD                                                    
         MVC   GREFAGY,SELAGY                                                   
VSAGYX   EQU   *                                                                
*                                                                               
VSELBDE  EQU   *                                                                
         GOTO1 AFVAL,LSTBDEH                                                    
         BNE   VSBDEX                                                           
         MVC   SELBDE,FVIFLD                                                    
VSBDEX   EQU   *                                                                
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
VSSY012  LA    RE,SYSLEX           CHECK IN TABLE EXTENDED SYSTEMS              
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
         MVC   LSTSUB,APWORK                                                    
         OI    LSTSUBH+6,X'80'                                                  
VSSUBX   EQU   *                                                                
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
         MVC   GREFKEY,APRECKEY                                                 
         CLI   GREFKMAJ,X'FF'      TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GREFKMAJ,0                                                       
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
         CLI   GREFKREC,GREFRECQ                                                
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
         CLC   SELAGY,GREFAGY                                                   
         BNE   GETSEL8                                                          
GSAGYX   EQU   *                                                                
*                                                                               
GSELEM   EQU   *                   GET ELEMENT DATA                             
         LA    R3,GRFIRST(R2)                                                   
         SR    RF,RF                                                            
         USING GRTDEL,R3                                                        
GSEM010  CLI   GRTDEL,0                                                         
         BE    GSEM030                                                          
         CLI   GRTDEL,GRTDELQ                                                   
         BE    GSEM030                                                          
GSEM020  IC    RF,GRTDELL                                                       
         AR    R3,RF                                                            
         B     GSEM010                                                          
*                                                                               
GSEM030  EQU   *                                                                
GSBDE    EQU   *                                                                
         OC    SELBDE,SELBDE                                                    
         BZ    GSBDEX                                                           
         CLC   SELBDE(1),GRTDBDE                                                
         BNE   GETSEL8                                                          
         CLC   SELBDE,GRTDBDEI                                                  
         BNE   GETSEL8                                                          
GSBDEX   EQU   *                                                                
*                                                                               
GSSYS    EQU   *                                                                
         OC    SELSYS,SELSYS                                                    
         BZ    GSSYSX                                                           
         CLC   SELSYS,GRTDSYS                                                   
         BNE   GETSEL8                                                          
GSSYSX   EQU   *                                                                
*                                                                               
GSSUB    EQU   *                                                                
         OC    SELSUB,SELSUB                                                    
         BZ    GSSUBX                                                           
         CLC   SELSUB,GRTDSUB                                                   
         BNE   GETSEL8                                                          
GSSUBX   EQU   *                                                                
         DROP  R3                                                               
GSEMX    EQU   *                                                                
*                                                                               
GETSELY  MVC   APRECKEY(L'GREFKEY),GREFKEY                                      
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
         OC    GREFAGY,GREFAGY                                                  
         BZ    *+10                                                             
         MVC   LISTAGY,GREFAGY                                                  
         MVC   LISTREF,GREFID                                                   
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GRFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GRTDELQ                                                    
         BE    DSREFEL                                                          
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GRTDEL,R3                                                        
DSREFEL  EQU   *                                                                
         MVC   LISTBDE,GRTDBDE                                                  
         GOTO1 ADISSYS,GRTDSYS                                                  
         MVC   LISTSYS,APWORK                                                   
         GOTO1 DISSUB,GRTDSUB                                                   
         MVC   LISTSUB,APWORK                                                   
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
         MVI   GREFKREC,GREFRECQ                                                
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
         CLI   GREFKREC,GREFRECQ                                                
         BNE   PRTREPX                                                          
*                                                                               
PR040    GOTO1 VREPORT,REPD                                                     
         B     PR020                                                            
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
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
APALLOC  DC    XL3'00012C'                                                      
ASALLOC  DC    XL3'000064'                                                      
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
* CTESSWRK                                                                      
       ++INCLUDE CTESSWRK                                                       
         SPACE 1                                                                
* DXSUBLSTD                                                                     
       ++INCLUDE DXSUBLSTD                                                      
         SPACE 1                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSF7D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSD7D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSB7D                                                       
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
LISTREF  DS    CL5                                                              
         DS    CL1                                                              
LISTSYS  DS    CL7                                                              
         DS    CL1                                                              
LISTSUB  DS    CL7                                                              
         DS    CL1                                                              
LISTBDE  DS    CL8                                                              
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
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
*                                                                               
SELKEY   DS    0XL32                                                            
SELAGY   DS    CL2                                                              
SELSYS   DS    CL1                                                              
SELSUB   DS    CL1                                                              
SELBDE   DS    CL8                                                              
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063CTESS08   05/16/16'                                      
         END                                                                    
