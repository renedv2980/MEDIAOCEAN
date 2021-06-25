*          DATA SET CTESS04    AT LEVEL 100 AS OF 05/16/16                      
*PHASE TA0E04A                                                                  
*INCLUDE TIMBER                                                                 
*INCLUDE NUMVAL                                                                 
*                                                                               
         TITLE 'CTESS04 - FILE MAINTENANCE - EXTRACT FILE RECORDS'              
ESS04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ES04**,R9,RA,RR=RE                                           
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
         B     SETTWA                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF EXTRACT FILE DEFINITION RECORD           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GXTRD,R2            R2=A(XFILE RECORD KEY)                       
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAFRECQ     RECORD TYPE                                  
*                                                                               
VKDATE   EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,XFIDACH                                                    
         BNE   VKTIME                                                           
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BNE   EIIF                                                             
         MVC   GXAFDAT,APWORK+PVALCSTA-PERVALD                                  
         MVC   FCDATE,GXAFDAT                                                   
*&&UK*&& CLC   FCDATE,=XL2'CC3E'   COMPLEMENT AFTER 30JAN2002                   
*&&US*&& CLC   FCDATE,=XL2'CB77'   COMPLEMENT AFTER 23NOV2001                   
         BL    *+10                                                             
         XC    GXAFDAT,=XL2'FFFF'                                               
*                                                                               
VKTIME   EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 VALTIM,XFITICH                                                   
         BNE   VKAGY                                                            
         MVC   GXAFTIM,APFULL                                                   
*&&UK*&& CLC   FCDATE,=XL2'CC3E'   COMPLEMENT AFTER 30JAN2002                   
*&&US*&& CLC   FCDATE,=XL2'CB77'   COMPLEMENT AFTER 23NOV2001                   
         BL    *+10                                                             
         XC    GXAFTIM,=XL4'FFFFFFFF'                                           
*                                                                               
VKAGY    EQU   *                                                                
         MVI   FVMINL,2                                                         
         GOTO1 VALAGY,XFIAGYH      VALIDATE AGENCY ALPHA ID                     
         BNE   VALKEYX                                                          
         MVC   GXAFAGY,FVIFLD                                                   
*                                                                               
VKSYS    MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,XFISYSH                                                    
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
VKSY012  LA    RE,SYSLEX           CHECK IN EXTENEDE SYSTEM TABLE               
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
VKSY020  MVC   GXAFSYS,SYSLNUM     SET SYSTEM NUMBER FROM LIST                  
         MVC   XFISYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    XFISYSH+6,X'80'                                                  
*                                                                               
VKSUB    EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 VALSUB,XFISUBH                                                   
         BNE   EIIF                                                             
         MVC   GXAFSUB,APHALF                                                   
         MVC   XFISUB,APWORK                                                    
         MVC   AGYALPH,GXAFAGY                                                  
         MVC   SYSTEM,GXAFSYS                                                   
         MVC   SUBSYS,GXAFSUB                                                   
*                                  READ RECORD                                  
VKREAD   MVC   APRECKEY(GXKEYL),GXKEY                                           
         LA    R1,IOHID+IOGENDIR                                                
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         CLC   GXKEY(GXAFSUB+L'GXAFSUB-GXKEY),APRECKEY                          
         BE    VKR010                                                           
         MVI   APINDS,APIOKADD     NO RECORD WITH THIS KEY                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKEYX                                                          
*                                                                               
VKR010   MVC   FCDATE,GXAFDAT                                                   
         MVC   FCTIME,GXAFTIM                                                   
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXFKRECQ     RECORD TYPE                                  
         MVC   GXFKAGY,AGYALPH                                                  
         MVC   GXFKSYS,SYSTEM                                                   
         MVC   GXFKSUB,SUBSYS                                                   
         MVC   GXFKDAT,FCDATE                                                   
         MVC   GXFKTIM,FCTIME                                                   
*                                                                               
         MVC   APRECKEY(GXKEYL),GXKEY                                           
         LA    R1,IORDD+IOGENDIR                                                
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VKR020                                                           
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VKR020   LA    R1,IOGET+IOGENFIL+IO1                                            
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
         OI    XFIAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XFISYSH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XFISUBH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XFIDACH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    XFITICH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    APWORK,APWORK                                                    
         MVC   FCDATE,GXFKDAT                                                   
         MVC   FCTIME,GXFKTIM                                                   
         CLC   GXFKDAT,=XL2'7FFF'                                               
         BH    VALKEYYA                                                         
         XC    FCDATE,=XL2'FFFF'                                                
         XC    FCTIME,=XL4'FFFFFFFF'                                            
VALKEYYA EQU  *                                                                 
         GOTO1 VDATCON,APPARM,(2,FCDATE),(X'51',APWORK)                         
         MVC   XFIDAC,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,FCTIME,APWORK                                     
         MVC   XFITIC,APWORK                                                    
         MVC   XFIAGY,GXFKAGY                                                   
         GOTO1 ADISSYS,GXFKSYS                                                  
         MVC   XFISYS,APWORK                                                    
         GOTO1 DISSUB,GXFKSUB                                                   
         MVC   XFISUB,APWORK                                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A XFILE RECORD                             *         
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
         B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         B     VRDATA                                                           
*                                                                               
VRDATA   EQU   *                                                                
*                                  UPDATE RECORD                                
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
* ROUTINE TO DISPLAY KEY OF XFILE RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         CLI   LSTMODE,0                                                        
         BE    DKEY010                                                          
*                                                                               
         CLI   GXKREC,GXSFRECQ                                                  
         BNE   DKEY010                                                          
         MVC   SAVEEID,GXSFEID                                                  
         MVC   IODA,APRECDA                                                     
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
*                                                                               
DKEY010  XC    APWORK,APWORK                                                    
         MVC   FCDATE,GXFKDAT                                                   
         MVC   FCTIME,GXFKTIM                                                   
         CLC   GXFKDAT,=XL2'7FFF'                                               
         BH    DKEY012                                                          
         XC    FCDATE,=XL2'FFFF'                                                
         XC    FCTIME,=XL4'FFFFFFFF'                                            
DKEY012  GOTO1 VDATCON,APPARM,(2,FCDATE),(X'51',APWORK)                         
         MVC   XFIDAC,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,FCTIME,APWORK                                     
         MVC   XFITIC,APWORK                                                    
         MVC   XFIAGY,GXFKAGY                                                   
         GOTO1 ADISSYS,GXFKSYS                                                  
         MVC   XFISYS,APWORK                                                    
         GOTO1 DISSUB,GXFKSUB                                                   
         MVC   XFISUB,APWORK                                                    
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY XFILE RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         XC    XFIGNUM,XFIGNUM                                                  
         OI    XFIGNUMH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R3,APELEM                                                        
         USING GXGNEL,R3                                                        
         MVI   GXGNEL,GXGNELQ                                                   
         MVI   GXGNELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC006                                                          
         SR    RF,RF                                                            
         ICM   RF,3,GXGNNUM                                                     
         EDIT  (RF),(5,XFIGNUM),ZERO=NOBLANK,ALIGN=LEFT                         
*                                                                               
DREC006  CLI   LSTMODE,0                                                        
         BE    DREC008                                                          
         CLI   SCSELCOD,C'F'                                                    
         BNE   DREC007                                                          
* ??     MVI   LSTMODE,0                                                        
         B     DREC008                                                          
DREC007  CLI   SUBSCRN,SUBSXF2S                                                 
         BE    DRECSXFD                                                         
         GOTO1 SETSUBS,SUBSXF2S                                                 
         B     DRECSXFD                                                         
*                                                                               
DREC008  CLI   SUBSCRN,SUBSXF2S                                                 
         BE    DREC030                                                          
*                                                                               
         CLI   SUBSCRN,SUBSNULL                                                 
         BE    DREC030                                                          
*                                                                               
         USING SLUSD,R4                                                         
         LA    R4,XF1ACT1H         SET ADDRESS OF FIRST LIST LINE               
         LA    R1,XF1ACT2H         SET LIST LINE LENGTH                         
         SR    R1,R4                                                            
         STH   R1,SLUSLEN                                                       
         LA    R1,XF1TENDH         SET ADDRESS END OF LIST                      
         ST    R1,SLUSLAST                                                      
DREC010  EQU   *                                                                
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,SLUSACTH                                                   
         BNE   DREC020                                                          
         CLI   FVILEN,1                                                         
         BNE   EIIF                                                             
         CLI   FVIFLD,C'S'                                                      
         BNE   EIIF                                                             
         OC    SLUSEID,SLUSEID                                                  
         BZ    EIIF                                                             
         MVC   SAVEEID,SLUSEID                                                  
         GOTO1 SETSUBS,SUBSXF2S                                                 
         B     DRECSXFD                                                         
*                                                                               
DREC020  AH    R4,SLUSLEN                                                       
         C     R4,SLUSLAST                                                      
         BL    DREC010                                                          
         B     DREC100                                                          
         DROP  R4                                                               
*                                                                               
DREC030  CLI   LSTMODE,0                                                        
         BNE   *+10                                                             
         XC    SAVEEID,SAVEEID                                                  
         GOTO1 SETSUBS,SUBSXF1S                                                 
*                                                                               
DREC100  TWAXC XF1TYPH                                                          
         XC    XF1FFL,XF1FFL                                                    
         OI    XF1FFLH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    XF1TFL,XF1TFL                                                    
         OI    XF1TFLH+(FVOIND-FVIHDR),FVOXMT                                   
*                                  CLEAR LUID TRANSFER LIST LINES               
         USING SLUSD,RF                                                         
         LA    RF,XF1ACT1H         SET ADDRESS OF FIRST LIST LINE               
         LA    R1,XF1TENDH         SET ADDRESS END OF LIST                      
         LA    RE,XF1ACT2H         SET LIST LINE LENGTH                         
         SR    RE,RF                                                            
DREC110  CR    RF,R1                                                            
         BNL   DREC120                                                          
         XC    SLUSLIN,SLUSLIN                                                  
         OI    SLUSLINH+(FVOIND-FVIHDR),FVOXMT                                  
         AR    RF,RE                                                            
         B     DREC110                                                          
         DROP  RF                                                               
*                                                                               
DREC120  LA    R3,APELEM                                                        
         USING GXFDEL,R3                                                        
         MVI   GXFDEL,GXFDELQ                                                   
         MVI   GXFDELL,0                                                        
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DRECSLU                                                          
         MVC   XF1TYP,GXFDTYP                                                   
         EDIT  GXFDRNM,(8,XF1RNM),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GXFDBNM,(8,XF1BNM),ZERO=NOBLANK,ALIGN=LEFT                       
         EDIT  GXFDMTO,(8,XF1MTO),ZERO=NOBLANK,ALIGN=LEFT                       
         MVC   XF1DSN,GXFDDSN                                                   
         MVC   XF1MOD,GXFDMOD                                                   
         GOTO1 TIMEOUT,APPARM,GXFDFTM,APWORK                                    
         MVC   XF1FTM,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFDTTM,APWORK                                    
         MVC   XF1TTM,APWORK                                                    
         GOTO1 VHEXOUT,APPARM,GXFDFDA,APWORK,4,=C'TOG'                          
         MVC   XF1FDA,APWORK                                                    
         GOTO1 VHEXOUT,APPARM,GXFDTDA,APWORK,4,=C'TOG'                          
         MVC   XF1TDA,APWORK                                                    
         TM    GXFDFFL,X'01'                                                    
         BNO   *+8                                                              
         MVI   XF1FFL,C'*'                                                      
         TM    GXFDTFL,X'01'                                                    
         BNO   *+8                                                              
         MVI   XF1TFL,C'*'                                                      
         DROP  R3                                                               
*                                                                               
DRECSLU  EQU   *                                                                
         USING SLUSD,R4                                                         
         LA    R4,XF1ACT1H         SET ADDRESS OF FIRST LIST LINE               
         LA    R1,XF1ACT2H         SET LIST LINE LENGTH                         
         SR    R1,R4                                                            
         STH   R1,SLUSLEN                                                       
         LA    R1,XF1TENDH         SET ADDRESS END OF LIST                      
         ST    R1,SLUSLAST                                                      
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
*                                                                               
DRSL010  CLI   0(R3),0                                                          
         BE    DREC200             END OF RECORD                                
         CLI   0(R3),GXFTELQ                                                    
         BE    DRSL030                                                          
*                                                                               
DRSL020  SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DRSL010                                                          
*                                                                               
         USING GXFTEL,R3                                                        
DRSL030  EQU   *                   PROCESS FILE TRANSMISION ELEMENT             
         C     R4,SLUSLAST                                                      
         BNL   DREC200                                                          
         MVC   SLUSEID,GXFTEID                                                  
         SR    RF,RF                                                            
         ICM   RF,3,GXFTFNUM                                                    
         EDIT  (RF),(8,SLUSFNUM),ZERO=NOBLANK,ALIGN=LEFT                        
         OC    GXFTDRE,GXFTDRE                                                  
         BZ    DRSL032                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFTDRE),(X'51',APWORK)                        
         MVC   SLUSDTR,APWORK                                                   
DRSL032  OC    GXFTTRE,GXFTTRE                                                  
         BZ    DRSL034                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFTTRE,APWORK                                    
         MVC   SLUSTTR,APWORK                                                   
DRSL034  OC    GXFTDCO,GXFTDCO                                                  
         BZ    DRSL036                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFTDCO),(X'51',APWORK)                        
         MVC   SLUSDCO,APWORK                                                   
DRSL036  OC    GXFTTCO,GXFTTCO                                                  
         BZ    DRSL038                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFTTCO,APWORK                                    
         MVC   SLUSTCO,APWORK                                                   
DRSL038  EQU   *                                                                
         TM    GXFTCTL,GXFTCOM                                                  
         BNZ   DRSL039                                                          
         OC    GXFTRCO,GXFTRCO                                                  
         BZ    DRSL040                                                          
         CLC   GXFTRCO,SPACES                                                   
         BE    DRSL040                                                          
DRSL039  EQU   *                                                                
         CLC   GXFTRCO,=CL6'000000'                                             
         BE    *+14                                                             
         MVC   SLUSSTAT,=CL10'COMMIT ERR'                                       
         B     DRSL050                                                          
         MVC   SLUSSTAT,=CL10'COMMITTED'                                        
         B     DRSL050                                                          
DRSL040  EQU   *                                                                
         TM    GXFTCTL,GXFTRCV                                                  
         BNZ   DRSL041                                                          
         OC    GXFTRRE,GXFTRRE                                                  
         BZ    DRSL042                                                          
         CLC   GXFTRRE,SPACES                                                   
         BE    DRSL042                                                          
DRSL041  EQU   *                                                                
         CLC   GXFTRRE,=CL6'000000'                                             
         BE    *+14                                                             
         MVC   SLUSSTAT,=CL10'RCV. ERROR'                                       
         B     DRSL050                                                          
         MVC   SLUSSTAT,=CL10'RECEIVED'                                         
         B     DRSL050                                                          
DRSL042  EQU   *                                                                
         TM    GXFTCTL,GXFTNOT                                                  
         BNZ   DRSL043                                                          
         OC    GXFTRNO,GXFTRNO                                                  
         BZ    DRSL044                                                          
         CLC   GXFTRNO,SPACES                                                   
         BE    DRSL044                                                          
DRSL043  EQU   *                                                                
         CLC   GXFTRNO,=CL6'000000'                                             
         BE    *+14                                                             
         MVC   SLUSSTAT,=CL10'NOT. ERROR'                                       
         B     DRSL050                                                          
         MVC   SLUSSTAT,=CL10'NOTIFIED'                                         
         B     DRSL050                                                          
DRSL044  MVC   SLUSSTAT,=CL10'CREATED'                                          
DRSL050  OI    SLUSLINH+(FVOIND-FVIHDR),FVOXMT                                  
         AH    R4,SLUSLEN                                                       
         B     DRSL020                                                          
         DROP  R4                                                               
*                                                                               
DRECSXFD TWAXC XF2EIDH                                                          
*                                                                               
         LA    R3,APELEM                                                        
         USING GXFTEL,R3                                                        
         MVI   GXFTEL,GXFTELQ                                                   
         MVI   APELEM+1,L'SAVEEID                                               
         MVC   APELEM+2,SAVEEID                                                 
         GOTO1 AGETELS,GXTRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DISRECX                                                          
         MVC   XF2EID,GXFTEID                                                   
         MVC   XF2LOAD,GXFTLOAD                                                 
         SR    RF,RF                                                            
         ICM   RF,3,GXFTFNUM                                                    
         EDIT  (RF),(8,XF2FNM),ZERO=NOBLANK,ALIGN=LEFT                          
         OC    GXFTDNO,GXFTDNO                                                  
         BZ    DXFD010                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFTDNO),(X'51',APWORK)                        
         MVC   XF2DNO,APWORK                                                    
         OI    XF2DNOH+(FVOIND-FVIHDR),FVOXMT                                   
DXFD010  OC    GXFTTNO,GXFTTNO                                                  
         BZ    DXFD020                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFTTNO,APWORK                                    
         MVC   XF2TNO,APWORK                                                    
         OI    XF2TNOH+(FVOIND-FVIHDR),FVOXMT                                   
DXFD020  OC    GXFTDRE,GXFTDRE                                                  
         BZ    DXFD030                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFTDRE),(X'51',APWORK)                        
         MVC   XF2DRE,APWORK                                                    
         OI    XF2DREH+(FVOIND-FVIHDR),FVOXMT                                   
DXFD030  OC    GXFTTRE,GXFTTRE                                                  
         BZ    DXFD040                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFTTRE,APWORK                                    
         MVC   XF2TRE,APWORK                                                    
         OI    XF2TREH+(FVOIND-FVIHDR),FVOXMT                                   
DXFD040  OC    GXFTDCO,GXFTDCO                                                  
         BZ    DXFD050                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFTDCO),(X'51',APWORK)                        
         MVC   XF2DCO,APWORK                                                    
         OI    XF2DCOH+(FVOIND-FVIHDR),FVOXMT                                   
DXFD050  OC    GXFTTCO,GXFTTCO                                                  
         BZ    DXFD060                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFTTCO,APWORK                                    
         MVC   XF2TCO,APWORK                                                    
         OI    XF2TCOH+(FVOIND-FVIHDR),FVOXMT                                   
DXFD060  EQU   *                                                                
         OC    GXFTRNO,GXFTRNO                                                  
         BZ    *+14                                                             
         MVC   XF2SNO(6),GXFTRNO                                                
         OI    XF2SNOH+(FVOIND-FVIHDR),FVOXMT                                   
         OC    GXFTRRE,GXFTRRE                                                  
         BZ    *+14                                                             
         MVC   XF2SRE(6),GXFTRRE                                                
         OI    XF2SREH+(FVOIND-FVIHDR),FVOXMT                                   
         OC    GXFTRRE,GXFTRRE                                                  
         BZ    *+14                                                             
         MVC   XF2SCO(6),GXFTRCO                                                
         OI    XF2SCOH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DXFD100  LA    R4,GXFTSLST                                                      
         USING GXFTSLST,R4                                                      
         LR    RF,R3                                                            
         SR    R1,R1                                                            
         IC    R1,GXFTELL                                                       
         AR    RF,R1                                                            
         ST    RF,RFSAVE                                                        
         LA    R8,XF2LIN1H                                                      
         USING APPLISTD,R8                                                      
         LA    R1,XF2LIN2H         SET LIST LINE LENGTH                         
         SR    R1,R8                                                            
         STH   R1,APPLLEN                                                       
         LA    R1,XF2TENDH         SET ADDRESS END OF LIST                      
         ST    R1,APPLLAST                                                      
*                                                                               
DXFD110  EQU   *                                                                
         L     RF,RFSAVE                                                        
         CR    RF,R4                                                            
         BNH   DREC200                                                          
         OI    APPLLINH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   APPLKEY,GXFTSKEY                                                 
         OC    GXFTDSCO,GXFTDSCO                                                
         BZ    DXFD120                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFTDSCO),(X'51',APWORK)                       
         MVC   APPLDCO,APWORK                                                   
DXFD120  OC    GXFTTSCO,GXFTTSCO                                                
         BZ    DXFD130                                                          
         XC    APWORK,APWORK                                                    
         GOTO1 TIMEOUT,APPARM,GXFTTSCO,APWORK                                   
         MVC   APPLTCO,APWORK                                                   
DXFD130  OC    GXFTRSCO,GXFTRSCO                                                
         BZ    DXFD140                                                          
         MVC   APPLRCO(6),GXFTRSCO                                              
DXFD140  LA    R4,GXFTSLQ(R4)                                                   
         AH    R8,APPLLEN                                                       
         C     R8,APPLLAST                                                      
         BL    DXFD110                                                          
*                                                                               
DREC200  EQU   *                                                                
*                                                                               
         GOTO1 ADISACT,GXTRD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN XFILE RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
DELREC   EQU   *                                                                
         LA    R2,IOKEY                                                         
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
         MVC   AGYALPH,GXFKAGY                                                  
         MVC   SYSTEM,GXFKSYS                                                   
         MVC   SUBSYS,GXFKSUB                                                   
         MVC   FCDATE,GXFKDAT                                                   
         MVC   FCTIME,GXFKTIM                                                   
         LA    R2,IOKEY                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAFRECQ                                                  
         MVC   GXAFAGY,AGYALPH                                                  
         MVC   GXAFSYS,SYSTEM                                                   
         MVC   GXAFSUB,SUBSYS                                                   
         MVC   GXAFDAT,FCDATE                                                   
         MVC   GXAFTIM,FCTIME                                                   
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
DELR002  GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
*                                                                               
DELR010  CLI   0(R3),0                                                          
         BE    DELR100             END OF RECORD                                
         CLI   0(R3),GXFTELQ                                                    
         BE    DELR030                                                          
         CLI   0(R3),GXFDELQ                                                    
         BE    DELR040                                                          
*                                                                               
DELR020  SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DELR010                                                          
*                                                                               
         USING GXFTEL,R3                                                        
DELR030  EQU   *                   PROCESS FILE TRANSMISION ELEMENT             
         LA    R2,IOKEY                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSFRECQ                                                  
         MVC   GXSFEID,GXFTEID                                                  
         MVC   GXSFAGY,AGYALPH                                                  
         MVC   GXSFSYS,SYSTEM                                                   
         MVC   GXSFSUB,SUBSYS                                                   
         MVC   GXSFFNUM,GXFTFNUM                                                
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DELR020                                                          
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DELR020                                                          
         B     DELR020                                                          
*                                                                               
         USING GXFDEL,R3                                                        
DELR040  EQU   *                   PROCESS FILE DEFINITION ELEMENT              
         MVC   FTPDSN,GXFDDSN                                                   
         MVC   TXTDSN+6(44),FTPDSN LOAD DYNALLOC BLOCK WITH DSN                 
         SR    RF,RF                                                            
         ICM   RF,7,ARBLKDLA+1                                                  
         A     RF,APRELO                                                        
         STCM  RF,7,ARBLKDLA+1                                                  
         SR    RF,RF                                                            
         ICM   RF,15,RBLKDELA+8                                                 
         A     RF,APRELO                                                        
         STCM  RF,15,RBLKDELA+8                                                 
         SR    RF,RF                                                            
         ICM   RF,7,ADELALLT+1                                                  
         A     RF,APRELO                                                        
         STCM  RF,7,ADELALLT+1                                                  
         SR    RF,RF                                                            
         ICM   RF,7,ADELALLT+5                                                  
         A     RF,APRELO                                                        
         STCM  RF,7,ADELALLT+5                                                  
         SR    RF,RF                                                            
         ICM   RF,7,ADELALLT+9                                                  
         A     RF,APRELO                                                        
         STCM  RF,7,ADELALLT+9                                                  
*                                  DYNAMICALLY PURGE MVS FILE                   
*        LA    R1,ARBLKDLA         NOT ON LINE YOU DONT                         
*        DYNALLOC                                                               
*        LTR   RF,RF                                                            
*        BZ    DELR050             DATASET WAS DELETED OK                       
*        B     DELR020               ELSE IGNORE ERROR                          
*                                  DYNAMICALLY DEALLOCATE MVS FILE              
DELR050  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,7,ARBLKUN2+1                                                  
         A     RF,APRELO                                                        
         STCM  RF,7,ARBLKUN2+1                                                  
         SR    RF,RF                                                            
         ICM   RF,15,RBLKUNA2+8                                                 
         A     RF,APRELO                                                        
         STCM  RF,15,RBLKUNA2+8                                                 
         SR    RF,RF                                                            
         ICM   RF,7,ACATUNT2+1                                                  
         A     RF,APRELO                                                        
         STCM  RF,7,ACATUNT2+1                                                  
*                                                                               
*        LA    R1,ARBLKUN2                                                      
*        DYNALLOC                                                               
*        LTR   RF,RF                                                            
*        BZ    DELR020             DATASET WAS DEALLOCATED OK                   
*        B     DELR020               ELSE IGNORE ERROR                          
         B     DELR020               ELSE IGNORE ERROR                          
*                                                                               
DELR100  EQU   *                                                                
*                                                                               
DELRECX  B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED XFILE RECORD                           *         
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
         MVC   AGYALPH,GXFKAGY                                                  
         MVC   SYSTEM,GXFKSYS                                                   
         MVC   SUBSYS,GXFKSUB                                                   
         MVC   FCDATE,GXFKDAT                                                   
         MVC   FCTIME,GXFKTIM                                                   
         LA    R2,IOKEY                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAFRECQ                                                  
         MVC   GXAFAGY,AGYALPH                                                  
         MVC   GXAFSYS,SYSTEM                                                   
         MVC   GXAFSUB,SUBSYS                                                   
         MVC   GXAFDAT,FCDATE                                                   
         MVC   GXAFTIM,FCTIME                                                   
         GOTO1 AIO,IORDD+IOGENDIR                                               
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    GXDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
*                                                                               
RESR010  CLI   0(R3),0                                                          
         BE    RESR100             END OF RECORD                                
         CLI   0(R3),GXFTELQ                                                    
         BE    RESR030                                                          
*                                                                               
RESR020  SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RESR010                                                          
*                                                                               
         USING GXFTEL,R3                                                        
RESR030  EQU   *                   PROCESS FILE TRANSMISION ELEMENT             
         LA    R2,IOKEY                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSFRECQ                                                  
         MVC   GXSFEID,GXFTEID                                                  
         MVC   GXSFAGY,AGYALPH                                                  
         MVC   GXSFSYS,SYSTEM                                                   
         MVC   GXSFSUB,SUBSYS                                                   
         MVC   GXSFFNUM,GXFTFNUM                                                
         GOTO1 AIO,IORDD+IOGENDIR                                               
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    GXDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     RESR020                                                          
*                                                                               
RESR100  EQU   *                                                                
*                                                                               
RESRECX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    GXKEY,GXKEY                                                      
         XC    LASTPAGE,LASTPAGE                                                
         MVI   GXKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GXKREC,GXFKRECQ                                                  
         CLI   APACTN,ACTTRN                                                    
         BNE   *+8                                                              
         MVI   GXKREC,GXSFRECQ                                                  
         XC    SELKEY,SELKEY                                                    
*                                                                               
         MVI   SUBSCRN,SUBSNULL                                                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VSELDAT  EQU   *                                                                
         GOTO1 AFVAL,LSTDATH                                                    
         BNE   VSDATX                                                           
         ZIC   R0,FVILEN                                                        
         MVC   BYTE,CULANG                                                      
         OI    BYTE,PVINSGLO+PVINSGLS                                           
         GOTO1 VPERVAL,PARM,((R0),FVIFLD),(BYTE,WORK)                           
         CLI   4(R1),PVRCONE                                                    
         BNE   EIIF                                                             
         MVC   SELDAT,WORK+PVALCSTA-PERVALD                                     
VSDATX   EQU   *                                                                
*                                                                               
VSELTIM  EQU   *                                                                
         GOTO1 AFVAL,LSTTIMH                                                    
         BNE   VSTIMX                                                           
         GOTO1 VALTIM,LSTTIMH                                                   
         BNE   EIIF                                                             
         MVC   SELTIM,APFULL                                                    
VSTIMX   EQU   *                                                                
*                                                                               
VSELEID  EQU   *                                                                
         GOTO1 AFVAL,LSTEIDH                                                    
         BNE   VSEIDX                                                           
         MVC   SELEID,FVIFLD                                                    
         CLI   APACTN,ACTTRN                                                    
         BNE   VSEIDX                                                           
         MVC   GXSFEID,SELEID                                                   
VSEIDX   EQU   *                                                                
*                                                                               
VSELAGY  EQU   *                                                                
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VSAGYX                                                           
         GOTO1 VALAGY,LSTAGYH                                                   
         BNE   EIIF                                                             
         MVC   SELAGY,FVIFLD                                                    
         CLI   APACTN,ACTTRN                                                    
         BE    *+14                                                             
         MVC   GXFKAGY,SELAGY                                                   
         B     *+10                                                             
         MVC   GXSFAGY,SELAGY                                                   
VSAGYX   EQU   *                                                                
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
         CLI   APACTN,ACTTRN                                                    
         BE    *+14                                                             
         MVC   GXFKSYS,SELSYS                                                   
         B     *+10                                                             
         MVC   GXSFSYS,SELSYS                                                   
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
         CLI   APACTN,ACTTRN                                                    
         BE    *+14                                                             
         MVC   GXFKSUB,SELSUB                                                   
         B     *+10                                                             
         MVC   GXSFSUB,SELSUB                                                   
         MVC   LSTSUB,APWORK                                                    
         OI    LSTSUBH+6,X'80'                                                  
VSSUBX   EQU   *                                                                
*                                                                               
VSELGNUM EQU   *                                                                
         GOTO1 AFVAL,LSTGNUMH                                                   
         BNE   VSGNUX                                                           
         GOTO1 =V(NUMVAL),PARM,LSTGNUM,(X'01',0),RR=APRELO                      
         CLI   0(R1),X'FF'                                                      
         BE    EIIF                                                             
         L     R1,4(R1)                                                         
         STCM  R1,3,SELGNUM                                                     
VSGNUX   EQU   *                                                                
*                                                                               
VSELCOM  EQU   *                                                                
         GOTO1 AFVAL,LSTCOMH                                                    
         BNE   VSCOMX                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VSCO010                                                          
         CLI   FVIFLD,C'Y'                                                      
         BE    VSCO010                                                          
         CLI   APACTN,ACTTRN                                                    
         BNE   EIIF                                                             
         CLI   FVIFLD,C'L'                                                      
         BE    VSCO010                                                          
         B     EIIF                                                             
VSCO010  EQU   *                                                                
         MVC   SELCOM,FVIFLD                                                    
         MVC   LSTCOM,FVIFLD                                                    
         OI    LSTCOMH+6,X'80'                                                  
VSCOMX   EQU   *                                                                
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
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
*                                                                               
GETSEL2  EQU   *                                                                
         TM    APINDS,2            TEST SEQUENCE BROKEN                         
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
*                                                                               
         GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    RF,=H'9'                                                         
         D     RE,=F'10'           90 PERCENT OF MAX IOS IN RF                  
         CLM   RF,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    GETSEL9                                                          
         MVC   FVMSGNO,=AL2(CE#IOCNT)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    RF,ESSACTH                                                       
         ST    RF,APCURSOR                                                      
         B     GETSELX                                                          
         DROP  R1                                                               
*                                                                               
GETSEL9  LA    R2,IOKEY                                                         
         MVC   APRECKEY(L'GXKEY),GXKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         CLI   APACTN,ACTTRN                                                    
         BE    GETSEL10                                                         
         CLI   GXKREC,GXFKRECQ     CHECK STILL XFILE RECORD                     
         BNE   GETSELN                                                          
         B     GETSEL20                                                         
GETSEL10 CLI   GXKREC,GXSFRECQ     CHECK STILL XFILE TRANS PASSIVE              
         BNE   GETSELN                                                          
         OC    SELEID,SELEID                                                    
         BZ    GETSEL20                                                         
         CLC   SELEID,GXSFEID                                                   
         BNE   GETSEL8                                                          
         B     GETSEL20                                                         
         SPACE 1                                                                
*                                                                               
GETSEL20 EQU   *                                                                
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BNE   GETSEL8                                                          
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
GSDAT    EQU   *                                                                
         OC    SELDAT,SELDAT                                                    
         BZ    GSDATX                                                           
         CLC   SELDAT,GXFKDAT                                                   
         BH    GETSEL8                                                          
GSDATX   EQU   *                                                                
*                                                                               
GSTIM    EQU   *                                                                
         OC    SELTIM,SELTIM                                                    
         BZ    GSTIMX                                                           
         CLC   SELTIM,GXFKTIM                                                   
         BH    GETSEL8                                                          
GSTIMX   EQU   *                                                                
*                                                                               
GSAGY    EQU   *                                                                
         OC    SELAGY,SELAGY                                                    
         BZ    GSAGYX                                                           
         CLC   SELAGY,GXFKAGY                                                   
         BNE   GETSEL8                                                          
GSAGYX   EQU   *                                                                
*                                                                               
GSSYS    EQU   *                                                                
         OC    SELSYS,SELSYS                                                    
         BZ    GSSYSX                                                           
         CLC   SELSYS,GXFKSYS                                                   
         BNE   GETSEL8                                                          
GSSYSX   EQU   *                                                                
*                                                                               
GSSUB    EQU   *                                                                
         OC    SELSUB,SELSUB                                                    
         BZ    GSSUBX                                                           
         CLC   SELSUB,GXFKSUB                                                   
         BNE   GETSEL8                                                          
GSSUBX   EQU   *                                                                
*                                                                               
GSEID    EQU   *                                                                
         OC    SELEID,SELEID                                                    
         BZ    GSEIDX                                                           
         CLI   APACTN,ACTTRN                                                    
         BE    GSEIDX                                                           
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
         SR    RF,RF                                                            
         USING GXFTEL,R3                                                        
GSEI010  CLI   GXFTEL,0                                                         
         BE    GETSEL8                                                          
         CLI   GXFTEL,GXFTELQ                                                   
         BNE   *+14                                                             
         CLC   SELEID,GXFTEID                                                   
         BE    GSEIDX                                                           
         IC    RF,GXFTELL                                                       
         AR    R3,RF                                                            
         B     GSEI010                                                          
*                                                                               
GSEIDX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GSGNUM   EQU   *                                                                
         OC    SELGNUM,SELGNUM                                                  
         BZ    GSGNUX                                                           
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
         SR    RF,RF                                                            
         USING GXGNEL,R3                                                        
GSGN010  CLI   GXGNEL,0                                                         
         BE    GETSEL8                                                          
         CLI   GXGNEL,GXGNELQ                                                   
         BE    *+14                                                             
         IC    RF,GXGNELL                                                       
         AR    R3,RF                                                            
         B     GSGN010                                                          
         CLC   SELGNUM,GXGNNUM                                                  
         BH    GETSEL8                                                          
*                                                                               
GSGNUX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GSCOM    EQU   *                                                                
         OC    SELCOM,SELCOM                                                    
         BZ    GSCOMX                                                           
*                                                                               
GSCO010  EQU   *                                                                
         CLI   APACTN,ACTTRN                                                    
         BNE   GSCO100                                                          
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
         SR    RF,RF                                                            
         USING GXFTEL,R3                                                        
GSCO012  CLI   GXFTEL,0                                                         
         BE    GETSEL8                                                          
         CLI   GXFTEL,GXFTELQ                                                   
         BNE   GSCO014                                                          
         LA    R2,IOKEY                                                         
         CLC   GXFTEID,GXSFEID                                                  
         BE    GSCO020                                                          
         L     R2,AIOAREA1                                                      
GSCO014  IC    RF,GXFTELL                                                       
         AR    R3,RF                                                            
         B     GSCO012                                                          
GSCO020  EQU   *                                                                
         L     R2,AIOAREA1                                                      
         CLC   GXFTRCO,=CL6'000000'                                             
         BE    GSCO030                                                          
         CLI   SELCOM,C'L'                                                      
         BE    GSCO040                                                          
         CLI   SELCOM,C'Y'                                                      
         BE    GETSEL8                                                          
         B     GSCOMX                                                           
GSCO030  CLI   SELCOM,C'L'                                                      
         BE    GETSEL8                                                          
         CLI   SELCOM,C'N'                                                      
         BE    GETSEL8                                                          
         B     GSCOMX                                                           
         DROP  R3                                                               
GSCO040  EQU   *                                                                
         LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GSCOMX                                                           
         MVI   APINDS,2                                                         
         CLC   APRECKEY(GXSFSUB+L'GXSFSUB-GXKEY),IOKEY                          
         BE    GETSEL                                                           
         LA    R2,IOKEY                                                         
         MVC   GXKEY,APRECKEY                                                   
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         MVC   APRECKEY(L'GXKEY),GXKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         B     GSCOMX                                                           
*                                                                               
GSCO100  EQU   *                                                                
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
         SR    RF,RF                                                            
         MVI   BYTE,C'Y'                                                        
         USING GXFTEL,R3                                                        
GSCO110  CLI   GXFTEL,0                                                         
         BE    GSCO200                                                          
         CLI   GXFTEL,GXFTELQ                                                   
         BE    GSCO120                                                          
GSCO112  IC    RF,GXFTELL                                                       
         AR    R3,RF                                                            
         B     GSCO110                                                          
GSCO120  EQU   *                                                                
         CLC   GXFTRCO,=CL6'000000'                                             
         BE    GSCO112                                                          
         MVI   BYTE,C'N'                                                        
         B     GSCO112                                                          
*                                                                               
GSCO200  EQU   *                                                                
         CLI   BYTE,C'Y'                                                        
         BE    GSCO210                                                          
         CLI   SELCOM,C'Y'                                                      
         BE    GETSEL8                                                          
         B     GSCOMX                                                           
GSCO210  CLI   SELCOM,C'N'                                                      
         BE    GETSEL8                                                          
         B     GSCOMX                                                           
*                                                                               
GSCOMX   EQU   *                                                                
         DROP  R3                                                               
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
DISSEL   EQU   *                                                                
         CLI   APACTN,ACTTRN                                                    
         BE    DISTRN                                                           
         MVI   LSTMODE,0                                                        
         L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                  GET DATA FROM RECORD KEY                     
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GXFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GXFDELQ                                                    
         BE    DSFDEF                                                           
         CLI   0(R3),GXGNELQ                                                    
         BE    DSGNUM                                                           
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GXFDEL,R3                                                        
DSFDEF   EQU   *                                                                
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,GXFDDAC),(X'51',APWORK)                        
         MVC   LISTDAC,APWORK                                                   
         GOTO1 TIMEOUT,APPARM,GXFDTIC,APWORK                                    
         MVC   LISTTIC,APWORK                                                   
         MVC   LISTAGY,GXFDAGY                                                  
         GOTO1 ADISSYS,GXFDSYS                                                  
         MVC   LISTSYS,APWORK                                                   
         GOTO1 DISSUB,GXFDSUB                                                   
         MVC   LISTSUB,APWORK                                                   
         EDIT  GXFDRNM,(8,LISTRNUM),ZERO=NOBLANK,ALIGN=LEFT                     
         MVC   LISTTYP,GXFDTYP                                                  
         MVC   LISTMODE,GXFDMOD                                                 
         B     DSLP1A                                                           
         DROP  R3                                                               
*                                                                               
         USING GXGNEL,R3                                                        
DSGNUM   EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,GXGNNUM                                                     
         EDIT  (RF),(5,LISTGNUM),ZERO=NOBLANK,ALIGN=LEFT                        
         B     DSLP1A                                                           
         DROP  R3                                                               
*                                                                               
DISSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR TRANSACTION PASSIVE POINTER            *         
***********************************************************************         
         SPACE 1                                                                
DISTRN   EQU   *                                                                
         MVI   LSTMODE,1                                                        
         MVC   SAVEEID,GXSFEID                                                  
         LA    R2,IOKEY                                                         
         L     R4,APPARM                                                        
         USING LTRND,R4            R4=A(TRANS LIST/SELECT LINE)                 
*                                  GET DATA FROM RECORD KEY                     
         XC    APWORK,APWORK                                                    
         MVC   LTRNEID,GXSFEID                                                  
         MVC   LTRNAGY,GXSFAGY                                                  
         GOTO1 ADISSYS,GXSFSYS                                                  
         MVC   LTRNSYS,APWORK                                                   
         GOTO1 DISSUB,GXSFSUB                                                   
         MVC   LTRNSUB,APWORK                                                   
         SR    RF,RF                                                            
         ICM   RF,3,GXSFFNUM                                                    
         EDIT  (RF),(8,LTRNFNUM),ZERO=NOBLANK,ALIGN=LEFT                        
*                                  GET DATA FROM ELEMENTS                       
         L     R2,AIOAREA1                                                      
         MVC   SAVKEY,0(R2)                                                     
         LA    R3,GXFIRST(R2)                                                   
DTLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISTRNX                                                          
         CLI   0(R3),GXFTELQ                                                    
         BE    DTFTRN                                                           
         CLI   0(R3),GXGNELQ                                                    
         BE    DTFGNM                                                           
*                                                                               
DTLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DTLP1                                                            
*                                                                               
         USING GXFTEL,R3                                                        
DTFTRN   EQU   *                                                                
         CLC   LTRNEID,GXFTEID                                                  
         BNE   DTLP1A                                                           
         MVI   FTELFLAG,C'Y'                                                    
         OC    GXFTRNO,GXFTRNO                                                  
         BZ    DTFT010                                                          
         MVC   LTRNSNOT(6),GXFTRNO                                              
DTFT010  OC    GXFTRRE,GXFTRRE                                                  
         BZ    DTFT020                                                          
         MVC   LTRNSRCV(6),GXFTRRE                                              
DTFT020  OC    GXFTRCO,GXFTRCO                                                  
         BZ    DTFT030                                                          
         MVC   LTRNSCOM(6),GXFTRCO                                              
         B     DTLP1A                                                           
*                                                                               
DTFT030  LA    R8,GXFTSLST                                                      
         USING GXFTSLST,R8                                                      
         LR    RF,R3                                                            
         SR    R1,R1                                                            
         IC    R1,GXFTELL                                                       
         AR    RF,R1                                                            
         ST    RF,RFSAVE                                                        
         XC    WORK,WORK                                                        
         MVI   WORK+L'GXFTDSCO+L'GXFTTSCO-1,X'FF'                               
         MVI   COMOKFLG,C'N'                                                    
*                                                                               
DTFT100  EQU   *                                                                
         L     RF,RFSAVE                                                        
         CR    RF,R8                                                            
         BNH   DTFT130                                                          
         OC    GXFTRSCO,GXFTRSCO                                                
         BZ    DTFT120                                                          
         CLC   GXFTRSCO,=CL6'000000'                                            
         BNE   DTFT110                                                          
         MVI   COMOKFLG,C'Y'                                                    
         OC    WORK,WORK                                                        
         BNZ   DTFT120                                                          
         MVC   LTRNSCOM(6),GXFTRSCO                                             
         B     DTFT120                                                          
DTFT110  CLC   WORK(L'GXFTDSCO+L'GXFTTSCO),GXFTDSCO                             
         BH    DTFT120                                                          
         MVC   LTRNSCOM(6),GXFTRSCO                                             
         MVC   WORK(L'GXFTDSCO+L'GXFTTSCO),GXFTDSCO                             
DTFT120  LA    R8,GXFTSLQ(R8)                                                   
         B     DTFT100                                                          
DTFT130  CLI   COMOKFLG,C'N'                                                    
         BE    DTLP1A                                                           
         OC    WORK,WORK                                                        
         BZ    DTLP1A                                                           
         MVC   LTRNSCOM(7),=CL7'PARTIAL'                                        
         B     DTLP1A                                                           
         DROP  R8                                                               
*                                                                               
         USING GXGNEL,R3                                                        
DTFGNM   EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,GXGNNUM                                                     
         EDIT  (RF),(8,LTRNFNUM),ZERO=NOBLANK,ALIGN=LEFT                        
         B     DTLP1A                                                           
*                                                                               
DISTRNX  CLI   FTELFLAG,C'Y'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
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
PRTREP   L     R8,AREP                                                          
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
* CONVERT TIME                                                        *         
***********************************************************************         
         SPACE 1                                                                
TIMEOUT  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     R1,0(R2)                                                         
         SRL   R1,28                                                            
         STC   R1,0(R3)                                                         
         OI    0(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,24                                                            
         STC   R1,1(R3)                                                         
         OI    1(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,20                                                            
         STC   R1,2(R3)                                                         
         OI    2(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,16                                                            
         STC   R1,3(R3)                                                         
         OI    3(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,12                                                            
         STC   R1,4(R3)                                                         
         OI    4(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,8                                                             
         STC   R1,5(R3)                                                         
         OI    5(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         SRL   R1,4                                                             
         STC   R1,6(R3)                                                         
         OI    6(R3),X'F0'                                                      
         L     R1,0(R2)                                                         
         STC   R1,7(R3)                                                         
         OI    7(R3),X'F0'                                                      
         B     TOUTOK                                                           
*                                                                               
TOUTNO   B     NO                                                               
TOUTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TIME IN FVIFLD, HHMMSSHT                        *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: CC .EQ. IF OK ELSE .NE., APFULL TIME IN MVS PWOS FORMAT       *         
***********************************************************************         
         SPACE 1                                                                
VALTIM   NTR1                                                                   
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BNE   VTIMNO                                                           
*                                                                               
         MVC   APWORK,=CL8'00000000'                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),FVIFLD                                                 
         PACK  APDUB(5),APWORK(8)                                               
         ICM   RE,15,APDUB                                                      
         ICM   RF,15,APDUB+4                                                    
         SLDL  RE,4                                                             
         ST    RE,APFULL                                                        
         B     VTIMOK                                                           
*                                                                               
VTIMNO   B     NO                                                               
VTIMOK   EQU   *                                                                
         B     YES                                                              
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
***********************************************************************         
* ROUTINE TO SET UP SUB-SCREEN                                        *         
*                                                                     *         
* NTRY: R1=SUB-SCREEN NUMBER                                          *         
***********************************************************************         
         SPACE 1                                                                
SETSUBS  NTR1  ,                                                                
* ??     XC    SUBSTAB,SUBSTAB     CLEAR TAB TO NEXT SCREEN                     
*                                                                               
         CLM   R1,1,SUBSCRN        TEST CHANGE OF SUB-SCREEN                    
         BE    SSUBS2                                                           
         STC   R1,SUBSCRN                                                       
         XC    APCURSOR,APCURSOR                                                
*                                                                               
         MVI   PARM+4,C'R'         SET UP PARAMETER 2 FOR OVERLAY CALL          
         MVC   PARM+5(2),ACSYSPGM                                               
         MVC   PARM+7(1),SUBSCRN                                                
         GOTO1 VCOLY,PARM,XFITABH,,0                                            
         CLI   4(R1),X'FF'         TEST SCREEN LOADED OK                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,ESSMSGH          TRANSMIT SCREEN                              
         XR    RF,RF                                                            
         USING FHD,R1                                                           
         OI    FHOI,FHOITR                                                      
         ICM   RF,1,FHLN                                                        
         BZ    SETSUBSX                                                         
         BXH   R1,RF,*-12                                                       
         DROP  R1                                                               
         SPACE 1                                                                
SSUBS2   LA    RF,XF2EIDH          RF=FIRST FIELD                               
         LA    R1,XF2TENDH         R1=LAST FIELD                                
         USING FHD,RF                                                           
         XR    R0,R0                                                            
         SPACE 1                                                                
SSUBS4   OI    FHAT,FHATPR         PROTECT                                      
         NI    FHAT,X'FF'-FHATHI      UNHIGHLIGHT                               
         OI    FHOI,FHOITR         TRANSMIT                                     
         IC    R0,FHLN                                                          
         LR    RE,R0                                                            
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    FHDA(0),FHDA        CLEAR FIELD                                  
         BXLE  RF,R0,SSUBS4                                                     
         DROP  RF                                                               
         SPACE 1                                                                
SETSUBSX B     EXIT                                                             
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
FTPFILE  DCB   DDNAME=FTPFILE,MACRF=(GM,PM),DSORG=PS,RECFM=VB,         +        
               LRECL=2048,BLKSIZE=8120                                          
         SPACE 2                                                                
* CREATE FTPFILE DATASETS -- DYNAMIC ALLOCATION                                 
*                                                                               
         DS    0F                                                               
ARBLKCTA DC    X'80',AL3(RBLKCATA) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKCATA DC    X'1401000000000000',A(ACATALLT),X'0000000000000000'              
*                                                                               
ACATALLT DC    X'00',AL3(TXTDD)                                                 
         DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTDISP)                                               
         DC    X'00',AL3(TXTNDISP)                                              
         DC    X'00',AL3(TXTBLKLN)                                              
         DC    X'00',AL3(TXTPRI)                                                
         DC    X'00',AL3(TXTSEC)                                                
         DC    X'00',AL3(TXTRLSE)                                               
         DC    X'80',AL3(TXTUNIT)                                               
         SPACE 2                                                                
* FTPFILE DATASETS -- DYNAMIC UNALLOCATION BY DDNAME                            
*                                                                               
         DS    0F                                                               
ARBLKUNA DC    X'80',AL3(RBLKUNA)  R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA  DC    X'1402000000000000',A(ACATUNT),X'0000000000000000'               
*                                                                               
ACATUNT  DC    X'80',AL3(TXTUNDD)                                               
         SPACE 2                                                                
* DELETE FTPFILE DATASETS -- DYNAMIC ALLOCATION                                 
*                                                                               
         DS    0F                                                               
ARBLKDLA DC    X'80',AL3(RBLKDELA) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKDELA DC    X'1401000000000000',A(ADELALLT),X'0000000000000000'              
*                                                                               
ADELALLT DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTDISPO)                                              
         DC    X'80',AL3(TXTNDISD)                                              
         SPACE 2                                                                
* FTPFILE DATASETS -- DYNAMIC UNALLOCATION BY DSN                               
*                                                                               
         DS    0F                                                               
ARBLKUN2 DC    X'80',AL3(RBLKUNA2) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA2 DC    X'1402000000000000',A(ACATUNT2),X'0000000000000000'              
*                                                                               
ACATUNT2 DC    X'80',AL3(TXTDSN)                                                
         SPACE 3                                                                
TXTDD    DC    AL2(DALDDNAM),X'00010007',C'FTPFILE'    DDNAME=FTPFILE           
TXTUNDD  DC    AL2(DUNDDNAM),X'00010007',C'FTPFILE'    DDNAME=FTPFILE           
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTDISP  DC    AL2(DALSTATS),X'0001000104'             DISP=(NEW,.....)         
TXTNDISP DC    AL2(DALNDISP),X'0001000102'                 =(...,CATLG)         
TXTDISPO DC    AL2(DALSTATS),X'0001000101'             DISP=(OLD,...)           
TXTNDISD DC    AL2(DALNDISP),X'0001000104'                 =(...,DEL)           
TXTBLKLN DC    AL2(DALBLKLN),X'00010003',AL3(16500)    SPACE=(16500,...         
TXTPRI   DC    AL2(DALPRIME),X'00010003',AL3(25)            =(..(25,...         
TXTSEC   DC    AL2(DALSECND),X'00010003',AL3(40)            =(...,40),.         
TXTRLSE  DC    AL2(DALRLSE),X'0000'                         =(...,RLSE)         
TXTUNIT  DC    AL2(DALUNIT),X'00010005',C'SYSDA'       UNIT=SYSDA               
         SPACE 2                                                                
FTPDSN   DC    CL44'DDSESS.EDICTX.RCVGLUID.UIDNNNNN.ATTTBBRR.TUU'               
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
* DXSYSLEX                                                                      
       ++INCLUDE DXSYSLEX                                                       
         EJECT                                                                  
* DXSUBLST                                                                      
       ++INCLUDE DXSUBLST                                                       
         EJECT                                                                  
* CTESSWRK                                                                      
       ++INCLUDE CTESSWRK                                                       
         EJECT                                                                  
* DXSUBLSTD                                                                     
       ++INCLUDE DXSUBLSTD                                                      
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSFBD                                                       
         ORG   XFITABH                                                          
       ++INCLUDE CTESSA2D                                                       
         ORG   XFITABH                                                          
       ++INCLUDE CTESSA1D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSDBD                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSA0D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSBBD                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT DISPLAYED SYSTEM                
SAVEEID  DS    CL8                 SAVE ESS EID                                 
SAVKEY   DS    CL(L'APRECKEY)      SAVE TRANS LIST XFILE KEY                    
LASTPAGE DS    CL(L'APRECKEY)      SAVE LAST LIST PAGE START KEY                
LSTMODE  DS    CL1                 LIST MODE (LIST/TRANS)                       
         SPACE 1                                                                
SUBSCRN  DS    XL1                 CURRENT DISPLAYED SUB-SCREEN                 
SUBSNULL EQU   0                   NO SUB-SCREEN DISPLAYED                      
SUBSXF1S EQU   X'A2'               FILE TRANSFER DETAILS SUB SCREEN 1           
SUBSXF2S EQU   X'A1'               FILE TRANSFER DETAILS SUB SCREEN 2           
         SPACE 1                                                                
SAVCLRL  EQU   *-SAVOVER           CLEAR TWA UP TO HERE (FOR SETTWA)            
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTDAC  DS    CL10                                                             
         DS    CL1                                                              
LISTTIC  DS    CL8                                                              
         DS    CL1                                                              
LISTAGY  DS    CL2                                                              
         DS    CL1                                                              
LISTSYS  DS    CL7                                                              
         DS    CL1                                                              
LISTSUB  DS    CL7                                                              
         DS    CL1                                                              
LISTGNUM DS    CL5                                                              
         DS    CL1                                                              
LISTTYP  DS    CL3                                                              
         DS    CL2                                                              
LISTRNUM DS    CL8                                                              
         DS    CL1                                                              
LISTMODE DS    CL1                                                              
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
LTRND    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LTRNACTH DS    XL8                                                              
LTRNACT  DS    CL3                 ACTION FIELD                                 
LTRNLINH DS    CL8                                                              
LTRNLIN  DS    0CL(L'LSTLIN1)                                                   
LTRNEID  DS    CL8                                                              
         DS    CL1                                                              
LTRNAGY  DS    CL2                                                              
         DS    CL1                                                              
LTRNSYS  DS    CL8                                                              
         DS    CL1                                                              
LTRNSUB  DS    CL7                                                              
         DS    CL1                                                              
LTRNFNUM DS    CL8                                                              
         DS    CL2                                                              
LTRNSNOT DS    CL10                                                             
         DS    CL2                                                              
LTRNSRCV DS    CL10                                                             
         DS    CL2                                                              
LTRNSCOM DS    CL10                                                             
         DS    CL2                                                              
         ORG   LTRNLIN+L'LTRNLIN                                                
         EJECT                                                                  
SLUSD    DSECT                     ** ESS SERVER ID LIST DSECT **               
SLUSACTH DS    XL8                                                              
SLUSACT  DS    CL3                 ACTION FIELD                                 
SLUSLINH DS    CL8                                                              
SLUSLIN  DS    0CL(L'XF1LIN1)                                                   
SLUSEID  DS    CL8                                                              
         DS    CL1                                                              
SLUSFNUM DS    CL8                                                              
         DS    CL1                                                              
SLUSDTR  DS    CL10                                                             
         DS    CL1                                                              
SLUSTTR  DS    CL8                                                              
         DS    CL1                                                              
SLUSDCO  DS    CL10                                                             
         DS    CL1                                                              
SLUSTCO  DS    CL8                                                              
         DS    CL1                                                              
SLUSSTAT DS    CL10                                                             
         ORG   SLUSLIN+L'SLUSLIN                                                
         EJECT                                                                  
APPLISTD DSECT                     ** APPLICATION STATUS LIST **                
APPLLINH DS    CL8                                                              
APPLLIN  DS    0CL(L'XF2LIN1)                                                   
APPLKEY  DS    CL8                                                              
         DS    CL10                                                             
APPLDCO  DS    CL10                                                             
         DS    CL1                                                              
APPLTCO  DS    CL8                                                              
         DS    CL2                                                              
APPLRCO  DS    CL8                                                              
         ORG   APPLLIN+L'APPLLIN                                                
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
PARM     DS    6A                                                               
WORK     DS    XL64                                                             
IOKEYSV  DS    XL(L'IOKEY)                                                      
ASE      DS    A                                                                
APGM     DS    A                                                                
RFSAVE   DS    F                                                                
*                                                                               
SLUSLAST DS    A                                                                
SLUSLEN  DS    H                                                                
*                                                                               
APPLLAST DS    A                                                                
APPLLEN  DS    H                                                                
*                                                                               
FCDATE   DS    XL2                 FILE CREATE DATE BINARY COMPRESSED           
FCTIME   DS    XL4                 FILE CREATE TIME PWOS HHMMSSTH               
*                                                                               
LASTCOMK DS    XL(L'APRECKEY)                                                   
*                                                                               
BYTE     DS    XL1                                                              
COMOKFLG DS    CL1                                                              
AGYALPH  DS    CL2                 AGENCY ALPHA ID                              
LASTSYS  DS    XL1                 CONTROL TOF ON CHANGE OF SYSTEM              
SUBSYS   DS    CL1                 SUB SYSTEM CODE                              
SYSTEM   DS    CL1                 SYSTEM CODE                                  
FTELFLAG DS    CL1                                                              
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
*                                                                               
SELKEY   DS    0XL32                                                            
SELDAT   DS    XL2                                                              
SELTIM   DS    XL4                                                              
SELEID   DS    CL8                                                              
SELAGY   DS    CL2                                                              
SELSYS   DS    CL1                                                              
SELSUB   DS    CL1                                                              
SELGNUM  DS    XL2                                                              
SELCOM   DS    CL1                                                              
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
SYSEL    DS    XL(L'APELEM)                                                     
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
         PRINT ON                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100CTESS04   05/16/16'                                      
         END                                                                    
