*          DATA SET GEPAK04    AT LEVEL 004 AS OF 12/08/09                      
*PHASE TF3604A,*                                                                
*NOTE: PHASE=TF35 IN UK, PHASE=TF36 IN US                                       
*                                                                               
*&&      SET   NOP=N               COMMENTED OUT CODE                           
         TITLE 'GEPAK04 - PCPAK DATA FILE VERSION LIST'                         
         SPACE 1                                                                
*WHO  LVL DATE    CHANGE                                                        
         EJECT                                                                  
PAK04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PK04**,R9,RR=RE                                              
         L     RC,0(R1)            RC=A(GENCON GLOBAL STORAGE)                  
         USING GEND,RC                                                          
         L     R7,ASYSD                                                         
         USING SYSWORKD,R7                                                      
         L     RA,ATWA                                                          
         USING PAKFFD,RA                                                        
         ST    RE,SYOVRELO                                                      
         MVI   SYFLID,GENDIRQ                                                   
         OI    SYOVCALL,SYOVYESQ                                                
         EJECT                                                                  
***********************************************************************         
* DETERMINE MODE                                                      *         
***********************************************************************         
         SPACE 1                                                                
MODECHK  CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         SPACE 2                                                                
* GENERAL EXIT POINTS                                                           
*                                                                               
EXITBAD  LTR   RB,RB                                                            
         B     *+6                                                              
EXITOK   CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VKEY     CLI   TWASYSID,NULLQ      SKIP 1ST TIME IN OR SAME KEY AS LAST         
         BE    VKEYX                                                            
         TM    SCRKEYH+FHIID,FHIITH                                             
         BZ    *+12                                                             
         MVI   TWASYSID,NULLQ                                                   
         B     VKEYX                                                            
*                                                                               
         BAS   RE,CHKLOCK          ENSURE SYSTEM STILL NOT LOCKED               
         BNE   VKERR01             SYSTEM LOCKED SINCE LAST TRN                 
*                                                                               
         SPACE 2                                                                
VKEYX    B     EXIT                                                             
         SPACE 1                                                                
VKERR01  MVI   ERROR,SYSLOCKQ      SYSTEM LOCKED FOR UPDATE                     
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VREC     LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
*                                                                               
         CLI   TWASYSID,NULLQ      1ST PASS GET SYS/LANG                        
         BNE   VR100                                                            
         SPACE 1                                                                
* KEY CONTAINS VERSION/SYS/LANG                                                 
*                                                                               
         LA    R2,SCRKEYH                                                       
         LA    R0,SCANWRK          CLEAR SCANNER BLOCK                          
         LHI   R1,SCANWKLQ                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 SCANNER,DMCB,('SCXTRA2Q',(R2)),SCANWRK,C',=/='                   
         CLI   4(R1),3                                                          
         BNE   VRERR01A            KEY FIELD ERROR 1                            
         SPACE 1                                                                
* GET SYSTEM NAME                                                               
*                                                                               
         LA    R2,SCANWRK+SCBLKLQ+SCXTRAQ    SKIP 1ST ENTRY                     
         USING SCANBLKD,R2                                                      
         CLI   SC2NDLEN,NULLQ                                                   
         BNE   VRERR01B                                                         
         CLI   SC1STLEN,2                                                       
         BL    VRERR01C                                                         
         SR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         L     RE,S#ASYSL                                                       
         LA    RE,6(RE)            RE=A(SYSTEM LIST)                            
         USING SYSLSTD,RE                                                       
*                                                                               
VR010    CLI   0(RE),NULLQ         TEST EOT                                     
         BE    VRERR02             INVALID SYSTEM                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),SC1STFLD                                             
         BE    VR020                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLSHRT(0),SC1STFLD                                             
         BE    VR020                                                            
         LA    RE,SYSLLEN(RE)                                                   
         B     VR010                                                            
VR020    MVC   TWASYSID,SYSLNUM                                                 
*&&US                                                                           
         CLI   TWASYSID,X'0D'      SYSTEM=STR                                   
         BNE   *+8                 NO                                           
         MVI   TWASYSID,X'02'      YES - OVERRIDE STR W/ SPT                    
*&&                                                                             
         DROP  RE                                                               
         SPACE 1                                                                
* GET LANGUAGE EXTENSION                                                        
*                                                                               
         LA    R2,SCBLKLQ+SCXTRAQ(R2)        NEXT ENTRY                         
         CLI   SC2NDLEN,NULLQ                                                   
         BNE   VRERR01D                                                         
         CLI   SC1STLEN,2                                                       
         BL    VRERR01E                                                         
         SR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         L     RE,VLANGEXT                                                      
         USING LANGEXTD,RE                                                      
*                                                                               
VR030    CLI   LANGCODE,EOTQ                                                    
         BE    VRERR03             INVALID LANGUAGE                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LANGEXT(0),SC1STFLD                                              
         BE    VR040                                                            
         LA    RE,LANGELQ(RE)                                                   
         B     VR030                                                            
*                                                                               
VR040    MVC   TWALEXT,LANGCODE                                                 
         SPACE 1                                                                
         BAS   RE,CHKLOCK          ENSURE SYSTEM AVAILABLE                      
         BL    VRERR04             VESRION CONTROL NOT YET IMPLEMENTED          
         BH    VRERR05             SYSTEM LOCKED                                
*                                                                               
         ICM   R3,15,SYAELEM       GET A(VERSION CONTROL ELEM)                  
         USING GPKCD,R3                                                         
         MVC   DSPWORK,SPACEPAD                                                 
         MVC   DSPWORK(L'VERTXT),VERTXT                                         
         LA    RF,DSPWORK+L'VERTXT                                              
         GOTO1 DATCON,DMCB,(2,GPKCDATE),(20,(RF))                               
         LA    RF,DSPWORK+L'VERTXT+8                                            
         SR    R1,R1                                                            
         ICM   R1,3,GPKCTIME                                                    
         CVD   R1,DUB                                                           
         UNPK  0(4,RF),DUB                                                      
         OI    3(RF),ZEROQ                                                      
         GOTO1 GENDSP,SCRL03H                                                   
         DROP  R3                                                               
         LA    R2,SCRL04H                                                       
         LA    R0,(SCRL24H-SCRL04H)/(SCRL06H-SCRL05H)+1                         
         SPACE 1                                                                
         XC    KEY,KEY             SET INITIAL KEY                              
         MVI   GPKREC,GPKRECQ                                                   
         MVC   GPKSYS,TWASYSID                                                  
         MVC   GPKLEXT,TWALEXT                                                  
         B     VR120                                                            
         SPACE 2                                                                
* LIST ALL FILES FOR SYSTEM/LANGUAGE                                            
*                                                                               
VR100    MVC   KEY,TWASVKEY        RESTORE LAST KEY                             
         SPACE 1                                                                
*                                                                               
VR110    LA    R2,SCRL03H                                                       
         LA    R0,(SCRL24H-SCRL03H)/(SCRL04H-SCRL03H)+1                         
         SPACE 1                                                                
VR120    IC    R1,GPKFILE+L'GPKFILE-1        BUMP TO FIRST/NEXT FILE            
         LA    R1,1(R1)                                                         
         STC   R1,GPKFILE+L'GPKFILE-1                                           
         MVI   GPKSEQN,NULLQ                                                    
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   GPKEY(GPKLEXT-GPKEY+L'GPKLEXT),KEYSAVE                           
         BNE   VR130                                                            
         LA    R5,DSPWORK                                                       
         USING LLINED,R5                                                        
         MVC   LFILEID,GPKFILE                                                  
         CLI   GPKSEQN,NULLQ       CORRUPT FILE IF NOT A SEQ ZERO REC           
         BE    VR122                                                            
         MVC   LVERDATE(L'FLERRTXT),FLERRTXT                                    
         B     VR126                                                            
*                                                                               
VR122    GOTO1 VGETREC                                                          
         SPACE 1                                                                
         XC    SYAELEM,SYAELEM     GET VERSION INFO ELEM                        
         MVI   ELCODE,GPKVELQ                                                   
         GOTO1 VGETEL,AIO                                                       
         BE    VR124                                                            
         MVC   LVERDATE(L'ELERRTXT),ELERRTXT                                    
         B     VR126                                                            
*                                                                               
VR124    ICM   R3,15,SYAELEM                                                    
         USING GPKVD,R3                                                         
         GOTO1 DATCON,DMCB,(2,GPKVDATE),(20,LVERDATE)                           
         SR    R1,R1                                                            
         ICM   R1,3,GPKVTIME                                                    
         CVD   R1,DUB                                                           
         UNPK  LVERTIME,DUB                                                     
         OI    LVERTIME+L'LVERTIME-1,ZEROQ                                      
         LR    RE,R0               EDIT DESTROYS R0                             
         EDIT  (B2,GPKVSCRS),(4,LSCREENS),ZERO=NOBLANK                          
         LR    R0,RE                                                            
         CLI   GPKVELL,GPKVLNQ                                                  
         BL    *+14                                                             
         MVC   LPID,GPKVPID                                                     
         B     *+10                                                             
         MVC   LPID(L'NOPIDTXT),NOPIDTXT                                        
         DROP  R3,R5                                                            
*                                                                               
VR126    GOTO1 GENDSP,(R2)                                                      
         LA    R2,(SCRL04H-SCRL03H)(R2)                                         
         BCT   R0,VR120                                                         
         SPACE 1                                                                
         MVC   TWASVKEY,KEY                                                     
         MVC   HALF,=AL2(ILSTDSPQ)                                              
         B     VRECOKX                                                          
         SPACE 1                                                                
* END OF FILES FOR SYSTEM                                                       
*                                                                               
VR130    LTR   R0,R0               TEST IF UNUSED LINES ON SCREEN               
         BZ    VR132                                                            
         TWAXC (R2),SCRL24H                                                     
*                                                                               
VR132    MVC   HALF,=AL2(ILSTENDQ)                                              
         MVI   TWASYSID,NULLQ                                                   
         B     VRECOKX                                                          
         SPACE 2                                                                
VRECOKX  XC    GETTXTCB,GETTXTCB   SET OK MESSAGE                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMSYS,GTGENSYS                                                  
         MVI   GTMTYP,GTMINF                                                    
         OI    GT1INDS,GT1REF                                                   
         DROP  R1                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
         SPACE 1                                                                
VRECX    B     EXIT                                                             
         SPACE 2                                                                
VRERRORS EQU   *                                                                
VRERR01A MVI   SYFLDIX,(*-VRERRORS)/8+1      ERR01A-Y = KEY ERRORS 1-25         
         B     VRERR01Z                                                         
VRERR01B MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01C MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01D MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01E MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01F MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01G MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01H MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01I MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01J MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01K MVI   SYFLDIX,(*-VRERRORS)/8+1                                         
         B     VRERR01Z                                                         
VRERR01Z MVI   ERROR,KEYERRQ       KEY FIELD ERROR &I                           
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
*                                                                               
VRERR02  MVI   ERROR,INVSYSQ       INVALID SYSTEM                               
         B     TRAPERR                                                          
VRERR03  MVI   ERROR,INVLANGQ      INVALID LANGUAGE                             
         MVI   TWASYSID,NULLQ      FORCE REVALIDATION                           
         B     TRAPERR                                                          
VRERR04  MVI   ERROR,NOVRCTLQ      VERSION CONTROL NOT YET IMPLEMENTED          
         B     TRAPERR                                                          
VRERR05  MVI   ERROR,SYSLOCKQ      SYSTEM LOCKED FOR UPDATE                     
         B     TRAPERR                                                          
VRERR06  MVI   ERROR,USERHIQ       USERS VERSION HIGHER THAN CURRENT            
         B     TRAPERR                                                          
VRERR07  MVI   ERROR,NOFILESQ      FAILED TO FIND ANY NEWER VERSIONS            
         B     TRAPERR                                                          
VRERR08  MVI   ERROR,ALLOCQ        TSAR RESTORE FAILED                          
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* GET CONTROL RECORD AND ENSURE SYSTEM NOT LOCKED                     *         
***********************************************************************         
         SPACE 1                                                                
CHKLOCK  NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GPAKD,R4                                                         
         OC    GPDDA,TWACTLDA      SKIP DIRECTORY READ IF D/A KNOWN             
         BNZ   CHLK010                                                          
*                                                                               
         MVI   GPKREC,GPKRECQ                                                   
         OI    SYERRFLG,RETNERRQ                                                
         MVI   BYTE,NULLQ                                                       
         GOTO1 VREAD                                                            
         BNE   CHLKERR1            NOT YET IMPLEMENTED                          
         MVC   TWACTLDA,DMDSKADD   SAVE FOR SUBSEQUENT TRNS                     
         SPACE 1                                                                
CHLK010  GOTO1 VGETREC                                                          
         XC    SYAELEM,SYAELEM     GET RELEVANT CONTROL ELEM                    
         MVI   ELCODE,GPKCELQ                                                   
*                                                                               
CHLK020  GOTO1 VGETEL,AIO                                                       
         BNE   CHLKERR1            NOT YET IMPLEMENTED FOR SYS/LANG             
         ICM   R1,15,SYAELEM                                                    
         USING GPKCD,R1                                                         
         CLC   TWASYSLX,GPKCSYS    TEST SYSTEM/LANGEXT                          
         BNE   CHLK020                                                          
         SPACE 1                                                                
         TM    GPKCFLAG,GPKCLCKQ   TEST IF SYSTEM LOCKED                        
         BNZ   CHLKERR2                                                         
         CR    RB,RB                                                            
         B     CHLKSYSX                                                         
         SPACE 1                                                                
CHLKERR1 CR    RB,R9               FORCE CC LOW (NO CTL REC/ELEM)               
         B     *+6                                                              
CHLKERR2 CR    R9,RB               FORCE CC HIGH (LOCKED)                       
         SPACE 1                                                                
CHLKSYSX B     EXIT                                                             
         DROP  R1,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VARIOUS SHORT ROUTINES                                             *          
**********************************************************************          
         SPACE 1                                                                
**********************************************************************          
* MOVE FROM DSPWORK TO TWA AND XMIT IF REQUIRED - CLEARING DSPWORK   *          
**********************************************************************          
         SPACE 1                                                                
GENDSP   ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         IC    RE,FHLND(R1)                                                     
         SH    RE,=Y(FHDAD+1)                                                   
         TM    FHATD(R1),FHATXH    TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    RE,=Y(FHDAD)                                                     
         EX    RE,GENCLC           IS DATA ON SCREEN ALREADY                    
         BE    GENDSP10            YES- RETURN (CC=)                            
         EX    RE,GENMVC           NO - SO MOVE IT THERE                        
         OI    FHOID(R1),FHOITR                                                 
GENDSP10 EX    RE,GENCLR                                                        
         L     RE,SAVERE                                                        
         BR    RE                  RETURN (CC NOT=)                             
         SPACE 1                                                                
GENCLC   CLC   FHDAD(0,R1),DSPWORK                                              
GENMVC   MVC   FHDAD(0,R1),DSPWORK                                              
GENCLR   MVC   DSPWORK(0),SPACEPAD                                              
         SPACE 2                                                                
TRAPERR  NI    GENSTAT2,X'FF'-USGETTXT                                          
*                                                                               
TRAPERR1 GOTO1 VERRXIT                                                          
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GEPAK04TC                                                      
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE GEPAKDS                                                        
         EJECT                                                                  
PAKFFD   DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE GEPAKFED                                                       
         EJECT                                                                  
***********************************************************************         
* LOCALLY DEFINED DSECTS                                              *         
***********************************************************************         
         SPACE 1                                                                
LLINED   DSECT                                                                  
LFILEID  DS    CL(L'GPKFILE)                                                    
         DS    CL1                                                              
LVERDATE DS    CL8                                                              
         DS    CL1                                                              
LVERTIME DS    CL4                                                              
         DS    CL1                                                              
LSCREENS DS    CL4                                                              
         DS    CL1                                                              
LPID     DS    CL(L'GPKVPID)                                                    
         EJECT                                                                  
***********************************************************************         
* REDEFINED OVERLAY WORK AREA                                         *         
***********************************************************************         
         SPACE 1                                                                
SYSWORKD DSECT                                                                  
         ORG   SYOVWORK                                                         
         SPACE 1                                                                
***********************************************************************         
* REDEFINED TWA0 SPARE AREA                                           *         
***********************************************************************         
         SPACE 1                                                                
PAKFFD   DSECT                                                                  
         ORG   SCRWORK+128                                                      
         DS    0A                                                               
TWAFLDS  DS    0XL(TWAFLDLQ)                                                    
TWAFSTRT EQU   *                                                                
TWASYSLX DS    0XL(L'GPKSYSLX)                                                  
TWASYSID DS    XL1                 EQUATED SYSTEM NUMBER                        
TWALEXT  DS    XL1                 LANGUAGE FILE EXTENSION CODE                 
         DS    XL2                 N/D (WORD ALIGN)                             
*                                                                               
TWACTLDA DS    XL(L'GPDDA)         DA OF CONTROL REC                            
TWASVKEY DS    XL(GPKEYLQ)         LAST REC READ                                
*                                                                               
TWAFLDLQ EQU   *-TWAFSTRT                                                       
TWAUSEQ  EQU   *-PAKFFD                                                         
         ORG   PAKFFD+(TWAUSRLQ-TWAUSEQ) ASM ERR IF TWA USAGE > MAX             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GEPAK04   12/08/09'                                      
         END                                                                    
