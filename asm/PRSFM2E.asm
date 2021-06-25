*          DATA SET PRSFM2E    AT LEVEL 015 AS OF 12/17/13                      
*PHASE T41C2EA                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C2E - CLIADV MAINT                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS CHG, DISP                                    *         
*                                                                     *         
*  INPUTS       SCREEN T41CAF (CLIADV MAINTENANCE)                    *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C2E - CLIADV MAINT'                                          
*                                                                               
T41C2E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C2E,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         JE    EXIT                YES, DISPLAY INIT'D SCR                      
*                                                                               
* FOLLOWING ACTIONS ARE INVALID (SHOULD BE CHECKED IN PRSFM00)                  
*                                                                               
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREST      ACTION RESTORE?                              
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JE    RECACERR                                                         
*                                                                               
******** CLI   PFAID,0             PFKEY IS PRESSED?                            
******** BE    *+12                NO                                           
******** BRAS  RE,CKPFKEYS                                                      
******** JNE   PFKEYERR            INVALID PFKEY IS PRESSED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    PPTRS                                                            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,CLAMEDH          POINT TO MEDIA FLD                           
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   CLAMEDN,MEDNM                                                    
         OI    CLAMEDNH+6,X'80'    DISPLAY MEDIA NAME                           
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PCLTKEY,R6                                                       
*                                                                               
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'      RECORD CODE FOR CLIENT                       
*                                                                               
         LA    R2,CLACLTH          POINT TO CLIENT FLD                          
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         JE    INVFDERR            ALL IS NOT ALLOWED AS CLIENT CODE            
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   PCLTKCLT,QCLT                                                    
         MVC   CLACLTN,CLTNM                                                    
         OI    CLACLTNH+6,X'80'                                                 
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE RECORD                              
         BRAS  RE,VALI_REC                                                      
         BE    VRX                                                              
         L     R2,SVERRFLD                                                      
         J     TRAPERR                                                          
*                                                                               
VRX      B     DR                  VALIDATED RECORD, GO DISPLAY IT              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         BRAS  RE,DISP_REC                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         BRAS  RE,DISP_KEY                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPTRS    DS    0H                  REC IS JUST CHANGED                          
*                                                                               
         BRAS  RE,PUTREQRC         TO REQUEST AUTO T/A REPORTS                  
         BE    PPTRS_X                                                          
         LHI   R2,65               CANNOT GENERATE REQ FOR T/A REPORT           
         BRAS  RE,GET_ITXT                                                      
         LA    R2,CONACTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
PPTRS_X  B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
*                                                                               
MSSNGERR MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,INVRCACT      INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,85            SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,89            CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,53            RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,62            INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,68            INVALID DATE FORMAT                          
         J     TRAPERR                                                          
*                                                                               
MAXLNERR MVI   ERROR,90            MAXIMUM RECORD SIZE EXCEEDED                 
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,88            INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    RE,KEY                                                           
         CLI   3(RE),X'02'         CLIENT RECORD CODE?                          
         BNE   INITI50                                                          
         USING PCLTKEY,RE                                                       
         LA    R2,CLAMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PCLTKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,CLACLTH          CLIENT FLD ON MAINT SCR                      
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PCLTKCLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  RE                                                               
*                                                                               
INITI50  MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
*                                                                               
INITI70  DS    0H                                                               
*                                                                               
INITIX   J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,2             PF2, CLIENT?                                 
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
*                                                                               
         J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
         CLI   PFAID,2             RECORD IS CLIENT2?                           
         BNE   CKPFK15                                                          
CKPFK10H MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             RECORD IS CLIENT?                            
         BNE   CKPFK20                                                          
         B     CKPFK10H                                                         
*                                                                               
CKPFK20  CLI   PFAID,6             RECORD IS PRD?                               
         BNE   CKPFK21                                                          
         MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK21  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             CLIENT MAINT?                                
         BNE   CKPFK30                                                          
         MVC   DUB,=C'CHANGE  '                                                 
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    CKPFK40                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    CKPFK40                                                          
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    CKPFK40                                                          
         CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
         BE    CKPFK40                                                          
         CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
         BE    CKPFK40                                                          
         MVC   DUB,=C'DISPLAY '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLIENT LIST?                                 
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK35                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK35  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CLAMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CLAMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CLACLTH,,GLVPRCLT   CLIENT                
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALI_REC NTR1  BASE=*,LABEL=*      VALIDATE RECORD                              
*                                                                               
         XC    ERROR,ERROR                                                      
         XC    SVERRFLD,SVERRFLD                                                
*                                                                               
         L     R6,AIO              EDIT ADVERTISER ELEMENT                      
         MVI   ELCODE,X'15'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VREC_10                                                          
         GOTO1 REMELEM             REMOVE ADVERTISER ELEM                       
*                                                                               
VREC_10  MVC   ELEM(50),SPACES     BUILD NEW ADVERTISER ELEMENT                 
         MVC   ELEM(2),=X'1514'                                                 
         LA    R4,ELEM                                                          
         USING PCLTADVE,R4                                                      
*                                                                               
         LA    R2,CLAAORH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    *+12                                                             
VREC_10E MVI   ERROR,MISSING                                                    
         B     VREC_ERR                                                         
*                                                                               
         CLC   8(2,R2),=C'*D'      SPECIAL CODE TO DELETE ELEM                  
         BE    VREC_X                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTAOR(0),CLAAOR                                                
*                                                                               
         LA    R2,CLAADVH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    *+8                                                              
         B     VREC_10E                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTADV(0),CLAADV                                                
*                                                                               
         XC    PCLTADVC,PCLTADVC   BINARY ZEROES IF NOT ENTERED                 
         LA    R2,CLAADVCH                                                      
         CLI   5(R2),0                                                          
         BE    VREC_20             CLIENT CODE NOT REQUIRED                     
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCLTADVC(0),CLAADVC                                              
*                                                                               
VREC_20  XC    WORK,WORK           VALIDATE DATES                               
         LA    R2,CLADATEH                                                      
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    *+8                                                              
         B     VREC_10E                                                         
*                                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         L     RF,AIO3                                                          
         GOTO1 PERVAL,DMCB,((R5),8(R2)),(X'20',(RF))                            
         CLI   DMCB+4,X'01'                                                     
         BE    VREC_30E                                                         
         CLI   DMCB+4,X'02'                                                     
         BE    VREC_30E                                                         
         L     R5,AIO3                                                          
         USING PERVALD,R5                                                       
         MVC   PCLTASDT,PVALBSTA                                                
         MVC   PCLTAEDT,PVALBEND                                                
         B     VREC_40                                                          
*                                                                               
VREC_30E MVI   ERROR,68            INVALID DATE FORMAT                          
         B     VREC_ERR                                                         
*                                                                               
VREC_40  LA    R2,CLAAORSH         AOR SE NUMBER                                
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    *+8                                                              
         B     VREC_10E                                                         
*                                                                               
         LA    R3,CLAAORS                                                       
         MVI   WORK,2              FLAG TO RETURN SE NUM                        
         BRAS  RE,SEFORM                                                        
         CLI   WORK,C' '                                                        
         BNE   *+12                                                             
VREC_40E MVI   ERROR,INVALID                                                    
         B     VREC_ERR                                                         
         MVC   PCLTAORS,WORK       SE NUMBER                                    
*                                                                               
         XC    PCLTACON,PCLTACON   CLEAR CONTROL BYTES                          
*                                                                               
         CLI   CLAPLR,C'Y'                                                      
         BNE   *+8                                                              
         OI    PCLTACON,X'01'                                                   
*                                                                               
         CLI   CLACRLC,C'Y'                                                     
         BNE   *+8                                                              
         OI    PCLTACON,X'02'                                                   
*                                                                               
         CLI   CLACLLC,C'Y'                                                     
         BNE   *+8                                                              
         OI    PCLTACON,X'04'                                                   
*                                                                               
         CLI   CLACRLB,C'Y'                                                     
         BNE   *+8                                                              
         OI    PCLTACON,X'08'                                                   
*                                                                               
         CLI   CLAASC,C'Y'                                                      
         BNE   *+8                                                              
         OI    PCLTACON,X'10'                                                   
*                                                                               
         CLI   CLANAC,C'Y'                                                      
         BNE   *+8                                                              
         OI    PCLTACON,X'20'                                                   
*                                                                               
         CLI   CLAACL,C'Y'                                                      
         BNE   *+8                                                              
         OI    PCLTACON,X'40'                                                   
*                                                                               
         CLI   CLAAID,C'Y'                                                      
         BNE   *+8                                                              
         OI    PCLTACON,X'80'                                                   
*                                                                               
         CLI   CLAACA,C'Y'                                                      
         BNE   *+8                                                              
         OI    PCLTACON+1,X'01'                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
         B     VREC_X                                                           
*                                                                               
VREC_ERR ST    R2,SVERRFLD                                                      
         J     SETCCNEQ                                                         
*                                                                               
VREC_X   J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISP_REC NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         XC    CLAAOR,CLAAOR                                                    
         OI    CLAAORH+6,X'80'                                                  
*                                                                               
         XC    CLAADV,CLAADV                                                    
         OI    CLAADVH+6,X'80'                                                  
*                                                                               
         XC    CLAADVC,CLAADVC                                                  
         OI    CLAADVCH+6,X'80'                                                 
*                                                                               
         XC    CLADATE,CLADATE                                                  
         OI    CLADATEH+6,X'80'                                                 
*                                                                               
         XC    CLAAORS,CLAAORS                                                  
         OI    CLAAORSH+6,X'80'                                                 
*                                                                               
         MVI   CLAPLR,C'N'                                                      
         OI    CLAPLRH+6,X'80'                                                  
*                                                                               
         MVI   CLACRLC,C'N'                                                     
         OI    CLACRLCH+6,X'80'                                                 
*                                                                               
         MVI   CLACLLC,C'N'                                                     
         OI    CLACLLCH+6,X'80'                                                 
*                                                                               
         MVI   CLACRLB,C'N'                                                     
         OI    CLACRLBH+6,X'80'                                                 
*                                                                               
         MVI   CLAASC,C'N'                                                      
         OI    CLAASCH+6,X'80'                                                  
*                                                                               
         MVI   CLANAC,C'N'                                                      
         OI    CLANACH+6,X'80'                                                  
*                                                                               
         MVI   CLAACL,C'N'                                                      
         OI    CLAACLH+6,X'80'                                                  
*                                                                               
         MVI   CLAAID,C'N'                                                      
         OI    CLAAIDH+6,X'80'                                                  
*                                                                               
         MVI   CLAACA,C'N'                                                      
         OI    CLAACAH+6,X'80'                                                  
*                                                                               
         MVI   CLAASE2,C' '                                                     
         OI    CLAASE2H+6,X'80'                                                 
*                                                                               
         MVI   CLAPLR2,C' '                                                     
         OI    CLAPLR2H+6,X'80'                                                 
*                                                                               
         MVI   CLACRL2,C' '                                                     
         OI    CLACRL2H+6,X'80'                                                 
*                                                                               
         MVI   CLACLL2,C' '                                                     
         OI    CLACLL2H+6,X'80'                                                 
*                                                                               
         MVI   CLACLB2,C' '                                                     
         OI    CLACLB2H+6,X'80'                                                 
*                                                                               
         MVI   CLAASC2,C' '                                                     
         OI    CLAASC2H+6,X'80'                                                 
*                                                                               
         MVI   CLANAC2,C' '                                                     
         OI    CLANAC2H+6,X'80'                                                 
*                                                                               
         MVI   CLAACL2,C' '                                                     
         OI    CLAACL2H+6,X'80'                                                 
*                                                                               
         MVI   CLAAID2,C' '                                                     
         OI    CLAAID2H+6,X'80'                                                 
*                                                                               
         MVI   CLAACA2,C' '                                                     
         OI    CLAACA2H+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'       LOOK FOR ADVERTISER ELEMENT                   
         BRAS  RE,GETEL                                                         
         JNE   EXIT                                                             
*                                                                               
         XC    WORK,WORK                                                        
         USING PCLTADVE,R6                                                      
         MVC   CLAAOR,PCLTAOR                                                   
         OI    CLAAORH+6,X'80'                                                  
*                                                                               
         MVC   CLAADV,PCLTADV                                                   
         OI    CLAADVH+6,X'80'                                                  
*                                                                               
         MVC   CLAADVC,PCLTADVC                                                 
         OI    CLAADVCH+6,X'80'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCLTASDT),(5,WORK)                                
         CLI   PCLTAEDT,X'FF'                                                   
         BE    FDAT10                                                           
         MVI   WORK+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,PCLTAEDT),(5,WORK+9)                              
FDAT10   MVC   CLADATE(17),WORK                                                 
         OI    CLADATEH+6,X'80'                                                 
*                                                                               
         LA    R3,PCLTAORS                                                      
         MVI   WORK,1              FLAG TO RETURN FILE NUM                      
         BRAS  RE,SEFORM                                                        
         MVC   CLAAORS,WORK                                                     
         OI    CLAAORSH+6,X'80'                                                 
*                                                                               
FCON00   DS    0H                                                               
         MVI   CLAPLR,C'N'                                                      
         OI    CLAPLRH+6,X'80'                                                  
*                                                                               
         MVI   CLACRLC,C'N'                                                     
         OI    CLACRLCH+6,X'80'                                                 
*                                                                               
         MVI   CLACLLC,C'N'                                                     
         OI    CLACLLCH+6,X'80'                                                 
*                                                                               
         MVI   CLACRLB,C'N'                                                     
         OI    CLACRLBH+6,X'80'                                                 
*                                                                               
         MVI   CLAASC,C'N'                                                      
         OI    CLAASCH+6,X'80'                                                  
*                                                                               
         MVI   CLANAC,C'N'                                                      
         OI    CLANACH+6,X'80'                                                  
*                                                                               
         MVI   CLAACL,C'N'                                                      
         OI    CLAACLH+6,X'80'                                                  
*                                                                               
         MVI   CLAAID,C'N'                                                      
         OI    CLAAIDH+6,X'80'                                                  
*                                                                               
         MVI   CLAACA,C'N'                                                      
         OI    CLAACAH+6,X'80'                                                  
*                                                                               
         TM    PCLTACON,X'01'                                                   
         BNO   *+12                                                             
         MVI   CLAPLR,C'Y'                                                      
         OI    CLAPLRH+6,X'80'                                                  
*                                                                               
         TM    PCLTACON,X'02'                                                   
         BNO   *+12                                                             
         MVI   CLACRLC,C'Y'                                                     
         OI    CLACRLCH+6,X'80'                                                 
*                                                                               
         TM    PCLTACON,X'04'                                                   
         BNO   *+12                                                             
         MVI   CLACLLC,C'Y'                                                     
         OI    CLACLLCH+6,X'80'                                                 
*                                                                               
         TM    PCLTACON,X'08'                                                   
         BNO   *+12                                                             
         MVI   CLACRLB,C'Y'                                                     
         OI    CLACRLBH+6,X'80'                                                 
*                                                                               
         TM    PCLTACON,X'10'                                                   
         BNO   *+12                                                             
         MVI   CLAASC,C'Y'                                                      
         OI    CLAASCH+6,X'80'                                                  
*                                                                               
         TM    PCLTACON,X'20'                                                   
         BNO   *+12                                                             
         MVI   CLANAC,C'Y'                                                      
         OI    CLANACH+6,X'80'                                                  
*                                                                               
         TM    PCLTACON,X'40'                                                   
         BNO   *+12                                                             
         MVI   CLAACL,C'Y'                                                      
         OI    CLAACLH+6,X'80'                                                  
*                                                                               
         TM    PCLTACON,X'80'                                                   
         BNO   *+12                                                             
         MVI   CLAAID,C'Y'                                                      
         OI    CLAAIDH+6,X'80'                                                  
*                                                                               
         TM    PCLTACON+1,X'01'                                                 
         BNO   *+12                                                             
         MVI   CLAACA,C'Y'                                                      
         OI    CLAACAH+6,X'80'                                                  
*                                                                               
         MVI   SVAORSE,0           SWITCH TO CONTROL SYSTEM                     
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'          CONTROL SYSTEM GENDIR                        
         GOTO1 SWITCH,DMCB                                                      
         CLI   DMCB+4,0                                                         
         BNE   FSWT10                                                           
*                                                                               
         LA    R6,CTKEY                                                         
         XC    CTKEY,CTKEY                                                      
         USING ADVREC,R6                                                        
         MVI   ADVREC,ADVRECQ                                                   
         MVI   ADVTYP,ADVTYPQ                                                   
         MVI   ADVSYS,C'P'                                                      
         MVC   ADVMED,QMED                                                      
         MVC   ADVAOR,CLAAOR                                                    
         MVC   ADVADV,CLAADV                                                    
         MVC   ADVAGY,CLAAOR                                                    
         L     RE,AIO3                                                          
         MVC   0(32,RE),CTKEY                                                   
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),=CL8'GENDIR',CTKEY,CTKEY           
         L     RE,AIO3                                                          
         CLC   CTKEY(32),0(R3)                                                  
         BNE   FSWT10              NOT FOUND SWITCH BACK                        
*                                                                               
         L     RF,AIO3                                                          
         GOTO1 DATAMGR,DMCB,(0,=CL8'GETREC'),=CL8'GENFIL',CTKEY+36,(RF)         
         L     R6,AIO3                                                          
         LA    R6,42(R6)           BUMP TO FIRST ELEM                           
         MVI   ELCODE,X'10'        LOOK FOR BUYING AGENCY ELEMENT               
         CLI   0(R6),X'10'                                                      
         BE    FCON100                                                          
         BRAS  RE,GETEL                                                         
         BNE   FSWT10                                                           
         USING AGYD,R6                                                          
FCON100  MVC   SVAORSE,AGYPRN                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,CTKEY                                                         
         XC    CTKEY,CTKEY                                                      
         USING ADVREC,R6                                                        
         MVI   ADVREC,ADVRECQ                                                   
         MVI   ADVTYP,ADVTYPQ                                                   
         MVI   ADVSYS,C'P'                                                      
         MVC   ADVMED,QMED                                                      
         MVC   ADVAOR,CLAAOR                                                    
         MVC   ADVADV,CLAADV                                                    
         MVC   ADVAGY,AGENCY                                                    
         L     RE,AIO3                                                          
         MVC   0(32,RE),CTKEY                                                   
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),=CL8'GENDIR',CTKEY,CTKEY           
         L     RE,AIO3                                                          
         CLC   CTKEY(32),0(RE)                                                  
         BNE   FSWT10              NOT FOUND SWITCH BACK                        
*                                                                               
         L     RF,AIO3                                                          
         GOTO1 DATAMGR,DMCB,(0,=CL8'GETREC'),=CL8'GENFIL',CTKEY+36,(RF)         
         L     R6,AIO3                                                          
         LA    R6,42(R6)           BUMP TO FIRST ELEM                           
         MVI   ELCODE,X'10'       LOOK FOR BUYING AGENCY ELEMENT                
         CLI   0(R6),X'10'                                                      
         BE    FSWT09                                                           
         BRAS  RE,GETEL                                                         
         BNE   FSWT10                                                           
*                                                                               
         USING AGYD,R6                                                          
FSWT09   LA    R3,SVAORSE                                                       
         MVI   WORK,1              FLAG TO RETURN FILE NUM                      
         BRAS  RE,SEFORM                                                        
         MVC   CLAASE2,WORK                                                     
         OI    CLAASE2H+6,X'80'                                                 
*                                                                               
         MVC   CLAPLR2(1),AGYCNTL                                               
         OI    CLAPLR2H+5,X'80'                                                 
         MVC   CLACRL2(1),AGYCNTL+1                                             
         OI    CLACRL2H+5,X'80'                                                 
         MVC   CLACLL2(1),AGYCNTL+2                                             
         OI    CLACLL2H+5,X'80'                                                 
         MVC   CLACLB2(1),AGYCNTL+3                                             
         OI    CLACLB2H+5,X'80'                                                 
         MVC   CLAASC2(1),AGYCNTL+4                                             
         OI    CLAASC2H+5,X'80'                                                 
         MVC   CLANAC2(1),AGYCNTL+5                                             
         OI    CLANAC2H+5,X'80'                                                 
         MVC   CLAACL2(1),AGYCNTL+6                                             
         OI    CLAACL2H+5,X'80'                                                 
         MVC   CLAAID2(1),AGYCNTL+7                                             
         OI    CLAAID2H+5,X'80'                                                 
         MVC   CLAACA2(1),AGYCNTL+8                                             
         OI    CLAACA2H+5,X'80'                                                 
*                                                                               
FSWT10   GOTO1 SWITCH,DMCB,=C'PRINT',0                                          
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                CANNOT SWITCH BACK!                          
         DROP  R6                                                               
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISP_KEY NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         L     R6,AIO                                                           
         CLI   3(R6),X'02'         CLIENT RECORD CODE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PCLTKEY,R6                                                       
         MVC   CLAMED,PCLTKMED                                                  
         MVC   CLACLT,PCLTKCLT                                                  
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    CLAMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    CLAMEDN,CLAMEDN                                                  
         OI    CLAMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   CLAMEDH+5,1         INPUT LENGTH                                 
         LA    R2,CLAMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   CLAMEDN,MEDNM                                                    
         OI    CLAMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    CLACLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    CLACLTN,CLACLTN                                                  
         OI    CLACLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   CLACLTH+5,3         INPUT LENGTH                                 
         LA    R2,CLACLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   CLACLTN,CLTNM                                                    
         OI    CLACLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREQRC NTR1  BASE=*,LABEL=*      PUT A REQUEST CARD FOR T/A REPORT            
*                                                                               
         XC    QCTL,QCTL                                                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA(2),=C'41'                                                  
         MVC   QAREA+2(2),AGENCY                                                
         MVC   QAREA+4(1),QMED                                                  
         MVC   QAREA+5(3),QCLT                                                  
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
GET_ITXT ST    RE,SAVERE                                                        
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SEFORM   NTR1  BASE=*,LABEL=*      PRINT FILE & SE NUM                          
*                                                                               
         CLI   WORK,2              RETURN SE NUMBER?                            
         BE    SEF40                                                            
         CLI   WORK,1              RETURN SYSTEM LETTER?                        
         BE    SEF20                                                            
         DC    H'0'                                                             
*                                                                               
SEF20    MVC   WORK,SPACES                                                      
         MVC   PRTSE#,0(R3)                                                     
         GOTOR DATAMGR,DMCB,(0,DDNAME),PRTSE,0                                  
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    SEF_X                                                            
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    SEF_X                                                            
         USING DDNAMED,RE                                                       
         MVC   WORK(2),DDNASEID    RETURN SYSTEM LETTER                         
         DROP  RE                                                               
*                                                                               
         J     SEF_X                                                            
*                                                                               
SEF40    MVC   WORK,SPACES                                                      
         MVC   PRTSYSC,0(R3)                                                    
         OC    PRTSYSC,SPACES                                                   
         GOTOR DATAMGR,DMCB,(0,DDNAME),PRTSYS,0                                 
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    SEF_X                                                            
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    SEF_X                                                            
         USING DDNAMED,RE                                                       
         MVC   WORK(1),DDNASENO    RETURN SE NUMBER                             
         DROP  RE                                                               
*                                                                               
SEF_X    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
DDNAME   DC    CL8'DDNAME'                                                      
PRTSE    DC    C'SE=',X'0000'                                                   
         ORG   PRTSE+3                                                          
         DS    X                                                                
PRTSE#   DS    X                                                                
PRTOV#   DS    X                                                                
*                                                                               
PRTSYS   DC    C'S=P  '                                                         
         ORG   PRTSYS+3                                                         
PRTSYSC  DS    CL2                                                              
*                                                                               
OUTSE#   DS    X                                                                
OUTOV#   DS    X                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMAFD          CLIADV MAINT SCREEN                          
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC4D          CLIENT LIST SCREEN                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO     DS    F                                                                
SVERRFLD DS    F                   ADDRESS OF ERROR FIELD FOR CURSOR            
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
SVAORSE  DS    CL1                 AOR SE NUMBER                                
WKKEY    DS    XL(L'KEY)                                                        
WKSWTICH DS    X                                                                
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
CTKEY    DS    CL50                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PCLTREC           DSECT CLIENT RECORD                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENADVD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
*                                                                               
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PRSFM2E   12/17/13'                                      
         END                                                                    
