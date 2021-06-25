*          DATA SET SPSFM4C    AT LEVEL 039 AS OF 05/20/10                      
*PHASE T2174CA                                                                  
*INCLUDE HEXOUT                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2174C  -- BUYER RECORD MAINT/LIST                   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SUPERVISOR RECORDS ON SPFILE               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMF8, AND SPSFME8                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED BUYER RECORDS                                *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2174C - BUYER RECORD MAINTENANCE'                              
T2174C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**174C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         USING GETTXTD,GETTXTCB                                                 
         ST    R3,RELO                                                          
*                                                                               
         CLI   ACTEQU,ACTREP                                                    
         BE    *+8                                                              
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECREST        RESTORE RECORD                               
         BNE   *+12                                                             
         BRAS  RE,RESTORE                                                       
         B     XIT                                                              
         CLI   MODE,RECPUT         WRITE BYR REC BACK TO SPTFIL?                
         BE    RP                  YES, MAKE SURE WE HAVE RIGHT D/A             
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,XRECADD                                                     
         BE    XR                  PASSIVE KEYS ON ADD                          
         CLI   MODE,XRECPUT                                                     
         BE    XR                  PASSIVE KEYS ON CHANGE                       
         CLI   MODE,XRECDEL                                                     
         BE    XR                  PASSIVE KEYS ON DELETE                       
         CLI   MODE,XRECREST                                                    
         BE    XR                  PASSIVE KEYS ON RESTORE                      
         CLI   MODE,PRINTREP                                                    
         BNE   SKIPPR              PASSIVE KEYS ON RESTORE                      
         CLI   ACTEQU,ACTLIST                                                   
         BE    LR                                                               
         BRAS  RE,PR                                                            
*                                                                               
SKIPPR   DS    0H                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* RECPUT                                                                        
*                                                                               
RP       DS    0H                                                               
         L     R6,AIO1                                                          
         CLC   0(2,R6),=X'0D62'         BUYER REC MUST BE IN AIO1               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SPVKEY),0(R6)                                              
         MVC   AIO,AIO2                 DON'T BLOW AWAY AIO1                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                   GET D/A OF REC FOR PUTREC               
         MVC   AIO,AIO1                 RESTORE CORRECT AIO FOR PUTREC          
RPX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
*                                                                               
         MVI   FLTFLAG,0                                                        
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   OFFICE,SPACES                                                    
         MVC   BUYR,SPACES                                                      
         XC    SAVESEL,SAVESEL                                                  
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SECURITY ALPHA                           
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
*                                                                               
         XC    WORK,WORK           GET AGY LEVEL SD PROF                        
         MVC   WORK(4),=C'S0SD'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,(X'C0',WORK),SDPROF,DATAMGR                         
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK02                                                             
         CLC   =C'SOON',CONACT                                                  
         BE    ERRREP                                                           
         B     VK05                SKIP VALIDATION FOR ACTION REPORT            
*                                                                               
VK02     DS    0H                                                               
         CLI   ACTEQU,ACTREP                                                    
         BNE   VK05                                                             
*                                                                               
         LA    R2,BRLMKTH                                                       
         LA    R1,8(R2)                                                         
         LLC   RE,5(R2)            LEN                                          
*                                                                               
         CLI   BRLMKTH+5,0                                                      
         BE    VK04                                                             
         CLI   BRLMKT,C'0'         MARKET OR MARKET GROUP?                      
         BNL   VK03                MAKE SURE MARKET NUMERIC                     
         CLI   BRLMKTH+5,2                                                      
         BL    ERRINV              MARKET GROUP MUST BE >=2 CHARS               
         CLI   0(R1),C'A'                                                       
         BL    ERRINV              INVALID MKT GROUP                            
         CLI   0(R1),C'Y'          ID CAN NOT BE Y,Z                            
         BNL   ERRINV              INVALID MKT GROUP                            
*                                                                               
         LA    R1,1(R1)            AND POINT TO MKT GRP ID                      
         AHI   RE,-1               MINUS 1 FOR MINUS SIGN = LEN                 
*                                                                               
VK03     CLI   0(R1),C'0'          TESTS IF MARKET NUMERIC                      
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
         LA    R1,1(R1)            CHECK NEXT CHARACTER                         
         BCT   RE,VK03             CHECKS EACH INPUT CHARACTER                  
VK04     DS    0H                                                               
*                                                                               
         B     VKXX                                                             
*                                                                               
VK05     DS    0H                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK50                                                             
*                                                                               
         LA    R2,BYLOFFH                                                       
         CLI   5(R2),0             DATA IN OFFICE FIELD?                        
         BE    VK10                NO SKIP                                      
         MVC   OFFICE,BYLOFF                                                    
         OC    OFFICE,SPACES                                                    
*                                                                               
VK10     LA    R2,BYLBYCH                                                       
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         MVC   BUYR,BYLBYC                                                      
         OC    BUYR,SPACES                                                      
         B     VK100                                                            
*                                                                               
VK50     DS    0H             VALIDATE FOR DISPLAY/ADD/CHANGE                   
         LA    R2,BYMOFFH               VALIDATE OFFICE CODE                    
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C' '                                                       
         BNH   ERRINV                                                           
         MVC   OFFICE,BYMOFF                                                    
         OC    OFFICE,SPACES                                                    
         BAS   RE,VALOFF                                                        
*                                                                               
         LA    R2,BYMBYCH               VALIDATE BUYER CODE                     
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C' '                                                       
         BNH   ERRINV                                                           
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK59                                                             
         LA    R1,BYMBYC           VALID ALPHA NUMERIC                          
         LA    R0,L'BYMBYC                                                      
VK54     CLI   0(R1),C' '                                                       
         BNH   VK56                                                             
         CLI   0(R1),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
VK56     LA    R1,1(R1)                                                         
         BCT   R0,VK54                                                          
*                                                                               
VK59     MVC   BUYR,BYMBYC                                                      
         OC    BUYR,SPACES                                                      
*                                                                               
         CLI   5(R2),2                  BUYER CODE LEN. AT LEAST 2              
         BL    ERRINV                                                           
         B     VK100                                                            
*                                                                               
VK100    XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD BUYER KEY                              
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,AGYX                                                     
         MVC   BYRKOFC,OFFICE                                                   
         MVC   BYRKBYR,BUYR                                                     
*                                                                               
         CLC   SAVEKEY,KEY                                                      
         BE    *+8                                                              
         OI    STATUS,KEYCHG                                                    
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      MVC   AIO,AIO1                 MUST RESTORE PROPER IO AREA             
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   XIT                                                              
*                                                                               
         MVI   FLTFLAG,0                                                        
         XC    BGFILTER,BGFILTER                                                
         LA    R2,BYLFILH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),SCANAREA                                       
         LLC   R3,DMCB+4           NUMBER OF FILTERS                            
         CHI   R3,MAXFLTQ                                                       
         BH    ERRINV                                                           
         LA    R6,SCANAREA                                                      
*                                                                               
VKX10    DS    0H                                                               
         CLC   =C'BG',12(R6)            BUY GROUP?                              
         BNE   VKX20                                                            
*                                                                               
         CLI   0(R6),2                                                          
         BNE   ERRINV                                                           
         CLI   1(R6),0                                                          
         BE    ERRINV                                                           
         CLI   1(R6),2                  BUYGROUPS ARE 1 OR 2 CHARS              
         BH    ERRINV                                                           
*                                                                               
         MVC   SVKEY1,KEY               CHECK TO SEE IF BUYGRP VALID            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D60'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'                                                      
         MVC   KEY+3(2),22(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(5),KEY                                                   
         BNE   ERRINV                                                           
         MVC   BGFILTER,22(R6)                                                  
         OI    FLTFLAG,BGFLTQ                                                   
         MVC   KEY,SVKEY1                                                       
         B     VKX60                                                            
*                                                                               
VKX20    DS    0H                                                               
         CLC   =C'MKT',12(R6)          MARKET?                                  
         BNE   VKX40                                                            
*                                                                               
         CLI   0(R6),3                                                          
         BNE   ERRINV                                                           
         CLI   1(R6),0                                                          
         BE    ERRINV                                                           
         CLI   1(R6),4                                                          
         BH    ERRINV                                                           
*                                                                               
         LLC   R1,1(R6)            LENGTH OF MARKET                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DWORD,22(0,R6)   PACK THE MARKET NUMBER                          
* IF MARKET CONTAINS CHARACTERS PACK WILL DIE                                   
         CVB   R1,DWORD                                                         
         STCM  R1,3,MKFILTER                                                    
         OI    FLTFLAG,MKFLTQ                                                   
         B     VKX60                                                            
*                                                                               
VKX40    DS    0H                                                               
         CLC   =C'SPV',12(R6)          SUPERVISOR?                              
         BNE   ERRINV                                                           
*                                                                               
         CLI   0(R6),3                                                          
         BNE   ERRINV                                                           
         CLI   1(R6),2                                                          
         BL    ERRINV                                                           
         CLI   1(R6),4                  ALL SUPERVISORS ARE 2-4 LETTERS         
         BH    ERRINV                                                           
         MVC   SPFILTER,22(R6)                                                  
         OI    FLTFLAG,SPFLTQ                                                   
*                                                                               
VKX60    DS    0H                                                               
         LA    R6,32(R6)           POINT TO NEXT SCAN LINE                      
         BCT   R3,VKX10            PROCESS NEXT SCAN LINE                       
*                                                                               
VKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         XC    ADDMKTS(ADMKLNEQ),ADDMKTS                                        
         XC    DELMKTS(DEMKLNEQ),DELMKTS                                        
         XC    ADDMGS(ADMGLNEQ),ADDMGS                                          
         XC    DELMGS(DEMGLNEQ),DELMGS                                          
*                                                                               
**********************************************************************          
*        VALIDATE X'01' GENERAL ELEMNT                               *          
*                                                                    *          
         CLI   ACTEQU,ACTADD                                                    
         BE    VR10                                                             
         MVI   ELCODE,X'01'        BUYER NAME ELEMENT                           
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         USING BYRNAMED,R6                                                      
         MVC   OLDSPV,BYRSPV                                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 REMELEM                                                          
*                                                                               
VR10     LA    R5,ELEM                                                          
         USING BYRNAMED,R5                                                      
         XC    ELEM,ELEM                                                        
         MVI   BYRNAMEL,X'01'      ELEMENT CODE                                 
         MVI   BYRNAMLN,BYRNAMLQ   ELEMENT LENGTH                               
*                                                                               
         LA    R2,BYMFNMH          CHECK BUYER NAME FIELD                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   BYRFNAME,BYMFNM                                                  
         OC    BYRFNAME,SPACES                                                  
*                                                                               
         LA    R2,BYMLNMH          CHECK BUYER NAME FIELD                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   BYRLNAME,BYMLNM                                                  
         OC    BYRLNAME,SPACES                                                  
*                                                                               
         LA    R2,BYMSUPH               CHECK SUPV CODE                         
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         BAS   RE,VALSUPV                                                       
*                                                                               
         MVC   BYRSPV,BYMSUP                                                    
         OC    BYRSPV,SPACES                                                    
*                                                                               
         LA    R2,BYMNWSTH               CHECK TV NWS CODE                      
         MVC   BAGYMDS,AGYX             BINARY AGENCY/TV MEDIA CODE             
         OI    BAGYMDS,X'1'             TURN ON TV BIT                          
         CLI   5(R2),0                                                          
         BE    VR12                     NOT REQUIRED                            
         MVC   SAVENWS,BYMNWST                                                  
         OC    SAVENWS,SPACES                                                   
         BAS   RE,VALNWS                                                        
         MVC   BYRNWST,SAVENWS                                                  
*                                                                               
VR12     LA    R2,BYMDARTH               CHECK TV DARE CODE                     
         CLI   5(R2),0                                                          
         BE    VR14                                                             
         MVC   SAVEDAR,BYMDART                                                  
         OC    SAVEDAR,SPACES                                                   
         BAS   RE,VALDARE                                                       
         MVC   BYRDARET,SAVEDAR                                                 
*                                                                               
VR14     LA    R2,BYMNWSRH               CHECK RADIO NWS CODE                   
         MVC   BAGYMDS,AGYX             BINARY AGENCY/RADIO MED CODE            
         OI    BAGYMDS,X'2'             TURN ON RADIO BIT                       
         CLI   5(R2),0                                                          
         BE    VR16                                                             
         MVC   SAVENWS,BYMNWSR                                                  
         OC    SAVENWS,SPACES                                                   
         BAS   RE,VALNWS                                                        
         MVC   BYRNWSR,SAVENWS                                                  
*                                                                               
VR16     LA    R2,BYMDARRH               CHECK RADIO DARE CODE                  
         CLI   5(R2),0                                                          
         BE    VR22                                                             
         MVC   SAVEDAR,BYMDARR                                                  
         OC    SAVEDAR,SPACES                                                   
         BAS   RE,VALDARE                                                       
         MVC   BYRDARER,SAVEDAR                                                 
*                                                                               
VR22     LA    R2,BYMOFOFH                                                      
         MVC   BYROFF,SPACES                                                    
         CLI   5(R2),0                                                          
         BNE   VR24                                                             
         CLI   SDPROF+9,C'Y'       BUYER OFFICE REQ'D                           
         BE    ERROFFR             OFFICE REQUIRED                              
         B     VR28                                                             
VR24     MVC   BYROFF,BYMOFOF                                                   
*                                                                               
VR28     LA    R2,BYMFILTH                                                      
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         CLI   BYMFILT,C'A'                                                     
         BL    ERRINV                                                           
         CLI   BYMFILT,C'Z'                                                     
         BH    ERRINV                                                           
         MVC   BYRFILT,BYMFILT                                                  
         DROP  R5                                                               
*                                                                               
**********************************************************************          
*        VALIDATE OPTIONS.  11/16/99                                 *          
*                                                                    *          
VR30     LA    R2,BYMOPTH                                                       
         MVI   DAYS,0                                                           
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BRAS  RE,VOPT                                                          
         GOTO1 ADDELEM                                                          
**********************************************************************          
*        VALIDATE PERSONAL ID                                        *          
*                                                                    *          
         LA    R2,BYMPIDH                                                       
         CLI   5(R2),0             REQ'D                                        
         BE    ERRMIS                                                           
         MVC   PIDNAME,BYMPID                                                   
         OC    PIDNAME,SPACES                                                   
         BAS   RE,VALPID                                                        
         BNE   ERRINV                                                           
*                                                                               
         MVI   ELCODE,BPIDELQ      X'22'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         USING BPIDELD,R5                                                       
         LA    R5,ELEM                                                          
         MVI   BPIDEL,BPIDELQ      X'22'                                        
         MVI   BPIDLN,BPIDLNQ                                                   
         MVC   BPIDNO,PIDNUM                                                    
         MVC   BPIDDAYS,DAYS                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
**********************************************************************          
         LA    R2,BYMMEDH               FINDING INPUT MEDIA AND ELCODE          
         CLI   5(R2),0                                                          
         BE    VR35                                                             
*                                                                               
         MVC   BYTE,BYMMED                                                      
         LHI   R0,MEDLTRQ                                                       
         BRAS  RE,GETMED                                                        
*        BNE   ERRINV                                                           
         MVC   MEDELCD,1(RF)                                                    
         MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         GOTO1 VALIMED                  MAKES A DM CALL                         
         MVC   AIO,AIO1                 MUST RESTORE PROPER IO AREA             
         B     VR40                                                             
*                                                                               
**** CHECK ADD MKTS/MKT GRPS FIELDS                                             
*                                                                               
VR35     LA    R2,BYMFMKH               POINT TO FIRST MKT INPUT                
*                                                                               
         CLI   5(R2),0                  NO MEDIA INPUT, MKT INPUT?              
         BE    VR100                    NO, ADD THE RECORD                      
         LA    R2,BYMMEDH               YES, REPORT MISSING MEDIA               
         B     ERRMIS                                                           
*                                                                               
VR40     LA    R2,BYMFMKH               POINT TO FIRST MKT ENTRY                
         LA    R3,ADDMKTS               CLEAR STORED ADDED MKTS                 
         LA    R5,DELMKTS               CLEAR STORED DELETED MKTS               
*                                                                               
VR45     CLI   5(R2),0                  MEDIA INPUT, MARKET INPUT?              
         BE    VR100                    NO MARKET, JUST ADD REC                 
*                                                                               
         CLI   ACTEQU,ACTADD            ACTION ADD?                             
         BE    VR55                     YES, DON'T CHECK FOR MINUS SIGN         
*                                                                               
         CLI   8(R2),C'-'               NO, CHECK FOR MINUS                     
         BNE   VR55                                                             
*                                                                               
         LLC   RE,5(R2)                 GET LENGTH OF INPUT MKT                 
         BCTR  RE,0                     SUBTRACT ONE FOR THE MINUS              
         LA    R1,9(R2)                 POINT TO START OF MARKET INPUT          
         CLC   =C'ALL',9(R2)                                                    
         BNE   VR50                                                             
         BAS   RE,DELALL                                                        
         B     VR100                                                            
*                                                                               
VR50     CLI   0(R1),C'0'               TESTS IF MARKET OR MKT GROUP            
         BL    VRMG10                   MAY BE MKT GROUP                        
VR52     CLI   0(R1),C'0'               TESTS IF MARKET NUMERIC                 
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
         LA    R1,1(R1)                 CHECK NEXT CHARACTER                    
         BCT   RE,VR52                  CHECKS EACH INPUT CHARACTER             
*                                                                               
         LLC   RE,5(R2)                 RESET INPUT MKT LEN FOR PACK            
         BCTR  RE,0                     SUBTRACT ONE FOR THE MINUS              
         LA    R1,9(R2)                 POINT TO START OF VALID INPUT           
         B     VR60                     GOTO EXECUTED PACK                      
*                                                                               
VR55     DS    0H                                                               
         TM    4(R2),X'08'              ADDED MKT VALID NUMERIC?                
         BO    VR57                                                             
         CLC   =C'ALL',8(R2)            WE'R ALLOWING ALL AS INPUT              
         BNE   VRMG10                   CHECK IF MKT GROUP                      
         BAS   RE,ADDALL                                                        
         B     VR100                                                            
*                                                                               
VR57     LLC   RE,5(R2)                 LENGTH OF INPUT FOR EX PACK             
         LA    R1,8(R2)                 POINT TO START OF VALID INPUT           
*                                                                               
VR60     BCTR  RE,0                     EXECUTED PACK FOR DELETE & ADD          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT2               BINARY MKT FIELD FOR ADD/DEL            
*                                                                               
         L     R6,AIO             CHK IF REC HAS ANY MKT GR ELEMS               
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    ERRNOMIX            CAN'T ADD/DEL MKT IF REC HAS MKT GR          
*                                                                               
         CLI   8(R2),C'-'               MINUS SIGN?                             
         BNE   VR80                     NO, GOTO ADD MKT LOGIC                  
*                                                                               
         LA    R1,ADDMKTS               YES, DELETE MKT LOGIC                   
         LA    R0,ADMKEND                                                       
VR70     DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   BMKT2,0(R1)            WON'T LET YOU DELETE A CLIENT             
         BE    ERRDUP                   IF YOU ADDED IT IN A PREVIOUS           
         LA    R1,L'ADDMKTS(R1)         "ADD CLIENT" FIELD                      
         CR    R1,R0                                                            
         BNE   VR70                                                             
*                                                                               
         MVC   0(L'BMKT2,R5),BMKT2      ENTRY OK, STORE IN DEL TABLE            
         BAS   RE,DELMKT                                                        
         LA    R5,L'DELMKTS(R5)         PT. TO NEXT AVAIL. DEL STORE            
         B     VR90                                                             
*                                                                               
VR80     LA    R1,DELMKTS               ADD MKT LOGIC                           
         LA    R0,DEMKEND                                                       
VR85     DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   BMKT2,0(R1)            WON'T LET YOU ADD A CLIENT                
         BE    ERRDUP                   IF YOU DELETED IT IN A PREVIOUS         
         LA    R1,L'DELMKTS(R1)         "ADD CLIENT" FIELD                      
         CR    R1,R0                                                            
         BNE   VR85                                                             
*                                                                               
         MVC   0(L'BMKT2,R3),BMKT2                                              
         BAS   RE,ADDMKT                                                        
         LA    R3,L'ADDMKTS(R3)         BUMP THROUGH STORAGE TABLES             
*                                                                               
VR90     BAS   RE,NXTSCRF               PT. TO NEXT "ADD MARKETS" ENTRY         
         LA    R0,BYMLMKH                                                       
         CR    R2,R0                    PAST LAST MKT FIELD?                    
         BNH   VR45                     NO, CHECK NEXT FIELD                    
         B     VR100                                                            
*                                                                               
VRMG10   BRAS  RE,VRMG             CKECK MKT GROUP                              
VR100    LA    R2,BYMBYRH          VALIDATE BUYER/BILLER NAME                   
         BRAS  RE,ADDBUBL                                                       
         B     VRX                                                              
*                                                                               
VRX      OI    GENSTAT2,RETEQSEL   STAY HERE FOR ONE TRANSACTION                
         B     DR                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'01'        DISPLAY NAME ELEMENT                         
         BRAS  RE,GETEL                                                         
*                                                                               
         USING BYRNAMED,R6                                                      
         MVC   BYMFNM,BYRFNAME      DISPLAY BUYER NAME                          
         OI    BYMFNMH+6,X'80'                                                  
         MVC   BYMLNM,BYRLNAME      DISPLAY BUYER NAME                          
         OI    BYMLNMH+6,X'80'                                                  
*                                                                               
         MVC   SUPV,BYRSPV       DISPLAY SUPV CODE                              
         OC    SUPV,SPACES                                                      
         MVC   BYMSUP,BYRSPV       DISPLAY SUPV CODE                            
         OI    BYMSUPH+6,X'80'                                                  
*                                                                               
         BAS   RE,GETSPN                GET SUPERVISOR NAME                     
         MVC   BYMSPN,BLOCK                                                     
         OI    BYMSPNH+6,X'80'                                                  
*                                                                               
         XC    BYMNWST,BYMNWST          CLEAR TV NWS CODE                       
         MVI   BYMNWSTH+5,0                                                     
         OI    BYMNWSTH+6,X'80'                                                 
*                                                                               
         OC    BYRNWST,BYRNWST                                                  
         BZ    *+14                                                             
         MVC   BYMNWST,BYRNWST      DISPLAY TV NWS CODE                         
         OI    BYMNWSTH+6,X'80'                                                 
*                                                                               
         XC    BYMDART,BYMDART          CLEAR TV DARE CODE                      
         MVI   BYMDARTH+5,0                                                     
         OI    BYMDARTH+6,X'80'                                                 
*                                                                               
         OC    BYRDARET,BYRDARET                                                
         BZ    *+14                                                             
         MVC   BYMDART,BYRDARET     DISPLAY TV DARE CODE                        
         OI    BYMDARTH+6,X'80'                                                 
*                                                                               
         XC    BYMNWSR,BYMNWSR          CLEAR RADIO NWS CODE                    
         MVI   BYMNWSRH+5,0                                                     
         OI    BYMNWSRH+6,X'80'                                                 
*                                                                               
         OC    BYRNWSR,BYRNWSR                                                  
         BZ    *+14                                                             
         MVC   BYMNWSR,BYRNWSR      DISPLAY RADIO NWS CODE                      
         OI    BYMNWSRH+6,X'80'                                                 
*                                                                               
         XC    BYMDARR,BYMDARR          CLEAR RADIO DARE CODE                   
         MVI   BYMDARRH+5,0                                                     
         OI    BYMDARRH+6,X'80'                                                 
*                                                                               
         OC    BYRDARER,BYRDARER                                                
         BZ    *+14                                                             
         MVC   BYMDARR,BYRDARER         DISPLAY RADIO DARE CODE                 
         OI    BYMDARRH+6,X'80'                                                 
*                                                                               
         BRAS  RE,CLRSCR                                                        
         MVC   BYMSTRT,MKHEAD           DISPLAY MARKET HEADER                   
         OI    BYMSTRTH+6,X'80'                                                 
*                                                                               
         MVC   BYMOFOF,BYROFF           DISPLAY BUYER OFFICE                    
         OI    BYMOFOFH+6,X'80'                                                 
*                                                                               
         MVC   BYMFILT,BYRFILT         DISPLAY FILTER                           
         OI    BYMFILTH+6,X'80'                                                 
*                                                                               
***                                                                             
*        DISPLAY OPTIONS: CABLE=Y/N UNWIRED=Y/N ASST=Y/N DAYS=1-90              
***                                                                             
         LA    R2,BYMOPT                                                        
         MVC   BYMOPT,SPACES                                                    
         OI    BYMOPTH+6,X'80'           CLEAR!                                 
*                                                                               
         TM    BYROPT1,BYROPT1_CBL                                              
         BNO   DR03                                                             
         MVC   0(8,R2),=C'CABLE=Y,'                                             
         LA    R2,8(R2)                                                         
*                                                                               
DR03     TM    BYROPT1,BYROPT1_UNW                                              
         BNO   DR04                                                             
         MVC   0(10,R2),=C'UNWIRED=Y,'                                          
         LA    R2,10(R2)                                                        
*                                                                               
DR04     TM    BYROPT1,BYROPT1_ASS                                              
         BNO   DR05                                                             
         MVC   0(7,R2),=C'ASST=Y,'                                              
         LA    R2,7(R2)                                                         
*                                                                               
DR05     L     R6,AIO                                                           
         MVI   ELCODE,X'22'        PERSON ID ELEMENT                            
         BRAS  RE,GETEL            HAVE ONE?                                    
         BNE   DR09                NO                                           
         USING BPIDELD,R6                                                       
         CLI   BPIDDAYS,0          HAVE A DAY FILTER?                           
         BE    DR09                NO                                           
         MVC   0(2,R2),=C'D='                                                   
         EDIT  BPIDDAYS,(2,2(R2)),ALIGN=LEFT                                    
         LA    R2,4(R2)                                                         
         DROP  R6                                                               
*                                                                               
DR09     AHI   R2,-1               BACK UP 1                                    
         CLI   0(R2),C','          AND CHECK FOR COMMA                          
         BNE   DR10                                                             
         MVI   0(R2),C' '          SPACE OUT LAST COMMA                         
*                                                                               
*        DISPLAY PID NAME                                                       
*                                                                               
DR10     MVC   BYMPID,SPACES                                                    
         OI    BYMPIDH+6,X'80'                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'             PID                                     
         BRAS  RE,GETEL                                                         
         BNE   DR11                                                             
         USING BPIDELD,R6                                                       
         MVC   PIDNUM,BPIDNO                                                    
         BAS   RE,GETPIDNM                                                      
         MVC   BYMPID,PIDNAME                                                   
*                                                                               
DR11     LA    R2,BYMMEDH                                                       
         CLI   ACTEQU,ACTDIS            ACTION CHANGE?                          
         BNE   DR12                     YES, DISPLAY FROM FIRST MARKET          
         TM    4(R2),X'80'              WAS MEDIA INPUT THIS TIME?              
         BNO   DR15                     NO, START DISPLAY FROM LAST POS         
DR12     XC    STARTMKT,STARTMKT        YES, DISPLAY FROM FIRST MARKET          
         XC    LASTMKT,LASTMKT                                                  
         XC    FRSTMKT,FRSTMKT                                                  
         XC    STARTMKG,STARTMKG        YES, DISP FROM FIRST MKT GROUP          
         XC    LASTMKG,LASTMKG                                                  
         XC    FRSTMKG,FRSTMKG                                                  
*                                                                               
DR15     CLI   ACTEQU,ACTSEL       LIST/SELECT?                                 
         BE    DR16                                                             
         CLI   5(R2),0                                                          
         BNE   DR20                                                             
         MVI   8(R2),C'T'          DEFAULT = TELEVISION                         
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         B     DR20                                                             
*                                                                               
DR16     CLI   5(R2),0             ANYTHING THERE                               
         BNE   DR20                THEN KEEP IT                                 
         LA    R1,MEDIATBL                                                      
         LLC   RE,SELLISTN         THE SEQUENCE # OF THE SELECTION              
         AR    R1,RE                                                            
         CLI   0(R1),X'40'         SPACE?                                       
         BNE   *+12                                                             
         MVI   8(R2),C'T'                                                       
         B     *+10                                                             
         MVC   8(1,R2),0(R1)                                                    
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR20     DS    0H                                                               
         MVC   BYTE,BYMMED                                                      
         LHI   R0,MEDLTRQ                                                       
         BRAS  RE,GETMED                                                        
         MVC   MEDELCD,1(RF)                                                    
*                                                                               
**** MAKE VALIMED CALL SO CAN MAKE VALIMKT CALL ****                            
         MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         GOTO1 VALIMED                  MAKES A DM CALL                         
         MVC   AIO,AIO1            RESET                                        
*                                                                               
         L     R6,AIO              CHK IF REC HAS ANY MKT GR ELEMS              
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR40                NO MKT GROUPS - GO LIST MARKETS              
         BAS   RE,MKGLS            LIST MARKET GROUPS                           
         B     DR50                                                             
*                                                                               
DR40     BRAS  RE,MKLS             LIST MARKETS                                 
*                                                                               
DR50     BRAS  RE,DISPBUBL         DISPLAY BUYER/BILLER NAME                    
*                                                                               
DR100    CLI   ACTEQU,ACTSEL                                                    
         BNE   DR100A                                                           
         CLI   SAVESEL,C'C'                                                     
         BE    DR101                                                            
         CLI   SAVESEL,C'D'                                                     
         BE    DR101                                                            
DR100A   CLI   ACTEQU,ACTCHA                                                    
         BNE   DRX                                                              
DR101    MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         NI    STATUS,X'FF'-KEYCHG                                              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING BYRRECD,R6                                                       
*                                                                               
         MVC   OFFICE,BYRKOFC                                                   
         OC    OFFICE,SPACES                                                    
         MVC   BYMOFF,OFFICE                                                    
         OI    BYMOFFH+6,X'80'                                                  
*                                                                               
         MVC   BUYR,BYRKBYR                                                     
         OC    BUYR,SPACES                                                      
         MVC   BYMBYC,BUYR                                                      
         OI    BYMBYCH+6,X'80'                                                  
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      CLI   ACTEQU,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SAVESEL,THISLSEL                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*ONLINE LIST                                                                    
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING BYRRECD,R6                                                       
         XC    SVLISTAR,SVLISTAR   LISTAR'S COMPARED WITH PREV LINE             
*                                  TO MAKE SURE NO MULTI INSTANCES              
         OC    KEY,KEY             IS THIS FIRST TIME AT LIST SCREEN?           
         BNZ   LR10                NO, DO NOT BUILD KEY                         
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         B     LR15                    FIRST TIME, DON'T USE SAVEKEY3           
*                                                                               
LR10     DS    0H                                                               
*                                                                               
         CLI   DISPED,C'Y'                                                      
         BNE   LR15                                                             
         MVC   KEY,SAVEKEY3                                                     
*                                                                               
LR15     DS    0H                                                               
         GOTO1 HIGH               FIND FIRST DIRECTORY REC                      
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                FIND SUBSEQUENT DIRECTORY RECS                
*                                                                               
LR30     DS    0H                                                               
         CLC   KEY(3),SAVEKEY     IF PAST BUYER RECORDS...                      
         BNE   LRX                STOP READING RECORDS                          
         MVC   SAVEKEY3,KEY                                                     
*                                                                               
         GOTO1 GETREC             GET BUYER DATA                                
         MVI   DISPED,C'N'         NOTHING FROM THIS REC DISPLAYED              
         MVI   ELCODE,X'02'       LOOKING FOR TV MARKETS 1ST                    
         MVC   SVELCD,ELCODE                                                    
*                                                                               
         MVC   LISTAR,SPACES      CLEAR PRINT LINE OF LIST ENTRIES              
*                                                                               
LR32     CLI   LISTNUM,0          TOP OF THE LIST                               
         BNE   LR35                                                             
         XC    MEDIATBL,MEDIATBL                                                
         LA    R1,MEDIATBL        TABLE LOOKS LIKE  |T|R|X|...                  
         ST    R1,SAVEPTR                                                       
*                                                                               
LR35     L     R6,AIO                                                           
         BRAS  RE,GETEL           DO WE HAVE THIS ELEMENT?                      
         BE    LR40               YES? PROCESS                                  
LR37     LLC   R1,SVELCD          NO?  LOOK FOR THE NEXT ONE                    
         LA    R1,1(R1)                                                         
         STCM  R1,1,ELCODE                                                      
         MVC   SVELCD,ELCODE      SAVE THE ELCODE AGAIN                         
         CLI   ELCODE,X'14'       ARE WE DONE                                   
         BNH   LR35               NO?  GO ON TO THE NEXT ELEMENT                
         CLI   DISPED,C'N'        ANYTHING FROM THIS REC DISPLAYED              
         BE    LR60                NO THEN DISPLAY - NO ELEMS                   
         B     LR20               YES? LIST THIS REC THEN SEQ                   
*                                                                               
LR40     MVC   BYTE,SVELCD        NEED TO TURN OFF HIGH NIBBLE                  
         NI    BYTE,X'0F'         TO DEAL WITH MKT GROUP ELEMS                  
*                                                                               
         CLI   BYTE,X'02'         T                                             
         BNE   *+8                                                              
         MVI   LSBYRMED,C'T'                                                    
         CLI   BYTE,X'03'         R                                             
         BNE   *+8                                                              
         MVI   LSBYRMED,C'R'                                                    
         CLI   BYTE,X'04'         X                                             
         BNE   *+8                                                              
         MVI   LSBYRMED,C'X'                                                    
*                                                                               
LR60     L     R6,AIO                                                           
         USING BYRRECD,R6                                                       
*                                                                               
* FILTERING TAKES PLACE HERE                                                    
*                                                                               
         TM    FLTFLAG,BGFLTQ     IS THERE AN OFFICE FILTER?                    
         BNO   *+14                                                             
         CLC   BYRKOFC,BGFILTER                                                 
         BNE   LR20                                                             
*                                                                               
         TM    FLTFLAG,MKFLTQ     IS THERE A MARKET FILTER?                     
         BNO   LR70               NOPE                                          
         CLI   ELCODE,X'14'       PASSED ALL MKT/MGROUPS?                       
         BH    LR20               YES, THIS REC DID NO PASS MKT FILTER          
         BRAS  RE,FLTMKT          FOUND MKT/MGROUP THAT MATCHES FILTER?         
         BNE   LR37               NO, CHECK NEXT ELEMENT                        
*                                                                               
LR70     TM    FLTFLAG,SPFLTQ     IS THERE A SUPERVISOR FILTER?                 
         BNO   *+12                                                             
         BRAS  RE,FLTSUP         CHECK FOR SUPERVISOR                           
         BNE   LR20                                                             
*                                                                               
         MVC   LSOFFCD,BYRKOFC    PRINT BUYER CODE                              
         MVC   OFFICE,BYRKOFC     SAVE OFFICE CODE FOR SUPV REC                 
         MVC   LSBYRCD,BYRKBYR    PRINT BUYER CODE                              
*                                                                               
         MVI   ELCODE,X'01'       FIND BUYER ELEMENT                            
         BRAS  RE,GETEL                                                         
*                                                                               
         USING BYRNAMED,R6                                                      
         XC    BLOCK(50),BLOCK                                                  
         MVC   BLOCK(L'BYRLNAME),BYRLNAME                                       
         MVC   BLOCK+15(L'BYRFNAME),BYRFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'LSBYRNM)                             
         MVC   LSBYRNM,BLOCK                                                    
*                                                                               
         MVC   LSBYRFIL,BYRFILT                                                 
         MVC   LSBYRSPV,BYRSPV                                                  
         MVC   SUPV,BYRSPV                                                      
         OC    SUPV,SPACES                                                      
*        MVC   OFFICE,BYROFF                                                    
         MVC   SVKEY1,KEY                                                       
         BAS   RE,GETSPN                                                        
         MVC   LSBYRSNM,BLOCK                                                   
         MVC   KEY,SVKEY1                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         CLC   LISTAR,SVLISTAR      DON'T LIST IDENTICAL ITEM                   
         BE    LR37                                                             
         MVC   SVLISTAR,LISTAR      SAVE LISTAR FOR NEXT COMPARE                
         L     R1,SAVEPTR           BUMP THE TABLE                              
         MVC   0(1,R1),LSBYRMED     STORE THE MEDIA IN TABLE: T,R,X,' '         
         LA    R1,1(R1)                                                         
         ST    R1,SAVEPTR           SAVE THE POINTER TO NEXT ENTRY              
         MVI   DISPED,C'Y'        SOMETHING FROM THIS REC DISPLAYED             
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR200                                                            
         MVC   P,SPACES                                                         
         MVC   P(L'LISTAR),LISTAR                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR210                                                            
*                                                                               
LR200    GOTO1 LISTMON                                                          
LR210    B     LR37                                                             
*                                                                               
LRX      DS    0H                                                               
         XC    SAVEKEY3,SAVEKEY3                                                
         MVI   DISPED,C'N'                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ADD AND DELETE PASSIVE POINTERS                                               
*                                                                               
XR       DS    0H                                                               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         MVC   DISKADD,KEY+14                                                   
*                                                                               
         CLI   ACTEQU,ACTREST           ACTION RESTORE?                         
         BE    XR6                      YES, ADD BACK P-KEYS                    
         CLI   SAVESEL,C'D'                                                     
         BE    XR0                                                              
         CLI   ACTEQU,ACTDEL            ACTION DELETE?                          
         BNE   XR9                      NO, GOTO ROUTINE FOR ADD/CHANGE         
*                                                                               
* FOR XRECDELETE                                                                
         USING BYRRECD,R4               DELETE SUPV P-KEYS                      
XR0      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS -                
         MVC   BYRPAGY2,AGYX            - TO THE BUYER RECORD VIA -             
         MVC   BYRPOFC2,OFFICE          - SUPERVISOR                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR2                                                              
*                                                                               
XR1      GOTO1 SEQ                                                              
XR2      CLC   KEY(5),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XR3                   NO, CHECK MKT P-KEYS                       
*                                                                               
         CLC   BYRPBYR2,BUYR            YES, CORRECT BUYER?                     
         BNE   XR1                      NO, CHECK NEXT                          
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XR1                      CHECK NEXT RECORD                       
*                                                                               
XR3      XC    KEY,KEY                  DELETE MKT P-KEYS                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP,=X'0DE2'        SEARCHING FOR ALL MARKET -               
         MVC   BYRPAM,AGYX            - P-KEYS TO THE BUYER FOR -               
         MVC   BYRPOFC,OFFICE          - ALL MEDIA                              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR5                                                              
*                                                                               
XR4      GOTO1 SEQ                                                              
XR5      CLC   KEY(2),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XR5A                  NO, DONE                                   
*                                                                               
* CHECK FOR CORRECT MEDIA - C1,C2,C4 HERE?                                      
         CLC   BYRPOFC,OFFICE           RIGHT OFFICE                            
         BH    XR5A                     HIGHER, EXIT                            
         CLC   BYRPBYR,BUYR            CORRECT BUYER?                           
         BNE   XR4                     NO, CHECK NEXT                           
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XR4                      CHECK NEXT RECORD                       
*                                                                               
XR5A     XC    KEY,KEY                  DELETE BUBL P-KEYS                      
         LA    R4,KEY                                                           
         MVC   BYRPTYP3,=X'0DE4'                                                
         MVC   BYRPAGY3,AGYX                                                    
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,BYRBBLQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   XRX                                                              
         MVC   BYRPBUBL,BYRBBLNM-BYRBBLD(R6)                                    
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS3),KEYSAVE  PASSIVE POINTER FOUND?                  
         BNE   XRX                       NO, DONE                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XRX                                                              
*                                                                               
* FOR XRECRESTORE                                                               
XR6      XC    KEY,KEY                  ADD SUPV P-KEYS                         
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS -                
         MVC   BYRPAGY2,AGYX            - TO THE BUYER RECORD VIA -             
         MVC   BYRPOFC2,OFFICE          - SUPERVISOR                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'            READ DELETED RECORDS                    
         GOTO1 HIGH                                                             
         B     XR7                                                              
*                                                                               
XR6A     GOTO1 SEQ                                                              
XR7      CLC   KEY(5),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XR7A                  NO, CHECK MKT P-KEYS                       
*                                                                               
         CLC   BYRPBYR2,BUYR            YES, RIGHT BUYER?                       
         BNE   XR6A                     NO, CHECK NEXT                          
*                                                                               
         NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         GOTO1 WRITE                    RESTORE P-KEY                           
         B     XR6A                     CHECK NEXT RECORD                       
*                                                                               
XR7A     XC    KEY,KEY                  ADD MKT P-KEYS                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP,=X'0DE2'        SEARCHING FOR ALL MARKET -               
         MVC   BYRPAM,AGYX            - P-KEYS TO THE BUYER FOR -               
         MVC   BYRPOFC,OFFICE          - ALL MEDIA                              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ DELETED RECORDS                     
         GOTO1 HIGH                                                             
         B     XR8A                                                             
*                                                                               
XR8      GOTO1 SEQ                                                              
XR8A     CLC   KEY(2),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XR8B                  NO, DONE                                   
*                                                                               
* CHECK FOR CORRECT MEDIA - C1,C2,C4 HERE?                                      
         CLC   BYRPOFC,OFFICE           CORRECT OFFICE?                         
         BH    XR8B                     HIGHER, DONE                            
         CLC   BYRPBYR,BUYR            CORRECT BUYER?                           
         BNE   XR8                     NO, CHECK NEXT                           
*                                                                               
         NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         GOTO1 WRITE                    RESTORE P-KEY                           
         B     XR8                      CHECK NEXT RECORD                       
*                                                                               
XR8B     XC    KEY,KEY                  RESTORE BUBL P-KEYS                     
         LA    R4,KEY                                                           
         MVC   BYRPTYP3,=X'0DE4'                                                
         MVC   BYRPAGY3,AGYX                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRBBLQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   XRX                                                              
         MVC   BYRPBUBL,BYRBBLNM-BYRBBLD(R6)                                    
         CLC   BYRPBUBL,SPACES     IF BUYER IS DELETED, DON'T RESTORE           
         BNH   XRX                                                              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'            READ DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS3),KEYSAVE  PASSIVE POINTER FOUND?                  
         BNE   XRX                       NO, DONE                               
         NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         MVC   KEY+14(4),DISKADD        DISK ADDRESS OF RECORD IN VR            
         GOTO1 WRITE                    WRITE BACK KEY RESTORED KEY             
         B     XRX                      NO, DONE                                
         DROP  R4                                                               
*                                                                               
* FOR XRECADD AND XRECPUT                                                       
XR9      LA    R3,DELMKTS                                                       
*                                                                               
XR10     OC    0(L'DELMKTS,R3),0(R3)    DONE AT 1ST BLANK ENTRY                 
         BZ    XR30                                                             
*                                                                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                  DELETE PTR TO BUYER VIA -               
         LA    R4,KEY                   - MKT FOR ALL DELETED MKTS              
         MVC   BYRPTYP,=X'0DE2'                                                 
         MVC   BYRPAM,BAGYMD                                                    
         MVC   BYRPOFC,OFFICE                                                   
         MVC   BYRPMKT,0(R3)                                                    
         MVC   BYRPBYR,BUYR                                                     
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS),KEYSAVE    PASSIVE POINTER FOUND?                 
         BNE   XR15                     NO, CHECK NEXT DELETED MKT              
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    DELETE PASSIVE PTR                      
*                                                                               
XR15     LA    R3,L'DELMKTS(R3)                                                 
         LA    R0,DEMKEND                                                       
         CR    R3,R0                                                            
         BNE   XR10                                                             
*                                                                               
XR30     DS    0H                       DELETE MKT PTR'S TO OTHER BYR'S         
         LA    R3,ADDMKTS                                                       
*                                                                               
XR35     OC    0(L'ADDMKTS,R3),0(R3)    DONE AT 1ST BLANK ENTRY                 
         BZ    XR70                                                             
*                                                                               
         XC    KEY,KEY                  BUILD P KEY TO BUYER BY MKT             
         LA    R4,KEY                                                           
         MVC   BYRPTYP,=X'0DE2'                                                 
         MVC   BYRPAM,BAGYMD                                                    
         MVC   BYRPOFC,OFFICE                                                   
         MVC   BYRPMKT,0(R3)                                                    
*                                                                               
         MVC   AIO,AIO2                 PRESERVE DISKADD OF INPUT REC           
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
*                                                                               
XR50     XC    KEY,KEY                  ADD PASSIVE POINTER TO BUYER -          
         LA    R4,KEY                   - VIA MKT                               
         MVC   BYRPTYP,=X'0DE2'                                                 
         MVC   BYRPAM,BAGYMD                                                    
         MVC   BYRPOFC,OFFICE                                                   
         MVC   BYRPMKT,0(R3)                                                    
         MVC   BYRPBYR,BUYR                                                     
         MVC   KEY+14(4),DISKADD       DISK ADDRESS OF RECORD IN VR             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ FOR DELETED RECORDS                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS),KEYSAVE                                           
         BE    XR60                                                             
         MVC   KEY(18),KEYSAVE          13 FOR KEY, 1 STATUS, 4 D/A             
         GOTO1 ADD                      NO RECORD FOUND, SO ADD P PTR           
         B     XR65                                                             
*                                                                               
XR60     NI    KEY+13,X'FF'-X'80'       REC FOUND, UNDEL PREV PASS PTR          
         GOTO1 WRITE                                                            
*                                                                               
XR65     LA    R3,L'ADDMKTS(R3)                                                 
         LA    R0,ADMKEND                                                       
         CR    R3,R0                                                            
         BNE   XR35                     CHECK NEXT ADDED CLIENT                 
*                                                                               
XR70     DS    0H                       LOGIC FOR SUPV TO BUYER P-KEYS          
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS -                
         MVC   BYRPAGY2,AGYX            - TO THE BUYER RECORD VIA -             
         MVC   BYRPOFC2,OFFICE          - SUPERVISOR                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR78                                                             
*                                                                               
XR74     GOTO1 SEQ                                                              
XR78     CLC   KEY(5),KEYSAVE        PREVIOUS PASSIVE POINTER FOUND?            
         BNE   XR80                  NO, ADD PASS PTR BY SUPV                   
*                                                                               
         CLC   BYRPBYR2,BUYR         IS THIS THE RIGHT BUYER                    
         BNE   XR74                                                             
*                                                                               
         CLC   BYRPSPV2,SUPV       SAME SUPV                                    
         BE    XR100               THEN DONE                                    
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XR74                     CHECK NEXT RECORD                       
*                                                                               
XR80     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS                  
         MVC   BYRPAGY2,AGYX            TO THIS BUYER RECORD WITH               
         MVC   BYRPOFC2,OFFICE          THE SAME SUPERVISOR                     
         MVC   BYRPSPV2,SUPV                                                    
         MVC   BYRPBYR2,BUYR                                                    
         MVC   KEY+14(4),DISKADD       DISK ADDRESS OF RECORD IN VR             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ FOR DELETED RECORDS                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS),KEYSAVE                                           
         BE    XR90                                                             
         MVC   KEY(18),KEYSAVE          13 FOR KEY, 1 STATUS, 4 D/A             
         GOTO1 ADD                      NO RECORD FOUND, SO ADD P PTR           
         B     XR100                                                            
*                                                                               
XR90     NI    KEY+13,X'FF'-X'80'       UNDELETE OLD PASSIVE POINTER            
         GOTO1 WRITE                                                            
*                                                                               
         USING BYRBBLD,R6                                                       
XR100    L     R6,AIO                                                           
         MVI   ELCODE,BYRBBLQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   XRX                                                              
         CLC   SVBUBL,BYRBBLNM     SAME NAME?                                   
         BE    XRX                  YES                                         
*                                                                               
         XC    KEY,KEY             DELETE OLD PASSIVE KEY                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP3,=X'0DE4'                                                
         MVC   BYRPAGY3,AGYX                                                    
         MVC   BYRPBUBL,SVBUBL                                                  
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS3),KEYSAVE  PASSIVE POINTER FOUND?                  
         BNE   XR110                     NO, DON'T WORRY ABOUT IT               
         OI    KEY+13,X'80'             SET DELETED BIT                         
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
*                                                                               
XR110    CLC   BYRBBLNM,SPACES     IF THEY DELETE THE BUYER,                    
         BNH   XRX                  DON'T ADD A PASSIVE                         
*                                                                               
         XC    KEY,KEY             ADD BUBL PASSIVE IF NOT FOUND                
         LA    R4,KEY                                                           
         MVC   BYRPTYP3,=X'0DE4'                                                
         MVC   BYRPAGY3,AGYX                                                    
         MVC   BYRPBUBL,BYRBBLNM                                                
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'            READ DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS3),KEYSAVE  PASSIVE POINTER FOUND?                  
         BNE   XR120                     NO - GO ADD ONE                        
         NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         MVC   KEY+14(4),DISKADD        DISK ADDRESS OF RECORD IN VR            
         GOTO1 WRITE                    WRITE BACK KEY RESTORED KEY             
         B     XRX                                                              
*                                                                               
XR120    MVC   KEY,KEYSAVE                                                      
         MVC   KEY+14(4),DISKADD       DISK ADDRESS OF RECORD IN VR             
         GOTO1 ADD                                                              
*                                                                               
XRX      MVC   KEY(13),SAVEKEY                                                  
         MVC   AIO,AIO1                 RESTORE DISKADD FOR DISPLAY             
         MVI   RDUPDATE,C'N'            DON'T READ DELETED REC'S -              
         NI    DMINBTS,X'FF'-X'08'      - NEXT CALL TO DISPLAY                  
         CLI   ACTEQU,ACTDEL                                                    
         BNE   DR                       DISPLAY AFTER CHANGE PASS KEYS          
         B     XIT                      JUST EXIT IF ACTION DELETE              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
ERROFFR  MVC   ERRNUM,=AL2(OFFREQD)                                             
         J     SPERREX                                                          
ERRMISPV MVC   ERRNUM,=AL2(MISSUPV)                                             
         J     SPERREX                                                          
ERRELEM  MVC   ERRNUM,=AL2(MISSEL)                                              
         J     SPERREX                                                          
ERRNWS   MVC   ERRNUM,=AL2(BDNWS)                                               
         J     SPERREX                                                          
ERRDAR   MVC   ERRNUM,=AL2(BDDAR)                                               
         J     SPERREX                                                          
ERRDUP   MVC   ERRNUM,=AL2(DUPMK)                                               
         J     SPERREX                                                          
ERRNOSPV MVC   ERRNUM,=AL2(NOSPV)                                               
         J     SPERREX                                                          
ERRNOOFF MVC   ERRNUM,=AL2(NOOFF)                                               
         J     SPERREX                                                          
ERRMKTEX MVC   ERRNUM,=AL2(MKTEX)                                               
         J     SPERREX                                                          
ERRNOMKT MVC   ERRNUM,=AL2(NOMK)                                                
         J     SPERREX                                                          
ERRXALL  MVC   ERRNUM,=AL2(XALL)                                                
         J     SPERREX                                                          
ERRXIND  MVC   ERRNUM,=AL2(XIND)                                                
         J     SPERREX                                                          
ERRNOMIX MVC   ERRNUM,=AL2(NOMIX)                                               
         J     SPERREX                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         J     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         J     VSFMERR                                                          
ERRREP   MVC   ERRNUM,=AL2(USEREP)                                              
         J     SPERREX                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         SPACE 2                                                                
*                                                                               
NOOFF    EQU   432                      OFFICE DOESN'T EXIST                    
NOSPV    EQU   435                      SUPERVISOR DOESN'T EXIST                
NOMK     EQU   434                      MARKET DOESN'T EXIST TO DELETE          
MKTEX    EQU   433                      CAN'T ADD EXISTING MARKET               
DUPMK    EQU   439                      CAN'T ADD + SUBTRACT IN ONE EX          
BDNWS    EQU   441                      BAD NWS CODE                            
BDDAR    EQU   442                      BAD DARE CODE                           
MISSEL   EQU   471                                                              
XALL     EQU   790                      MUST DEL INDIV MKTS BEFORE ALL          
XIND     EQU   791                      MUST DEL "ALL" MKTS BEFORE IND          
NOMIX    EQU   805                      CANNOT MIX MKTS AND MKT GRPS            
MISSUPV  EQU   811                      MISSING SUPV RECORD                     
USEREP   EQU   952                      PLEASE, USE ACTION REPORT               
DUPBYR   EQU  1169                      BUYER NAME IN USE                       
OFFREQD  EQU  1214                      OFFICE CODE IS REQUIRED                 
*                                                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         SPACE 2                                                                
**** ROUTINE TO BUMP TO NEXT SCREEN FIELD ****                                  
NXTSCRF  DS    0H                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*---------------------------------------------------------------------          
*   ROUTINE TO VALIDATE OFFICE CODE AGAINST EXISTING OFFICE RECORDS **          
*---------------------------------------------------------------------          
VALOFF   NTR1                                                                   
         CLI   ACTEQU,ACTADD                                                    
         BE    *+12                                                             
         CLI   ACTEQU,13                ACTION TRANSFER?                        
         BNE   VLOFX                    NOT ADD OR TRANSFER, EXIT               
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD OFFICE KEY                             
         USING OFCRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   OFCKTYP,=X'0D60'                                                 
         MVC   OFCKAGY,AGYX                                                     
         MVC   OFCKOFC,OFFICE      OFFICE CODE                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'OFCKEY),KEYSAVE                                            
         BE    VLOFX                                                            
*                                                                               
         LA    R2,BYMOFFH                                                       
         B     ERRNOOFF                                                         
VLOFX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO VALIDATE SUPV CODE AGAINST EXISTING SUPV RECORDS **              
*---------------------------------------------------------------------          
VALSUPV  NTR1                                                                   
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD SUPV KEY                               
         USING SPVRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE      OFFICE CODE                                  
*                                                                               
         MVC   SPVKSPV,BYMSUP                                                   
         OC    SPVKSPV,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    VLSPX                                                            
*                                                                               
         LA    R2,BYMSUPH                                                       
         B     ERRNOSPV                                                         
VLSPX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO VALIDATE BYR CODE FOR ACTION TRANSFER **                         
*---------------------------------------------------------------------          
VALBYR   NTR1                                                                   
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD SUPV KEY                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,AGYX                                                     
         MVC   BYRKOFC,OFFICE      OFFICE CODE                                  
*                                                                               
         MVC   BYRKBYR,BUYRVAL                                                  
         OC    BYRKBYR,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BE    VLSPX                                                            
*                                                                               
         LR    R2,R3                    POINT R2 TO APPROP BUYER FIELD          
         B     ERRINV                  NEW ERROR CODE?                          
VLBYX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO GET SUPERVISOR NAME **                                           
*---------------------------------------------------------------------          
GETSPN   NTR1                                                                   
         XC    BLOCK(50),BLOCK                                                  
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD SUPV KEY                               
         USING SPVRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE      OFFICE CODE                                  
         MVC   SPVKSPV,SUPV                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    GTSP10                                                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    GTSPX                                                            
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    GTSPX                                                            
         LA    R2,BYMSUPH                                                       
         B     ERRMISPV            MISSING SUPV REC                             
*                                                                               
GTSP10   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'             SUPERVISOR NAME ELEMENT                 
         BRAS  RE,GETEL                                                         
*                                                                               
         USING SPVNAMED,R6                                                      
         XC    BLOCK(50),BLOCK                                                  
         MVC   BLOCK(L'SPVLNAME),SPVLNAME                                       
         MVC   BLOCK+15(L'SPVFNAME),SPVFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'BYMSPN)                              
*        MVC   BYMSPN,BLOCK                                                     
*        OI    BYMSPNH+6,X'80'                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
GTSPX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO ADD ALL FOR MARKET ELEMENT ****                              
*---------------------------------------------------------------------          
ADDALL   NTR1                                                                   
         CLI   5(R2),3             ONLY ALL'S ACCEPTED                          
         BH    ERRINV                                                           
*                                                                               
         L     R6,AIO             CHK IF REC HAS ANY MKT GR ELEMS               
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    ERRNOMIX            CAN'T ADD ALL IF REC HAS MKT GR              
*                                                                               
         L     R6,AIO                                                           
         USING BYRMKTD,R6                                                       
         MVC   ELCODE,MEDELCD                                                   
         BRAS  RE,GETEL                                                         
         BNE   ADDALL10                                                         
         CLC   BYRMKT,=X'FFFF'                                                  
         BNE   ERRXALL             'ALL' INVALID WHEN MKT EXISTS IN REC         
         B     ERRMKTEX            HAVE MKT "ALL" -CREATES DUP PASSIVES         
*                                                                               
ADDALL10 LA    R6,ELEM                                                          
         MVC   BYRMKTEL,MEDELCD    ELEMENT CODE                                 
         MVI   BYRMKTLN,4          ELEMENT LENGTH                               
         MVC   BYRMKT,=X'FFFF'     ALL                                          
         GOTO1 ADDELEM                                                          
         MVC   ADDMKTS(2),=X'FFFF'                                              
ADDALLX  B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------          
*    SUBROUTINE TO ADD CLIENT A SINGLE CLIENT ELEMENT ****                      
*---------------------------------------------------------------------          
ADDMKT   NTR1                                                                   
         CLI   5(R2),4                                                          
         BH    ERRINV                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                  ALSO HAS A DM CALL                      
         MVC   AIO,AIO1                                                         
*                                                                               
         USING BYRMKTD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
AMK10    BRAS  RE,NEXTEL                                                        
         BNE   AMK20                                                            
         CLC   BYRMKT,BMKT                                                      
         BE    ERRMKTEX            MARKET ALREADY EXISTS                        
         CLC   BYRMKT,=X'FFFF'     ALL MKT ALREADY ADDED                        
         BE    ERRXIND                                                          
         B     AMK10                                                            
         DROP  R6                                                               
*                                                                               
AMK20    XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING BYRMKTD,R5                                                       
*                                                                               
         MVC   BYRMKTEL,MEDELCD    ELEMENT CODE                                 
         MVI   BYRMKTLN,4          ELEMENT LENGTH                               
         MVC   BYRMKT,BMKT                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
ACX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO DELETE ALL MKT ****                                          
*---------------------------------------------------------------------          
DELALL   NTR1                                                                   
         USING BYRMKTD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BRAS  RE,GETEL                                                         
         BNE   ERRNOMKT                                                         
DELALL05 CLC   BYRMKT,=X'FFFF'                                                  
         BE    DELALL10                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   ERRNOMKT                                                         
         B     DELALL05                                                         
*                                                                               
DELALL10 MVI   ELCODE,X'FF'                                                     
         MVI   0(R6),X'FF'                                                      
         GOTO1 REMELEM                                                          
         MVC   DELMKTS(2),=X'FFFF'                                              
DELALLX  B     XIT                                                              
         DROP  R6                                                               
*---------------------------------------------------------------------          
*    SUBROUTINE TO DELETE CLIENT ****                                           
*---------------------------------------------------------------------          
DELMKT   NTR1                                                                   
         USING BYRMKTD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DEL10    BRAS  RE,NEXTEL                                                        
         BNE   ERRNOMKT                                                         
         CLC   BYRMKT,BMKT2                                                     
         BNE   DEL10                                                            
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
DELX     B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATE NWS CODE                                                      
*---------------------------------------------------------------------          
VALNWS   NTR1                                                                   
         XC    KEY,KEY                                                          
         USING NBYRRECD,R4                                                      
         LA    R4,KEY                                                           
         MVI   NBYRKTYP,X'0D'                                                   
         MVI   NBYRKSUB,X'65'                                                   
         MVC   NBYRKAGMD,BAGYMDS                                                
         MVC   NBYRKBYR,SAVENWS                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(6),KEY                                                   
         BNE   ERRNWS                                                           
         DROP  R4                                                               
VNX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATE DARE CODE                                                     
*---------------------------------------------------------------------          
VALDARE  NTR1                                                                   
         XC    KEY,KEY                                                          
         USING DBYRRECD,R4                                                      
         LA    R4,KEY                                                           
         MVI   DBYRKTYP,X'0D'                                                   
         MVI   DBYRKSUB,X'31'           WHAT ABOUT 'C1' IN SPADBUYER?           
         MVC   DBYRKAM,BAGYMDS                                                  
         MVC   DBYRKBYR,SAVEDAR                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(6),KEY                                                   
         BNE   ERRDAR                                                           
         DROP  R4                                                               
VDX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        GETS PID NAME USING PIDNUM                                             
*---------------------------------------------------------------------          
GETPIDNM NTR1                                                                   
         USING SA0REC,R6                                                        
         MVC   PIDNAME,SPACES                                                   
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,PIDNUM                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(L'SA0KEY),0(R6)                                             
         BNE   GPIDX                                                            
         TM    SA0STAT,X'20'       LOCKED                                       
         BO    GPIDX                                                            
*                                                                               
         USING SAPALD,R6                                                        
         MVI   ELCODE,SAPALELQ     X'C3' - PERSONAL ID ELEM                     
         BRAS  RE,GETEL2                                                        
         BNE   GPIDX                                                            
         MVC   PIDNAME,SAPALPID                                                 
GPIDX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATES PID NAME AND GETS PID NUMBER                                 
*---------------------------------------------------------------------          
VALPID   NTR1                                                                   
         USING SAPEREC,R6                                                       
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   VPIDXNO                                                          
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BRAS  RE,GETEL2                                                        
         BNE   VPIDXNO                                                          
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
VPIDXYES B     XYES                                                             
VPIDXNO  B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* ROUTINE TO LIST MARKET GROUPS BY MEDIA                                        
*---------------------------------------------------------------------          
MKGLS    NTR1                                                                   
* PFKEY STUFF                                                                   
         CLI   PFKEY,8                  DOWN?                                   
         BNE   MGL10                    NO CHECK FOR PF6= TOP                   
         LA    R2,BYMENDH                                                       
         CLI   7(R2),0              LAST FIELD BLANK?                           
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTMKG,FRSTMKG         YES,DON'T PAGE DOWN                      
         MVC   STARTMKG,LASTMKG                                                 
         B     MGL20                                                            
*                                                                               
MGL10    CLI   PFKEY,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTMKG,FRSTMKG          START AT FIRST CLIENT KEY               
         MVC   STARTMKG,FRSTMKG         IF ANY OTHER PFKEY,START                
*                                                                               
MGL20    XC    FRSTMKG,FRSTMKG                                                  
         XC    LASTMKG,LASTMKG                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTMKG,STARTMKG        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         LA    R2,BYMCODEH         FIRST DISPLAY FIELD                          
*                                                                               
         USING BYRMKGD,R6                                                       
         L     R6,AIO1                                                          
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
MGL30    BRAS  RE,NEXTEL                                                        
         BNE   MGLX                NO MORE MKT GROUPS                           
         CLC   STARTMKG,BYRMKGID   ARE WE AT START                              
         BH    MGL30               NO, GET NEXT                                 
*                                                                               
         LA    R1,BYMCODEH                                                      
         CR    R2,R1                                                            
         BNE   *+10                     NO                                      
         MVC   FRSTMKG,BYRMKGID         YES, STORE FIRST MARKET                 
         MVC   LASTMKG,BYRMKGID         STORE LAST MARKET EVERY TIME            
*                                                                               
         MVC   MGNAME,SPACES                                                    
         MVI   MGRPLEN,4           DEFAULT TO 4 - IN CASE REC IS GONE           
         USING MKGRECD,R4                                                       
         LA    R4,KEY                   GET LEN FROM ID REC                     
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD                                                  
         XC    MKGKCLT,MKGKCLT          BINARY CLIENT CODE                      
         XC    MKGKPID,MKGKPID                                                  
         XC    MKGKPGRP,MKGKPGRP                                                
         MVC   MKGKMID,BYRMKGID                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE          IF MARKET DEFIN DOESN'T EXIST           
         BNE   MGL50                    STILL DISPLAY IF NO RECORD              
***>     BNE   ERRINV                   MGROUP NOT VALID                        
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         LA    R4,24(R4)                                                        
         CLI   0(R4),X'01'         HAS TO BE FIRST                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL01,R4                                                       
         LLC   R1,MKGBK1LN                                                      
         LLC   R0,MKGBK2LN                                                      
         AR    R1,R0                                                            
         LLC   R0,MKGBK3LN                                                      
         AR    R1,R0                                                            
         STC   R1,MGRPLEN                                                       
*                                                                               
         USING MKGRECD,R4                                                       
         LA    R4,KEY                   GET LEN FROM ID REC                     
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD                                                  
         XC    MKGKCLT,MKGKCLT          BINARY CLIENT CODE                      
         XC    MKGKPID,MKGKPID                                                  
         XC    MKGKPGRP,MKGKPGRP                                                
         MVC   MKGKMID,BYRMKGID                                                 
         MVC   MKGKMGRP,BYRMKGRP                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE          IF MARKET GROUP DOESN'T EXIST           
         BE    MGL38                    GOT IT - CONTINUE                       
         MVI   MGRPLEN,4                DISPLAY ALL 4 DIGITS                    
         B     MGL50                    STILL DISPLAY IF NO RECORD              
***>     BNE   ERRINV                   MGROUP NOT VALID                        
*                                                                               
MGL38    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         LA    R4,24(R4)                                                        
MGL40    CLI   0(R4),0                                                          
         BE    MGL50                                                            
         CLI   0(R4),X'10'         NAMES ELEM                                   
         BE    MGL42                                                            
         LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     MGL40                                                            
*                                                                               
         USING MKGEL10,R4                                                       
MGL42    MVC   MGNAME,MKGNAM1      PICK OUT MOST SPECIFIC NAME                  
         CLC   MKGNAM2,SPACES                                                   
         BNH   *+10                                                             
         MVC   MGNAME,MKGNAM2                                                   
         CLC   MKGNAM3,SPACES                                                   
         BNH   *+10                                                             
         MVC   MGNAME,MKGNAM3                                                   
*                                                                               
MGL50    XC    BLOCK(100),BLOCK    SET UP FOR SQUASHER                          
*                                                                               
         LA    R5,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
MGL50A   CLC   BYRMKGID,2(R5)                                                   
         BE    MGL50B                                                           
         LA    R5,3(R5)                                                         
         BCT   RF,MGL50A                                                        
         B     ERRINV                                                           
*                                                                               
MGL50B   MVC   BLOCK(2),0(R5)      MKT GRP ID                                   
         LA    R5,BLOCK+1                                                       
         CLI   0(R5),C' '          GOT A 2-CHAR MGROUP ID?                      
         BNH   *+8                 NOPE                                         
         LA    R5,1(R5)                                                         
*                                                                               
         UNPK  DUB,BYRMKGRP(3)                                                  
         LLC   R1,MGRPLEN                                                       
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),DUB+3       MKT GROUP NUMBER                             
         MVC   BLOCK+7(L'MGNAME),MGNAME                                         
         MVC   8(L'BYMCODE,R2),BLOCK                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MKDLQ(R2)                                                     
         LA    R1,BYMENDH                                                       
         CR    R2,R1                                                            
         BNH   MGL30                                                            
*                                                                               
MGLX     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6,R4                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
MKHEAD   DC    CL79'MARKET LIST'                                                
*                                                                               
*                                                                               
* * * * * * * * * *                                                             
* OFFLINE REPORT  *                                                             
* * * * * * * * * *                                                             
*                                                                               
PR       DS    0H                                                               
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R1,=A(HEDSPECS)           SET UP HEADHOOK AND SPECS              
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
*                                  BEFORE ANYTHING, VALIDATE FILTERS            
         LA    R2,BRLBGPH                                                       
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         MVC   BGFILTER,BRLBGP                                                  
         OI    FLTFLAG,BGFLTQ                                                   
*                                                                               
         LA    R2,BRLBYCH                                                       
         CLI   5(R2),0                                                          
         BE    *+20                                                             
         MVC   BCFILTER,BRLBYC                                                  
         OC    BCFILTER,SPACES                                                  
         OI    FLTFLAG,BCFLTQ                                                   
*                                                                               
         XC    SPFILTER,SPFILTER                                                
         LA    R2,BRLSUPH                                                       
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         MVC   SPFILTER,BRLSUP                                                  
         OI    FLTFLAG,SPFLTQ                                                   
         OC    SPFILTER,SPACES                                                  
*                                                                               
         LA    R2,BRLOFFH                                                       
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         MVC   OFFILTER,BRLOFF                                                  
         OI    FLTFLAG,OFFLTQ                                                   
*                                                                               
         LA    R2,BRLMEDH                                                       
         CLI   5(R2),0                                                          
         BE    PR02                                                             
         MVC   BYTE,BRLMED                                                      
         LHI   R0,MEDLTRQ                                                       
         BRAS  RE,GETMED                                                        
         MVC   MDFILTER,1(RF)                                                   
         OI    FLTFLAG,MDFLTQ                                                   
*                                                                               
PR02     DS    0H                                                               
         LA    R2,BRLMKTH                                                       
         CLI   5(R2),0                                                          
         BE    PR05                                                             
*                                                                               
         CLI   BRLMKT,C'0'         MARKET OR MARKET GROUP?                      
         BL    PR03                                                             
* MARKET HERE                                                                   
         LLC   R1,BRLMKTH+5        LENGTH OF MARKET                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DWORD,BRLMKT(0)   PACK THE MARKET NUMBER                         
* IF MARKET CONTAINS CHARACTERS PACK WILL DIE                                   
         CVB   R1,DWORD                                                         
         STCM  R1,3,MKFILTER                                                    
         OI    FLTFLAG,MKFLTQ                                                   
         B     PR05                                                             
*                                                                               
PR03     DS    0H                  MARKET GROUP HERE                            
         MVC   MGFILTER(1),BRLMKT  FIRST CHARACTER                              
         LLC   R1,BRLMKTH+5        LENGTH OF MARKET GROUP                       
         BCTR  R1,0                LESS FIRST CHARACTER                         
         BCTR  R1,0                                                             
         MVC   WORK(5),=C'00000'                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),BRLMKT+1    PACK THE MARKET NUMBER                       
         PACK  DUB(3),WORK(5)                                                   
         MVC   MGFILTER+1(L'MGFILTER-1),DUB                                     
         OI    FLTFLAG,MGFLTQ                                                   
*                                                                               
PR05     DS    0H                                                               
*                                                                               
* * *                              BUILD KEY                                    
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING BYRKEY,R1                                                        
*                                                                               
         MVC   BYRKTYP,=X'0D62'                                                 
*                                                                               
         MVC   BYRKAGY,BAGYMD                                                   
         NI    BYRKAGY,X'F0'      TURN OFF MEDIA                                
*                                                                               
         TM    FLTFLAG,BGFLTQ     FILTERING BY BUY GROUP?                       
         BNO   PR10               PROCEED TO READHI                             
         MVC   BYRKOFC,BGFILTER                                                 
*                                                                               
         TM    FLTFLAG,BCFLTQ                                                   
         BNO   PR10                                                             
         MVC   BYRKBYR,BCFILTER                                                 
         DROP  R1                                                               
* * *                              END BUILD KEY                                
PR10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH               FIND FIRST BUYER REC                          
         B     PR30                                                             
*                                                                               
PR20     GOTO1 SEQ                FIND SUBSEQUENT BUYER RECS                    
*                                                                               
PR30     DS    0H                                                               
         CLC   KEY(3),KEYSAVE     IF PAST BUYER RECORDS & OUR AGY               
         BNE   PRX                STOP READING                                  
*                                                                               
* KEY FILTERS - BUYER GROUP AND B CODE                                          
*                                                                               
         LA    R1,KEY                                                           
         USING BYRKEY,R1                                                        
         OC    BYRKOFC,BYRKOFC    ANY OFFICE CODE?                              
         BZ    PR20               NO, THIS IS AN AGY HI-BBL REC!                
*                                                                               
         TM    FLTFLAG,BGFLTQ     FILTERING BY BUYER GROUP?                     
         BNO   *+14                                                             
         CLC   BYRKOFC,BGFILTER   SAME BC?                                      
         BNE   PR20                                                             
*                                                                               
         TM    FLTFLAG,BCFLTQ     FILTERING BY BUYER CODE?                      
         BNO   *+14                                                             
         CLC   BYRKBYR,BCFILTER   SAME BC?                                      
         BNE   PR20                                                             
         DROP  R1                                                               
*                                                                               
         GOTO1 GETREC             GET BUYER DATA                                
*                                                                               
PR40     L     R6,AIO                                                           
*                                                                               
* NON-KEY FILTERS                                                               
*                                                                               
*                                                                               
         TM    FLTFLAG,SPFLTQ     FILTERING BY SUPERVISOR?                      
         BNO   *+12                                                             
         BRAS  RE,FLTSUP                                                        
         BNE   PR20                                                             
*                                                                               
         TM    FLTFLAG,OFFLTQ     FILTERING BY OFFICE?                          
         BNO   *+12                                                             
         BRAS  RE,FLTOFF                                                        
         BNE   PR20                                                             
*                                                                               
         TM    FLTFLAG,MKFLTQ+MGFLTQ    FILTERING BY MARKET/GROUP?              
         BZ    *+12                                                             
         BRAS  RE,FLTMKT                                                        
         BNE   PR20                                                             
*                                                                               
         TM    FLTFLAG,MDFLTQ      FILTERING BY MEDIA?                          
         BZ    *+12                                                             
         BRAS  RE,FLTMED                                                        
         BNE   PR20                                                             
*                                                                               
* END OF FILTERING, FORMAT THE REPORT PRINT LINES                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SPACE BETWEEN BUYERS                         
*                                                                               
         LA    R4,P                                                             
         L     R6,AIO                                                           
         USING BYRKEY,R6                                                        
*                                                                               
         TM    FLTFLAG,BGFLTQ      FILTERED BY GROUP?                           
         BO    PR60                IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE1,R4                                                        
         MVC   PLBGRP(2),BYRKOFC                                                
         LA    R4,(PLINE2-PLINE1)(R4)                                           
         DROP  R4                                                               
*                                                                               
PR60     DS    0H                                                               
         TM    FLTFLAG,BCFLTQ      FILTERED BY BUYER?                           
         BO    PR80                IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE2,R4                                                        
         MVC   PLBCODE,BYRKBYR                                                  
         LA    R4,(PLINE3-PLINE2)(R4)                                           
         DROP  R4,R6                                                            
*                                                                               
PR80     DS    0H                                                               
         USING PLINE3,R4                                                        
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BYRNAMED,R6                                                      
         MVC   PLFNAME,BYRFNAME                                                 
         MVC   PLLNAME,BYRLNAME                                                 
         TM    BYROPT1,BYROPT1_ASS                                              
         BNO   *+10                                                             
         MVC   PLASSIST,=CL2'Y'                                                 
         MVC   PLOFFICE(2),BYROFF                                               
         MVC   PLFILTER(1),BYRFILT                                              
*        MVC   PLSUPER,BYRSPV                                                   
         MVC   SUPV,BYRSPV         WILL NEED LATER FOR SUPERV NAME              
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING BYRKEY,R6                                                        
         MVC   OFFICE,BYRKOFC                                                   
         MVC   AGYX,BYRKAGY                                                     
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         BAS   RE,GETSPN                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVC   PLSPNAME,BLOCK      SUPERVISOR'S FULL NAME                       
         DROP  R6                                                               
*                                                                               
         LA    R4,(PLMKLIST-PLINE3)(R4)                                         
         BRAS  RE,PRTMG           PRINT MARKETS+GROUPS                          
*                                                                               
         B     PR20                                                             
         DROP  R4                                                               
*                                                                               
PRX      J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,H4                                                            
*                                                                               
         TM    FLTFLAG,BGFLTQ      FILTERED BY GROUP?                           
         BNO   HOOK02              IF NO - DON'T PRINT GROUP IN HEADER          
         MVC   0(12,R4),=CL12'Buy Group:'                                       
         MVC   12(L'BGFILTER,R4),BGFILTER                                       
         LA    R4,(12+L'BGFILTER)(R4)                                           
*                                                                               
HOOK02   DS    0H                                                               
         TM    FLTFLAG,BCFLTQ      FILTERED BY BUYER?                           
         BNO   HOOK04              IF YES - NO NEED TO PRINT IT IN LINE         
*                                                                               
         TM    FLTFLAG,BGFLTQ      FILTERING BY GROUP?                          
         BNO   *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,3(R4)                                                         
*                                                                               
         MVC   0(12,R4),=CL12'BUYER CODE:'                                      
         MVC   12(L'BCFILTER,R4),BCFILTER                                       
*                                                                               
HOOK04   DS    0H                                                               
         LA    R4,H6                                                            
*                                                                               
         TM    FLTFLAG,BGFLTQ      FILTERED BY GROUP?                           
         BO    HOOK10              IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE1,R4                                                        
         MVC   PLBGRP(3),=CL3'Buy'                                              
         LA    R4,(PLINE2-PLINE1)(R4)                                           
         DROP  R4                                                               
*                                                                               
HOOK10   DS    0H                                                               
         TM    FLTFLAG,BCFLTQ      FILTERED BY BUYER?                           
         BO    HOOK20              IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE2,R4                                                        
         MVC   PLBCODE,=CL4' ID'                                                
         LA    R4,(PLINE3-PLINE2)(R4)                                           
         DROP  R4                                                               
*                                                                               
HOOK20   DS    0H                                                               
         USING PLINE3,R4                                                        
*                                                                               
         MVC   PLFNAME,=CL10'First'                                             
         MVC   PLLNAME,=CL14'Last'                                              
         MVC   PLASSIST,=CL2'As'                                                
         MVC   PLOFFICE,=CL3'Off'                                               
         MVC   PLFILTER,=CL3'Flt'                                               
*        MVC   PLSUPER,=CL4'Supv'                                               
         MVC   PLSPNAME,=CL24'Supervisor'                                       
         MVC   PLMEDIA,=CL1'M'                                                  
         MVC   PLMKTGRP,=CL6'Mkt'                                               
         MVC   PLMKT,=CL4'Mkt'                                                  
         MVC   PLMKTNAM,=CL20'Market'                                           
         MVC   PLMK2,=CL4'Mkt'                                                  
         MVC   PLMK2NAM,=CL20'Market'                                           
*                                                                               
*                                                                               
         LA    R4,H7                                                            
*                                                                               
         TM    FLTFLAG,BGFLTQ      FILTERED BY GROUP?                           
         BO    HOOK30              IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE1,R4                                                        
         MVC   PLBGRP(3),=CL3'Grp'                                              
         LA    R4,(PLINE2-PLINE1)(R4)                                           
         DROP  R4                                                               
*                                                                               
HOOK30   DS    0H                                                               
         TM    FLTFLAG,BCFLTQ      FILTERED BY BUYER?                           
         BO    HOOK40              IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE2,R4                                                        
         LA    R4,(PLINE3-PLINE2)(R4)                                           
         DROP  R4                                                               
*                                                                               
HOOK40   DS    0H                                                               
         USING PLINE3,R4                                                        
*                                                                               
         MVC   PLFNAME,=CL10'Name'                                              
         MVC   PLLNAME,=CL14'Name'                                              
         MVC   PLASSIST,=CL2'st'                                                
         MVC   PLSPNAME,=CL24'name'                                             
         MVC   PLMKTGRP,=CL6'Group'                                             
         MVC   PLMKTNAM,=CL20'Name'                                             
         MVC   PLMK2NAM,=CL20'Name'                                             
*                                                                               
*                                                                               
*                                                                               
         LA    R4,H8                                                            
*                                                                               
         TM    FLTFLAG,BGFLTQ      FILTERED BY GROUP?                           
         BO    HOOK50              IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE1,R4                                                        
         MVC   PLBGRP,=2C'-'                                                    
         LA    R4,(PLINE2-PLINE1)(R4)                                           
         DROP  R4                                                               
*                                                                               
HOOK50   DS    0H                                                               
         TM    FLTFLAG,BCFLTQ      FILTERED BY BUYER?                           
         BO    HOOK60              IF YES - NO NEED TO PRINT IT IN LINE         
         USING PLINE2,R4                                                        
         MVC   PLBCODE,=4C'-'                                                   
         LA    R4,(PLINE3-PLINE2)(R4)                                           
         DROP  R4                                                               
*                                                                               
HOOK60   DS    0H                                                               
         USING PLINE3,R4                                                        
*                                                                               
         MVC   PLFNAME,=10C'-'                                                  
         MVC   PLLNAME,=14C'-'                                                  
         MVC   PLASSIST,=2C'-'                                                  
         MVC   PLOFFICE,=3C'-'                                                  
         MVC   PLFILTER,=3C'-'                                                  
*        MVC   PLSUPER,=4C'-'                                                   
         MVC   PLSPNAME,=24C'-'                                                 
         MVC   PLMEDIA,=C'-'                                                    
         MVC   PLMKTGRP,=6C'-'                                                  
         MVC   PLMKT,=4C'-'                                                     
         MVC   PLMKTNAM,=20C'-'                                                 
         MVC   PLMK2,=4C'-'                                                     
         MVC   PLMK2NAM,=20C'-'                                                 
*                                                                               
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,2,REQUESTOR                                                   
         SSPEC H1,52,C'BUYER REPORT'                                            
         SSPEC H1,92,AGYNAME                                                    
*                                                                               
         SSPEC H2,2,PAGE                                                        
         SSPEC H2,52,C'------------'                                            
         SSPEC H2,92,AGYADD                                                     
*                                                                               
         SSPEC H3,92,REPORT                                                     
         SSPEC H3,104,RUN                                                       
         DC    X'00'                                                            
*                                                                               
*                                                                               
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
SET10    CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    SETUPX                                                           
*                                                                               
         MVC   MPF02ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF02ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF03ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF03ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF06ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF06ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF08ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF08ACT,=CL8'DISPLAY'                                           
*                                                                               
         CLI   CALLSP,0             ANYTHING WAITING TO RETURN TO?              
         BE    *+8                  NO                                          
         NI    BYMREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
         OI    BYMREH+6,X'80'       TRANSMIT THE RESULT                         
*                                                                               
SETUP99  GOTO1 INITPFKY,DMCB,MPFTABLE                                           
SETUPX   J     XIT                                                              
         EJECT                                                                  
*                                                                               
MPFTABLE DS    0X                                                               
* PF02 = OFFICE                                                                 
         DC    AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYGRP '        RECORD: OFFICE                               
MPF02ACT DC    CL8'       '        ACTION:                                      
MPF02    DC    AL1(KEYTYTWA,L'BYMOFF-1),AL2(BYMOFF-T217FFD)                     
MPF02X   EQU   *                                                                
*                                                                               
* PF03 = SUPERVISOR                                                             
         DC    AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'SUPV   '        RECORD: SUPERVISOR                           
MPF03ACT DC    CL8'       '        ACTION:                                      
MPF03    DC    AL1(KEYTYTWA,L'BYMOFF-1),AL2(BYMOFF-T217FFD)                     
         DC    AL1(KEYTYTWA,L'BYMBYC-1),AL2(BYMSUP-T217FFD)                     
MPF03X   EQU   *                                                                
*                                                                               
* PF06 = TOP                                                                    
         DC    AL1(MPF06X-*,06,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF06ACT DC    CL8' '                                                           
MPF06X   EQU   *                                                                
*                                                                               
* PF08 = DOWN                                                                   
         DC    AL1(MPF08X-*,08,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF08ACT DC    CL8' '                                                           
MPF08X   EQU   *                                                                
*                                                                               
* PF12 = RETURN TO CALLER                                                       
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        CLEARS BOTTOM OF SCREEN                                                
***********************************************************************         
*                                                                               
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BYMFMKH               CLEARS SCREEN FROM ADD MKT DOWN         
         CLI   ACTEQU,13                ACTION TRANSFER?                        
         BNE   *+8                                                              
         LA    R2,BYTOKH                "ENTER OK" ON TRANSFER SCREEN           
*                                                                               
         LA    R3,BYMPFKYH              SAME ADD. ON TRANSF & DISP              
         CLI   ACTEQU,13                ACTION TRANSFER?                        
         BNE   *+8                                                              
         LA    R3,BYTPFKYH              "ENTER OK" ON TRANSFER SCREEN           
*                                                                               
CLRSCR10 LLC   R1,0(R2)            FIELD LENGTH                                 
         SHI   R1,9                8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLRSCR10            NO                                           
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
* ROUTINE TO LIST MARKETS BY MEDIA                                              
*---------------------------------------------------------------------          
MKLS     NTR1  BASE=*,LABEL=*                                                   
* PFKEY STUFF                                                                   
         CLI   PFKEY,8                  DOWN?                                   
         BNE   ML10                     NO CHECK FOR PF6= TOP                   
         LA    R2,BYMENDH                                                       
         CLI   ACTEQU,13                                                        
         BNE   *+8                                                              
         LA    R2,BYTENDH                                                       
         CLI   7(R2),0              LAST FIELD BLANK?                           
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTMKT,FRSTMKT          YES,DON'T PAGE DOWN                     
         MVC   STARTMKT,LASTMKT                                                 
         B     ML20                                                             
*                                                                               
ML10     CLI   PFKEY,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTMKT,FRSTMKT          START AT FIRST CLIENT KEY               
*                                                                               
         MVC   STARTMKT,FRSTMKT         IF ANY OTHER PFKEY,START                
*                                                                               
ML20     XC    FRSTMKT,FRSTMKT                                                  
         XC    LASTMKT,LASTMKT                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTMKT,STARTMKT        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         XC    KEY,KEY                                                          
         USING BYRRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP,=X'0DE2'                                                 
         MVC   BYRPAM,BAGYMD                                                    
         MVC   BYRPOFC,OFFICE                                                   
         MVC   BYRPMKT,STARTMKT                                                 
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         USING DMKTEL,R3                                                        
         LA    R3,BYMCODEH                                                      
         GOTO1 HIGH                                                             
         B     ML35                                                             
ML25     MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
ML30     GOTO1 SEQ                                                              
ML35     CLC   KEY(5),SAVEKEY2                                                  
         BNE   MLX                                                              
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         CLC   BYRPBYR,BUYR                                                     
         BNE   ML30                                                             
*                                                                               
         CLC   BYRPMKT,=X'FFFF'                                                 
         BNE   ML40                                                             
         MVC   DMKTCD(3),=C'ALL'                                                
         MVC   DMKTNM(11),=C'ALL MARKETS'                                       
         OI    6(R3),X'80'                                                      
         B     MLX                                                              
*                                                                               
ML40     ZICM  R0,BYRPMKT,2                                                     
         CVD   R0,DUB                                                           
         UNPK  DMKTCD,DUB                                                       
         OI    DMKTCD+3,X'F0'                                                   
*                                                                               
         LA    R1,BYMCODEH                                                      
         CR    R3,R1                                                            
         BNE   *+10                     NO                                      
         MVC   FRSTMKT,BYRPMKT          YES, STORE FIRST MARKET                 
*                                                                               
         MVC   LASTMKT,BYRPMKT          STORE LAST MARKET EVERY TIME            
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0000000008040000'                                  
         MVC   TEMPFLD+8(4),DMKTCD                                              
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,3                                                       
         GOTO1 VALIMKT                                                          
         MVC   AIO,AIO2                                                         
         DROP  R4                                                               
*                                                                               
         MVC   DMKTNM,MKTNM                                                     
         OI    6(R3),X'80'                                                      
*                                                                               
         LA    R3,MKDLQ(R3)                                                     
         LA    R1,BYMENDH               SAME ADD. ON TRANS AND DIS              
         CR    R3,R1                                                            
         BNH   ML25                                                             
         DROP  R3                                                               
MLX      J     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*---------------------------------------------------------------------          
* ROUTINE TO REPORT MARKETS BY MEDIA                                            
* R4 IS TO ADDRESS MKT PRINT AREA                                               
* AIO MUST POINT TO BUYER RECORD                                                
*---------------------------------------------------------------------          
PRTMG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVC   SAVEKEY,0(R6)      SAVE BUYER REC KEY                            
         MVI   COLUMN,X'01'                                                     
         USING PLMKLIST,R4                                                      
*                                                                               
         LA    R6,24(R6)           A(FIRST ELEMENT)                             
*                                                                               
PRTM10   DS    0H                                                               
         CLI   0(R6),X'00'         END OF RECORD?                               
         BE    PRTMX                                                            
*                                                                               
         CLI   0(R6),X'02'         LOWER THAN X'02' (TV)?                       
         BL    PRTM100             GET NEXT ELEMENT                             
         CLI   0(R6),X'04'         HIGHER THAN X'04' ?                          
         BH    PRTM40              GROUPS                                       
*                                                                               
         MVC   BYTE,0(R6)                                                       
         LHI   R0,MEDELCDQ                                                      
         BRAS  RE,GETMED                                                        
         MVC   QMED,0(RF)                                                       
*                                  MARKETS HERE                                 
         CLC   2(2,R6),=X'FFFF'                                                 
         BNE   PRTM12                                                           
         MVC   MKTNM,=CL24'ALL MARKETS'                                         
         MVC   QMKT,SPACES                                                      
         B     PRTM13                                                           
*                                                                               
PRTM12   DS    0H                                                               
         LA    R1,2(R6)                                                         
         BAS   RE,GETMKT                                                        
*                                                                               
PRTM13   DS    0H                                                               
         CLI   COLUMN,X'01'        WHICH COLUMN TO USE NEXT?                    
         BNE   PRTM15                                                           
         MVC   PLMEDIA,QMED                                                     
         MVC   PLMKT,QMKT          PUT MARKET ON PRINT LINE                     
         MVC   PLMKTNAM,MKTNM                                                   
         MVI   COLUMN,X'02'                                                     
*                                                                               
         LR    R2,R6               POINT TO CURRENT X'02-X'04' ELEM             
         LLC   R0,1(R2)            BUMP TO NEXT ELEM                            
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R2),0             END OF RECORD?                               
         BE    *+12                YES - PRINT LAST LINE!                       
         CLI   0(R2),X'04'         HIGHER THAN X'04' ?                          
         BNH   PRTM100             NO - STILL HAVE MORE TO GO                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT THE LAST LINE                          
         B     PRTM100                                                          
*                                                                               
PRTM15   DS    0H                                                               
         CLC   QMED,PLMEDIA        SAME MEDIA?                                  
         BE    PRTM16                                                           
         GOTO1 SPOOL,DMCB,(R8)    > IF NOT, PRINT THE LINE                      
         MVI   COLUMN,X'01'                                                     
         B     PRTM10             > AND PUT IT IN COLUMN 1                      
PRTM16   MVC   PLMK2,QMKT                                                       
         MVC   PLMK2NAM,MKTNM                                                   
         MVI   COLUMN,X'01'                                                     
         GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINE                               
         B     PRTM100             GET NEXT ELEMENT                             
*                                                                               
* THIS PART IS FOR MARKET GROUP ELEMENTS                                        
*                                                                               
PRTM40   DS    0H                  PROCESSING MARKET GROUPS                     
         MVI   COLUMN,X'01'                                                     
         CLI   0(R6),X'12'         LOWER THAN X'12' (TV)?                       
         BL    PRTM100             GET NEXT ELEMENT                             
         CLI   0(R6),X'15'         HIGHER THAN X'15' ?                          
         BH    PRTMX               DONE                                         
*                                                                               
* READ MARKET GROUP PASSIVE KEYS TO GET MARKETS IN THAT GROUP                   
*                                                                               
         MVC   BYTE,0(R6)                                                       
         LHI   R0,MEDELCDQ                                                      
         BRAS  RE,GETMED                                                        
         MVC   PLMEDIA,0(RF)       OUTPUT MEDIA LETTER FOR MKT GROUP            
*                                                                               
         BRAS  RE,PRTGRP                                                        
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         MVC   SAVEKEY,KEY         SAVE BUYER KEY                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING MKGRECD,R2                                                       
         MVC   MKGKTYP,=X'0D82'                                                 
*                                                                               
         LHI   R0,MEDELCDQ                                                      
         MVC   BYTE,0(R6)                                                       
         BRAS  RE,GETMED                                                        
         MVC   QMED,0(RF)                                                       
         MVC   MKGKAGMD,2(RF)                                                   
*                                                                               
         OC    MKGKAGMD,SAVEKEY+2  ADD AGENCY                                   
         MVC   MKGKMID(3),2(R6)    GROUP ID (3 BYTES)                           
*                                                                               
         GOTO1 HIGH                GET MARKET GROUP PASSIVE KEY                 
         B     PRTM46                                                           
PRTM45   GOTO1 SEQ                                                              
PRTM46   CLC   KEYSAVE(MKGPMKT-MKGKEY),KEY                                      
         BNE   PRTM90              GET NEXT MARKET GROUP ELEMENT                
*                                                                               
         LA    R1,MKGPMKT                                                       
         BRAS  RE,GETMKT           GET NAME, NUMBER FOR MARKET                  
*                                                                               
         CLI   COLUMN,X'01'        WHICH COLUMN TO USE NEXT?                    
         BNE   PRTM50                                                           
         MVC   PLMKT,QMKT          PUT MARKET ON PRINT LINE                     
         MVC   PLMKTNAM,MKTNM                                                   
         MVI   COLUMN,X'02'        SET TO USE COLUMN 2                          
         B     PRTM45              NEXT PASSIVE POINTER                         
PRTM50   DS    0H                                                               
         MVC   PLMK2,QMKT                                                       
         MVC   PLMK2NAM,MKTNM                                                   
         MVI   COLUMN,X'01'        SET TO USE COLUMN 1 AGAIN                    
         GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINE                               
         B     PRTM45              GET NEXT PASSIVE KEY                         
*                                                                               
         DROP  R4                                                               
*                                                                               
PRTM90   DS    0H                                                               
*                                                                               
         CLI   COLUMN,1          > IF ONLY COL 1 IS FILLED IN,                  
         BE    PRTM95            > MAKE SURE IT PRINTS                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTM95   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     SPACE BETWEEN MARKET GROUPS                  
         XC    KEY,KEY             DONE W PASSIVES, RESTORE BUYER KEY           
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
*                                                                               
PRTM100  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0               ADVANCE TO NEXT ELEMENT                      
         B     PRTM10                                                           
*                                                                               
PRTMX    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     SPACE BETWEEN BUYERS                         
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------*                   
* R6 MUST POINT TO MARKET GROUP ELEMENT                                         
*-----------------------------------------------------------*                   
PRTGRP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R6                                                            
         USING PLMKLIST,R4                                                      
*                                                                               
         MVC   SAVEKEY2,KEY         SAVE BUYER KEY                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING MKGRECD,R2                                                       
         MVC   MKGKTYP,=X'0D02'                                                 
*                                                                               
         LHI   R0,MEDELCDQ                                                      
         MVC   BYTE,0(R6)                                                       
         BRAS  RE,GETMED                                                        
         MVC   MKGKAGMD,2(RF)                                                   
*                                                                               
         OC    MKGKAGMD,SAVEKEY+2  ADD AGENCY                                   
         MVC   MKGKMID,2(R6)    GROUP ID                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BNE   PGRP20              DON'T DIE IF GROUP DELETED                   
***>     BE    *+6                                                              
***      DC    H'0'                                                             
*                                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING MKGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PGRPX                                                            
*                                                                               
         LLC   R3,MKGBK1LN                                                      
         LLC   R0,MKGBK2LN                                                      
         AR    R3,R0                                                            
         LLC   R0,MKGBK3LN                                                      
         AR    R3,R0                                                            
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 HEXOUT,DMCB,3(R5),FULL,2,=C'TOG',0                               
*                                                                               
         LA    R0,4                                                             
         SR    R0,R3                                                            
         BZ    PGRP14                                                           
         LA    R1,FULL             POIN TO GROUP NUMBERS                        
         AR    R1,R3               BUMP PAST DEFN LENGTH                        
PGRP12   CLI   0(R1),C'0'          IF NUMBERS GO PAST DEFN LEN                  
         BNE   PGRP20              PRINT THEM ALL OUT                           
         LA    R1,1(R1)                                                         
         BCT   R0,PGRP12                                                        
*                                                                               
PGRP14   BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PLMKTGRP+2(0),FULL                                               
         B     PGRP30                                                           
*                                                                               
PGRP20   XC    FULL,FULL                                                        
         GOTO1 HEXOUT,DMCB,3(R5),FULL,2,=C'TOG',0                               
         MVC   PLMKTGRP+2(4),FULL                                               
*                                                                               
PGRP30   LA    R1,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
PGRP40   CLC   2(1,R1),2(R5)                                                    
         BE    PGRP50                                                           
         LA    R1,3(R1)                                                         
         BCT   RF,PGRP40                                                        
         DC    H'0'                                                             
*                                                                               
PGRP50   MVC   PLMKTGRP(2),0(R1)                                                
         L     R6,AIO                                                           
*                                                                               
         DROP  R6,R2,R4                                                         
*                                                                               
PGRPX    XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY2                                                 
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
*                                                                               
*-----------------------------------------------------------*                   
* R1 MUST POINT TO MARKET                                                       
* - QMKT CONTAINS MARKET (4 CHARACTERS)                                         
* - MKTNM CONTAINS MARKET NAME                                                  
*-----------------------------------------------------------*                   
GETMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0000000008040000'                                  
         EDIT  (2,0(R1)),QMKT                                                   
         MVC   TEMPFLD+8(4),QMKT                                                
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,3                                                       
         GOTO1 VALIMKT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
         J     XIT                                                              
*                                                                               
*                                                                               
*-----------------------------------------------------------*                   
* R0 LOWEST BYTE MUST INDUCATE INPUT 01=LETTER, 02=ELCODE, 03=MEDIA             
* BYTE MUST CONTAIN ELEMENT CODE/MEDIA LETTER                                   
* ON EXIT BYTE CONTAINS MEDIA LETTER/ELCODE                                     
* RF ADDRESSES LAST ACCESSED LINE OF THE TABLE                                  
*-----------------------------------------------------------*                   
GETMED   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,MEDTAB                                                        
         BCTR  R0,R0                                                            
         LR    R3,R0                                                            
*                                                                               
         CHI   R0,0                SEE IF  LETTER->ELCODE                       
         BE    GET10                                                            
         NI    BYTE,X'0F'                                                       
*                                                                               
GET10    DS    0H                                                               
         LA    R1,0(RF,R3)                                                      
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    GETNO                                                            
         CLC   BYTE,0(R1)          OUR BYTE?                                    
         BE    GETYES                                                           
         LA    RF,MEDTABLQ(RF)     ADVANCE IN TABLE                             
         B     GET10                                                            
*                                                                               
GETNO    SR    RC,RC                                                            
GETYES   LTR   RC,RC                                                            
         XIT1  REGS=(RF)                                                        
*                                                                               
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'02',XL1'01'                                           
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'03',XL1'02'                                           
         DC    CL1'X',XL1'04',XL1'04'                                           
         DC    CL1'N',XL1'05',XL1'03'                                           
         DC    X'FF'                                                            
         LTORG                                                                  
MEDLTRQ  EQU   X'01'                                                            
MEDELCDQ EQU   X'02'                                                            
MEDMDCDQ EQU   X'03'                                                            
*                                                                               
*                                                                               
*-----------------------------------------------------------*                   
* FILTER RECORDS BY SUPERVISOR                                                  
* AIO MUST POINT TO BUYER RECORD                                                
*-----------------------------------------------------------*                   
FLTSUP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING BYRNAMED,R6                                                      
*                                                                               
         BRAS  RE,GETEL                                                         
         CLC   BYRSPV,SPFILTER                                                  
         JNE   XNO                                                              
         DROP  R6                                                               
*                                                                               
FLTSX    J     XYES                                                             
*                                                                               
*                                                                               
*-----------------------------------------------------------*                   
* FILTER RECORDS BY OFFICE CODE                                                 
* AIO MUST POINT TO BUYER RECORD                                                
*-----------------------------------------------------------*                   
FLTOFF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING BYRNAMED,R6                                                      
*                                                                               
         BRAS  RE,GETEL                                                         
         CLC   BYROFF,OFFILTER                                                  
         JNE   XNO                                                              
*                                                                               
FLTOX    J     XYES                                                             
*                                                                               
*-----------------------------------------------------------*                   
* AIO MUST POINT TO BUYER RECORD                                                
*-----------------------------------------------------------*                   
FLTMKT   NTR1  BASE=*,LABEL=*                                                   
         MVI   XFLAG,C'Y'                                                       
         MVI   MKTFLAG,0                                                        
*                                                                               
* PART I, DETERMINING IF WE HAVE MARKETS/GROUPS AND SAVING THEM                 
*                                                                               
         XCEFL MKBUF,'MKBUFLQ'     CLEAR MARKET BUFFER                          
         LA    R2,MKBUF                                                         
*                                                                               
         L     R6,AIO              ELCODE IS SET ALREADY                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
FLTM10   BRAS  RE,NEXTEL                                                        
         BNE   FLTM50                                                           
*                                                                               
         CLI   0(R6),X'02'         LOWER THAN X'02' (TV)?                       
         BL    FLTM10              GET NEXT ELEMENT                             
         CLI   0(R6),X'04'         HIGHER THAN X'04' ?                          
         JH    FLTM20              SEE IF WE'VE GOT MKT GROUPS                  
* HERE WE'VE GOT MARKET ELEMENT                                                 
         OI    MKTFLAG,MKTFMKQ     INDICATE WE HAVE MARKETS                     
         MVC   1(2,R2),2(R6)       SAVE MARKET IN TABLE                         
         LA    R2,4(R2)                                                         
         B     FLTM10              GET NEXT ELEMENT                             
*                                                                               
FLTM20   DS    0H                                                               
         CLI   0(R6),X'12'         LOWER THAN X'12' (TV)?                       
         BL    FLTM10              IF LOWER - NEXT ELEMENT                      
         CLI   0(R6),X'15'         HIGHETR THAN X'15'(NET)                      
         JH    FLTM50              NO MORE MARKET GROUPS                        
* HERE WE'VE GOT MARKET GROUP ELEMENT                                           
         OI    MKTFLAG,MKTFMGQ     INDICATE WE HAVE MKT GROUPS                  
         MVC   0(3,R2),2(R6)       SAVE MARKET GROUP                            
         MVI   3(R2),X'01'         TV MEDIA                                     
         CLI   0(R6),X'13'         MEDIA R MARKET?                              
         BNE   *+12                NOPE                                         
         MVI   3(R2),X'02'         YES, INDICATE IN MKBUF                       
         B     FLTM25                                                           
         CLI   0(R6),X'14'         MEDIA X MARKET?                              
         BNE   *+12                NOPE                                         
         MVI   3(R2),X'04'         YES, INDICATE IN MKBUF                       
         B     FLTM25                                                           
         CLI   0(R6),X'15'         MEDIA N MARKET?                              
         BNE   FLTM25              NOPE                                         
         MVI   3(R2),X'03'         YES, INDICATE IN MKBUF                       
*                                                                               
FLTM25   LA    R2,4(R2)                                                         
         B     FLTM10                                                           
*                                                                               
FLTM50   DS    0H                                                               
*                                                                               
* PART II - MKTS/GRPS SAVED, FILTERING STARTS                                   
*                                                                               
         MVI   0(R2),X'FF'         MARK END OF TABLE                            
         TM    MKTFLAG,MKTFMKQ+MKTFMGQ                                          
         BNO   *+6           IF WE HAVE BOTH MKT GROUPS AND MARKETS,            
         DC    H'0'                %\V                                          
*                                                                               
         TM    FLTFLAG,MGFLTQ      FILTERING BY MARKET GROUPS?                  
         BNO   FLTM80                                                           
*                                 HERE WE'RE FILTERING BY MARKET GROUPS         
         TM    MKTFLAG,MKTFMKQ                                                  
         JO    XNO                 IF RECORD HAS MARKETS - NO MATCH             
*                                                                               
         LA    R2,MKBUF                                                         
FLTM60   DS    0H                                                               
         CLI   0(R2),X'FF'         END OF TABLE?                                
         JE    XNO                                                              
         CLC   MGFILTER,0(R2)      MK GROUP MATCHES OUR FILTER?                 
         JE    XYES                                                             
         LA    R2,4(R2)            ADVANCE AHEAD IN THE TABLE                   
         B     FLTM60                                                           
*                                                                               
FLTM80   DS    0H                  HERE WE'RE FILTERING BY MARKETS              
         TM    MKTFLAG,MKTFMKQ     DOES RECORD HAVE MARKETS?                    
         BNO   FLTM100             IF NO-NEED TO READ MKT ASSIGN RECS           
*                                                                               
         LA    R2,MKBUF            IF YES - JUST RUN THROUGH THE TABLE          
FLTM90   DS    0H                                                               
         CLI   0(R2),X'FF'         END OF TABLE?                                
         JE    XNO                                                              
*                                                                               
         CLC   1(2,R2),=X'FFFF'      ALL MARKETS?                               
         JE    XYES                AUTOMATIC MATCH                              
*                                                                               
         CLC   MKFILTER,1(R2)      MKARKET MATCHES OUR FILTER?                  
         JE    XYES                                                             
         LA    R2,4(R2)            ADVANCE AHEAD IN THE TABLE                   
         B     FLTM90                                                           
*                                                                               
FLTM100  DS    0H                  WE NEED TO READ MKT ASSGN RECORDS            
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING MKARECD,R6                                                       
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,SAVEKEY+2  AGENCY                                       
         OC    MKAKAGMD,MKBUF+3    MEDIA                                        
         MVC   MKAKMKT,MKFILTER    MARKET                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FLTMXNO             NOPE                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 GETREC              GETREC                                       
         BRAS  RE,CHKBUF                                                        
         BE    FLTMX                                                            
*                                                                               
FLTMXNO  MVI   XFLAG,C'N'                                                       
FLTMX    XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         CLI   XFLAG,C'Y'                                                       
         JE    XYES                                                             
         J     XNO                                                              
*                                                                               
XFLAG    DS    C                                                                
         LTORG                                                                  
*                                                                               
* * * * * * * *                                                                 
* CHECKS MARKET GROUPS (IF ANY) FROM MKT ASSGN RECORD AGAINST THOSE             
* SAVED IN MKBUF                                                                
* AIO IS EXPECTED TO ADDRESS MKT ASSGN RECORD, MKBUF IS TO END W X'FF'          
* * * * * * * *                                                                 
CHKBUF   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CHKB10   BRAS  RE,NEXTEL                                                        
         JNE   XNO                 IF NO MORE ELEMENTS, EXIT                    
*                                                                               
         LA    R2,MKBUF                                                         
CHKB20   DS    0H                                                               
         CLI   0(R2),X'FF'         END OF BUFFER?                               
         BE    CHKB10         THIS GROUP NOT IN BUFFER, GET NEXT MKG EL         
         CLC   5(3,R6),0(R2)       COMPARE AGAINST OUR SAVED BUFFER             
         JE    XYES                YES                                          
*                                                                               
CHKB30   LA    R2,4(R2)            ADVANCE IN MKG BUFFER                        
         B     CHKB20                                                           
*                                                                               
*-----------------------------------------------------------*                   
* AIO MUST POINT TO BUYER RECORD                                                
*-----------------------------------------------------------*                   
FLTMED   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)           A(FIRST ELEMENT)                             
*                                                                               
FLMD10   DS    0H                                                               
         CLI   0(R6),X'00'         END OF RECORD?                               
         JE    XNO                                                              
*                                                                               
         CLI   0(R6),X'02'         LOWER THAN X'02' (TV)?                       
         BL    FLMD20              GET NEXT ELEMENT                             
         CLI   0(R6),X'04'         HIGHER THAN X'04' ?                          
         JNH   FLMD30              SEE IF WE'VE GOT MKT GROUPS                  
*                                                                               
         CLI   0(R6),X'12'         LOWER THAN X'12' (TV)?                       
         BL    FLMD20              IF LOWER - NEXT ELEMENT                      
         CLI   0(R6),X'15'         HIGHETR THAN X'15'(NET)                      
         JNH   FLMD30              NO MORE MARKET GROUPS                        
*                                                                               
FLMD20   DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0               ADVANCE TO NEXT ELEMENT                      
         B     FLMD10                                                           
*                                                                               
FLMD30   DS    0H                                                               
         MVC   BYTE,0(R6)                                                       
         NI    BYTE,X'0F'                                                       
         CLC   MDFILTER,BYTE                                                    
         JNE   FLMD20             IF NOT-CHECK NEXT ELEMENT                     
         J     XYES                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* FIRST BYTE=0, THEN IT IS A MARKET,IF NONZERO-MKT GROUP                        
MKBUF    DS    300X                                                             
MKBUFLQ  EQU   *-MKBUF                                                          
MKBUFX   DC    X'FF'                                                            
*                                                                               
*                                                                               
*-----------------------------------------------------------*                   
* VOPT - VALIDATE OPTIONS FIELD                                                 
*        R5 = A(ELEM)                                                           
*-----------------------------------------------------------*                   
         USING BYRNAMED,R5                                                      
VOPT     NTR1  BASE=*,LABEL=*                                                   
         XC    SCANAREA,SCANAREA   NOTE ROOM FOR 8 SCAN LINES                   
         GOTO1 SCANNER,DMCB,(R2),SCANAREA                                       
         LLC   R4,DMCB+4           NUMBER OF ENTRIES                            
*                                                                               
         LA    R6,SCANAREA                                                      
VOPT05   CLC   =C'CABLE',12(R6)                                                 
         BNE   VOPT10                                                           
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    BYROPT1,BYROPT1_CBL                                              
         B     VOPT99                                                           
         CLI   22(R6),C'N'                                                      
         JNE   ERRINV                                                           
         NI    BYROPT1,X'FF'-BYROPT1_CBL                                        
         B     VOPT99                                                           
*                                                                               
VOPT10   CLC   =C'UNWIRED',12(R6)                                               
         BNE   VOPT20                                                           
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    BYROPT1,BYROPT1_UNW                                              
         B     VOPT99                                                           
         CLI   22(R6),C'N'                                                      
         JNE   ERRINV                                                           
         NI    BYROPT1,X'FF'-BYROPT1_UNW                                        
         B     VOPT99                                                           
*                                                                               
VOPT20   CLC   =C'ASST',12(R6)                                                  
         BNE   VOPT30                                                           
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    BYROPT1,BYROPT1_ASS                                              
         B     VOPT99                                                           
         CLI   22(R6),C'N'                                                      
         JNE   ERRINV                                                           
         NI    BYROPT1,X'FF'-BYROPT1_ASS                                        
         B     VOPT99                                                           
*                                                                               
VOPT30   CLI   12(R6),C'D'                                                      
         JNE   ERRINV                                                           
         TM    3(R6),X'80'         NUMERIC FIELD?                               
         BZ    ERRINV              NO, ERROR                                    
         ICM   R1,15,8(R6)         BINARY VALUE                                 
         BZ    ERRINV              IF ZERO ERROR                                
         C     R1,=F'90'           > 90?                                        
         BH    ERRINV              YES, ERROR                                   
         CLI   DAYS,0              HAVE THIS FILTER ALREADY?                    
         BNE   ERRINV              YES, ERROR                                   
         STC   R1,DAYS             STORE THE DAYS                               
*                                                                               
VOPT99   LA    R6,32(R6)                                                        
         BCT   R4,VOPT05                                                        
         J     XIT                                                              
         DROP  R5,RB                                                            
         EJECT                                                                  
*-----------------------------------------------------------*                   
* VRMG - VALIDATE MARKET GROUPS                                                 
*-----------------------------------------------------------*                   
*                                                                               
VRMG     NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO             CHK IF REC HAS ANY MKT ELEMS                  
         MVC   ELCODE,MEDELCD                                                   
         BRAS  RE,GETEL                                                         
         JE    ERRNOMIX            CAN'T ADD/DEL MKT GR IF REC HAS MKT          
*                                                                               
         LA    R5,DELMGS                                                        
         LA    R3,ADDMGS                                                        
*                                                                               
VRMG11   CLI   5(R2),0             ANY INPUT?                                   
         BE    VRMGX                                                            
         LA    R1,8(R2)                                                         
         LLC   RE,5(R2)            LEN                                          
         CLI   8(R2),C'-'          MINUS MKT GROUP                              
         BNE   VRMG12                                                           
         CLI   5(R2),2                                                          
         JL    ERRINV                                                           
         AHI   RE,-1               MINUS 1 FOR MINUS SIGN = LEN                 
         LA    R1,1(R1)            AND POINT TO MKT GRP ID                      
         B     VRMG15                                                           
*                                                                               
VRMG12   CLI   5(R2),2                                                          
         JL    ERRINV                                                           
*                                                                               
VRMG15   CLI   0(R1),C'A'                                                       
         JL    ERRINV              INVALID MKT GROUP                            
         MVC   FULL(1),0(R1)                                                    
         MVI   FULL+1,C' '                                                      
         LA    R1,1(R1)                                                         
         BCTR  RE,0                MINUS 1 FOR MKT GRP ID                       
         CLI   0(R1),C'Z'          2-CHAR MGROUP?                               
         BH    VRMG15A             NO                                           
         MVC   FULL+1(1),0(R1)                                                  
         LA    R1,1(R1)                                                         
         BCTR  RE,0                MINUS 1 FOR MKT GRP ID                       
*                                                                               
VRMG15A  LA    R4,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
VRMG16   CLC   FULL(2),0(R4)                                                    
         BE    VRMG16A                                                          
         LA    R4,3(R4)                                                         
         BCT   RF,VRMG16                                                        
         B     ERRINV                                                           
*                                                                               
VRMG16A  MVC   MGRPID,2(R4)                                                     
*                                                                               
VRMG18   CLI   0(R1),C'0'          TESTS IF MARKET NUMERIC                      
         JL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         JH    ERRINV                                                           
         LA    R1,1(R1)            CHECK NEXT CHARACTER                         
         BCT   RE,VRMG18           CHECKS EACH INPUT CHARACTER                  
*                                                                               
         LA    R1,8(R2)                                                         
         LLC   RE,5(R2)                                                         
         CLI   8(R2),C'-'                                                       
         BNE   *+10                                                             
         BCTR  RE,0                GET RID OF '-'                               
         LA    R1,1(R1)                                                         
*                                                                               
         BCTR  RE,0                BUMP PAST MKT GRP ID                         
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'Z'          2-CHAR MGROUP?                               
         BH    *+10                NO                                           
         BCTR  RE,0                BUMP PAST MKT GRP ID                         
         LA    R1,1(R1)                                                         
*                                                                               
         MVC   WORK(4),=C'0000'                                                 
         BCTR  RE,0                BCTR FOR EXECUTED INSTR                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R1)            MARKET NUMBER IS PACKED BINARY          
         XC    DUB,DUB                                                          
         PACK  DUB(3),WORK(5)                                                   
         MVC   MGRPNO,DUB                                                       
         OC    MGRPNO,MGRPNO            CAN'T BE MARKET DEFINITION              
         JZ    ERRINV                                                           
*                                                                               
VRMG20   LA    R4,KEY                   BUILD MARKET GROUP KEY                  
         USING MKGRECD,R4               TO CHK IF ENTRY IS VALID                
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD                                                  
         XC    MKGKCLT,MKGKCLT          BINARY CLIENT CODE                      
         XC    MKGKPID,MKGKPID                                                  
         XC    MKGKPGRP,MKGKPGRP                                                
         MVC   MKGKMID,MGRPID                                                   
         MVC   MKGKMGRP,MGRPNO                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   8(R2),C'-'               MINUS SIGN?                             
         BNE   VRMG50                   NO, GOTO ADD MKT LOGIC                  
*                                                                               
         CLC   KEY(13),KEYSAVE          IF MARKET DEFIN DOESN'T EXIST           
         JNE   VRMG32                   DON'T NEED TO CHECK ADDS                
*                                                                               
         LA    R1,ADDMGS               YES, DELETE MKT LOGIC                    
         LA    R0,ADMGEND                                                       
VRMG30   DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   MGRPID(3),0(R1)          WON'T LET YOU DELETE A CLIENT           
         JE    ERRDUP                   IF YOU ADDED IT IN A PREVIOUS           
         LA    R1,L'ADDMGS(R1)         "ADD CLIENT" FIELD                       
         CR    R1,R0                                                            
         BL    VRMG30                                                           
*                                                                               
VRMG32   MVC   0(L'DELMGS,R5),MGRPID    ENTRY OK, STORE IN DEL TABLE            
         BRAS  RE,DELMG                                                         
         LA    R5,L'DELMGS(R5)         PT. TO NEXT AVAIL. DEL STORE             
         B     VRMG90                                                           
*                                                                               
VRMG50   DS    0H                  ADDING A MARKET GROUP                        
*                                                                               
         CLC   KEY(13),KEYSAVE          IF MARKET DEFIN DOESN'T EXIST           
         JNE   ERRINV                   MGROUP NOT VALID                        
*                                                                               
         LA    R1,DELMGS               ADD MKT GROUPS LOGIC                     
         LA    R0,DEMGEND                                                       
VRMG60   DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   MGRPID(3),0(R1)          WON'T LET YOU ADD A CLIENT              
         JE    ERRDUP                   IF YOU DELETED IT IN A PREVIOUS         
         LA    R1,L'DELMGS(R1)         "ADD CLIENT" FIELD                       
         CR    R1,R0                                                            
         BL    VRMG60                                                           
*                                                                               
         MVC   0(L'ADDMGS,R3),MGRPID                                            
         BRAS  RE,ADDMG                                                         
         LA    R3,L'ADDMGS(R3)         BUMP THROUGH STORAGE TABLES              
*                                                                               
VRMG90   BAS   RE,NXTSCRF               PT. TO NEXT "ADD MARKETS" ENTRY         
         LA    R0,BYMLMKH                                                       
         CR    R2,R0                    PAST LAST MKT FIELD?                    
         BNH   VRMG11                   NO, CHECK NEXT FIELD                    
*                                                                               
VRMGX    J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO ADD MKT GROUP ****                                           
*---------------------------------------------------------------------          
ADDMG    NTR1  BASE=*,LABEL=*                                                   
         USING BYRMKGD,R6                                                       
*                                                                               
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
AMG10    BRAS  RE,NEXTEL                                                        
         BNE   AMG20                                                            
         CLC   BYRMKGID,MGRPID                                                  
         BNE   AMG12                                                            
         CLC   BYRMKGRP,MGRPNO                                                  
         JE    ERRDUP              MARKET GROUP ALREADY EXISTS ON REC           
AMG12    B     AMG10                                                            
*                                                                               
AMG20    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         MVC   BYRMKGEL,MEDELCD    ELEMENT CODE                                 
         OI    BYRMKGEL,X'10'                                                   
         MVI   BYRMKGLN,5          ELEMENT LENGTH                               
         MVC   BYRMKGID,MGRPID                                                  
         MVC   BYRMKGRP,MGRPNO                                                  
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
AMGX     J     XIT                                                              
         DROP  R6,RB                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO DELETE MKT GROUP ****                                        
*---------------------------------------------------------------------          
DELMG    NTR1  BASE=*,LABEL=*                                                   
         USING BYRMKGD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DELMG10  BRAS  RE,NEXTEL                                                        
         JNE   ERRNOMKT            <====                                        
         CLC   BYRMKGID,MGRPID                                                  
         BNE   DELMG10                                                          
         CLC   BYRMKGRP,MGRPNO                                                  
         BNE   DELMG10                                                          
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
DELMGX   J     XIT                                                              
         DROP  R6,RB                                                            
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------*                   
* RESTORE - CHECK BUBL PASSIVE DOESN'T EXIST                                    
*-----------------------------------------------------------*                   
         SPACE                                                                  
         USING BYRBBLD,R6                                                       
RESTORE  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,BYRBBLQ      BUYER/BILLER NAME ELEM                       
         BRAS  RE,GETEL                                                         
         BNE   RESTX                                                            
         MVC   SAVEKEY,KEY                                                      
         LA    R6,BYRBBLNM                                                      
         BRAS  RE,CHKBUBL          MAKE SURE IT DOESN'T EXIST                   
         MVC   KEY,SAVEKEY                                                      
         OI    DMINBTS,X'08'       READ DELETED RECORDS!                        
         GOTO1 HIGH                                                             
*                                                                               
RESTX    J     XIT                                                              
         DROP  R6,RB                                                            
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------*                   
* DISPBUBL - DISPLAY BUYER/BILLER NAME                                          
*-----------------------------------------------------------*                   
         SPACE                                                                  
         USING BYRBBLD,R6                                                       
DISPBUBL NTR1  BASE=*,LABEL=*                                                   
         MVC   BYMBYR,SPACES                                                    
         MVI   BYMBYRH+7,10        SET OUTPUT LEN                               
         OI    BYMBYRH+6,X'80'     TRANSMIT                                     
         L     R6,AIO1                                                          
         MVI   ELCODE,BYRBBLQ      BUYER/BILLER NAME ELEM                       
         BRAS  RE,GETEL                                                         
         BNE   DBUBLX                                                           
         MVC   BYMBYR,BYRBBLNM                                                  
*                                                                               
DBUBLX   J     XIT                                                              
         DROP  R6,RB                                                            
*                                                                               
*-----------------------------------------------------------*                   
* ADDBUBL - ADD BUYER/BILLER NAME TO RECORD                                     
*        R2 = A(BUYER FIELD HEADER)                                             
*-----------------------------------------------------------*                   
         SPACE                                                                  
ADDBUBL  NTR1  BASE=*,LABEL=*                                                   
         XC    SVBUBL,SVBUBL                                                    
         USING BYRBBLD,R6                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,BYRBBLQ      BUYER/BILLER NAME ELEM                       
         BRAS  RE,GETEL                                                         
         BNE   ABUBL10             ADD NEW BUBL ELEM IF NONE                    
         MVC   SVBUBL,BYRBBLNM                                                  
         MVC   BYRBBLNM,8(R2)      CHANGE BUBL NAME, NOT CODE                   
         OC    BYRBBLNM,SPACES                                                  
         CLC   SVBUBL,BYRBBLNM     IF BUBL UNCHANGED, JUST EXIT                 
         BE    ABUBLX                                                           
         CLC   BYRBBLNM,SPACES     JUST DELETING BUBL NAME?                     
         BNH   ABUBLX               YES - DON'T CHECK                           
         LA    R6,BYRBBLNM                                                      
         BRAS  RE,CHKBUBL          OTHERWISE, CHECK IF IT'S VALID               
         B     ABUBLX                                                           
         DROP  R6                                                               
*                                                                               
ABUBL10  CLI   5(R2),0             ANY INPUT?                                   
         BE    ABUBLX               NO - GET OUT                                
         MVC   WORK(10),8(R2)      CHECK UNIQUE BUBL NAME                       
         OC    WORK(10),SPACES                                                  
         LA    R6,WORK                                                          
         BRAS  RE,CHKBUBL                                                       
*                                                                               
         XC    KEY,KEY             READ AGENCY LEVEL BUYER REC TO GET           
         MVC   AIO,AIO2             NEXT UNIQUE BUBL ID#                        
         LA    R4,KEY                                                           
         USING BYRRECD,R4                                                       
         MVC   BYRKTYP,=XL2'0D62'                                               
         MVC   BYRKAGY,AGYX                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(BYRLEN-BYRKEY),KEYSAVE                                       
         BE    ABUBL20             AGY LEVEL REC FOUND, UPDATE COUNTER          
*   BUILD NEW AGY LEVEL BYR REC                                                 
         L     R0,AIO              CLEAR NEW BUY AREA                           
         LHI   R1,LIOS                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO                                                           
         MVC   KEY,KEYSAVE                                                      
         MVC   BYRKEY,KEYSAVE                                                   
         MVC   BYRLEN,=Y(BYREL-BYRKEY)                                          
*                                                                               
         USING BYRBBLD,R6                                                       
         LA    R6,ELEM             AGY LEVEL REC ONLY HAS BUBL ELEM             
         XC    ELEM,ELEM                                                        
         MVI   BYRBBLEL,BYRBBLQ                                                 
         MVI   BYRBBLLN,BYRBBLNQ                                                
         LHI   R0,1                INITIALIZE HIGH UNIQUE BUBL CODE             
         STCM  R0,3,BYRBBLCD                                                    
         MVC   BYRBBLNM,=CL10' '   AGENCY LEVEL REC GETS SPACES                 
         OC    BYRBBLNM,SPACES                                                  
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         MVC   AIO,AIO1                                                         
         B     ABUBL30                                                          
         DROP  R4,R6                                                            
*                                                                               
ABUBL20  MVI   RDUPDATE,C'Y'       GET EXISTING AGY LEVEL REC                   
         GOTO1 GETREC                                                           
*                                                                               
         USING BYRBBLD,R6                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,BYRBBLQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DCHO                                                                   
         XR    R0,R0                                                            
         ICM   R0,3,BYRBBLCD       CURRENT HIGH BUBL CODE                       
         AHI   R0,1                INCREMENT                                    
         CLM   R0,3,=X'FFFF'       I DON'T THINK THIS CAN REALLY HAPPEN         
         BL    *+6                                                              
         DCHO                                                                   
         STCM  R0,3,BYRBBLCD                                                    
         DROP  R6                                                               
         GOTO1 PUTREC                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         USING BYRBBLD,R6                                                       
ABUBL30  LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   BYRBBLEL,BYRBBLQ                                                 
         MVI   BYRBBLLN,BYRBBLNQ                                                
         STCM  R0,3,BYRBBLCD                                                    
         MVC   BYRBBLNM,8(R2)                                                   
         OC    BYRBBLNM,SPACES                                                  
         DROP  R6                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
*                                                                               
ABUBLX   J     XIT                                                              
         DROP  RB                                                               
         SPACE                                                                  
*-----------------------------------------------------------*                   
* CHKBUBL - MAKE SURE BUBL NAME DOESN'T EXIST                                   
*        R6 = A(10 CHAR BUBL NAME - BLANK PADDED)                               
*-----------------------------------------------------------*                   
         SPACE                                                                  
CHKBUBL  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             MAKE SURE PASSIVE BUBL DOESN'T EXIST         
         LA    R4,KEY                                                           
         USING BYRRECD,R4                                                       
         MVC   BYRPTYP3,=X'0DE4'                                                
         MVC   BYRPAGY3,AGYX                                                    
         MVC   BYRPBUBL,0(R6)                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS3),KEYSAVE  PASSIVE POINTER FOUND?                  
         BNE   CHKBUBLX                  NO - OK                                
         GOTO1 GETREC                   BE NICE AND TELL USER WHERE             
         L     R4,AIO                                                           
         MVC   ERRNUM,=Y(DUPBYR)                                                
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GTASUBST       A(SUBST TEXT)                                
         MVI   ELEM,3              L'SUBST TEXT + 1                             
         MVC   ELEM+1(2),BYRKOFC                                                
         MVI   ELEM+3,5                                                         
         MVC   ELEM+4(4),BYRKBYR                                                
         J     SPERREX                                                          
*                                                                               
CHKBUBLX J     XIT                                                              
         DROP  R4,RB                                                            
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
* ++INCLUDES AND DSECTS                                                         
*                                                                               
*                                                                               
PLINED   DSECT                                                                  
PLINE1   DS    0X                                                               
PLBGRP   DS    CL3                                                              
         DS    (SPC)C                                                           
*                                                                               
PLINE2   DS    0X                                                               
PLBCODE  DS    CL4                                                              
         DS    (SPC)C                                                           
*                                                                               
PLINE3   DS    0X                                                               
PLFNAME  DS    CL10                                                             
         DS    (SPC)C                                                           
PLLNAME  DS    CL14                                                             
         DS    (SPC)C                                                           
PLASSIST DS    CL2                                                              
         DS    (SPC)C                                                           
PLOFFICE DS    CL3                                                              
         DS    (SPC)C                                                           
PLFILTER DS    CL3                                                              
         DS    (SPC)C                                                           
*LSUPER  DS    CL4                                                              
*        DS    (SPC)C                                                           
PLSPNAME DS    CL24                                                             
         DS    (SPC)C                                                           
*                                                                               
PLMKLIST DS    0X                                                               
PLMEDIA  DS    CL1                                                              
         DS    (SPC)C                                                           
PLMKTGRP DS    CL6                                                              
         DS    (SPC)C                                                           
PLMKT    DS    CL4                                                              
         DS    (SPC)C                                                           
PLMKTNAM DS    CL20                                                             
         DS    (SPC)C                                                           
PLMK2    DS    CL4                                                              
         DS    (SPC)C                                                           
PLMK2NAM DS    CL20                                                             
PLINEEND DS    0X                                                               
SPC      EQU   1                                                                
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME8D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME9D          BUYER ACTION TRANSFER SCREEN                 
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF8D          LIST SCREEN                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM58D          REPORT SCREEN                                
         EJECT                                                                  
       ++INCLUDE DDGENTWA          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE SPGENBYR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENOFC                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*PREFIX=N                                                                       
       ++INCLUDE SPNWSBYR                                                       
         EJECT                                                                  
*PREFIX=D                                                                       
       ++INCLUDE SPADBUYER                                                      
         EJECT                                                                  
*PREFIX=                                                                        
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
DWORD    DS    D                                                                
RELO     DS    A                   RELOCATION FACTOR                            
OFFICE   DS    CL2                                                              
BUYR     DS    CL4                      SAVED BUYER CODE                        
BUYR2    DS    CL4                      SAVED NEW BUYER CODE                    
BUYRVAL  DS    CL4                      SAVED NEW BUYER CODE                    
SUPV     DS    CL4                      STORED SUPV CODE                        
OLDSPV   DS    CL4                                                              
SAVENWS  DS    CL3                                                              
SAVEDAR  DS    CL3                                                              
STARTMKT DS    XL2                                                              
LASTMKT  DS    XL2                                                              
FRSTMKT  DS    XL2                                                              
STARTMKG DS    XL3                                                              
LASTMKG  DS    XL3                                                              
FRSTMKG  DS    XL3                                                              
SAVEKEY  DS    CL13                                                             
SAVEKEY2 DS    CL13                                                             
SAVEKEY3 DS    CL48                                                             
MEDELCD  DS    XL1                                                              
TEMPFLD  DS    XL12                     8 BYTE HEADER + 4 MKT CODE              
ERRNUM   DS    XL2                                                              
BMKT2    DS    XL2                                                              
         DS    0H                       FORCE ALIGNMENT FOR MKT STOR            
ADDMKTS  DS    7XL2                     TABLE STORAGE FOR ADDED MKTS            
ADMKEND  DS    0H                                                               
ADMKLNEQ EQU   ADMKEND-ADDMKTS                                                  
DELMKTS  DS    7XL2                     TABLE STORAGE FOR DELETED MKTS          
DEMKEND  DS    0H                                                               
DEMKLNEQ EQU   DEMKEND-DELMKTS                                                  
DELMGS   DS    7XL3                     TABLE STORAGE FOR DELETED MGS           
DEMGEND  DS    0H                                                               
DEMGLNEQ EQU   DEMGEND-DELMGS                                                   
ADDMGS   DS    7XL3                     TABLE STORAGE FOR ADDED MGS             
ADMGEND  DS    0H                                                               
ADMGLNEQ EQU   ADMGEND-ADDMGS                                                   
AGYX     DS    XL1                      AGENCY HEX, MEDIA=0                     
BAGYMDS  DS    XL1                      AGENCY/MEDIA CODE FOR SUBROUT'S         
STATUS   DS    XL1                                                              
KEYCHG   EQU   X'80'                                                            
DISKADD  DS    XL4                                                              
SAVESEL  DS    CL1                                                              
SVKEY1   DS    CL48                                                             
MEDIATBL DS    CL20                     LIST OF MEDIA IN ORDER OF LIST          
SAVEPTR  DS    F                        POINTER TO THE MEDIATABLE               
SVELCD   DS    X                        SAVE THE ELCODE                         
SVLISTAR DS    CL80                     BACK UP OF PREVIOUS LISTLINE            
SCANAREA DS    CL256                    AREA USED FOR SCANNER                   
COLUMN   DS    X                                                                
MKT      DS    CL4                                                              
*                                                                               
FLTFLAG  DS    XL1                                                              
BGFLTQ   EQU   X'80'                    BUYGRP FILTER                           
MKFLTQ   EQU   X'40'                    MARKET FILTER                           
SPFLTQ   EQU   X'20'                    SUPERVISOR FILTER                       
OFFLTQ   EQU   X'10'                    OFFICE FILTER                           
BCFLTQ   EQU   X'08'                    BUYER INITIALS                          
MDFLTQ   EQU   X'04'                    MEDIA FILTER                            
MGFLTQ   EQU   X'02'                    MARKET FILTER                           
MAXFLTQ  EQU   3                                                                
*                                                                               
MKTFLAG  DS    XL1                      RECORD HAS GOT:                         
MKTFMKQ  EQU   X'01'                    MARKETS                                 
MKTFMGQ  EQU   X'02'                    MARKET GROUPS                           
*                                                                               
BGFILTER DS    CL2                                                              
MKFILTER DS    CL2                                                              
MGFILTER DS    CL3                      FIRST BYTE - INPUT LENGTH               
SPFILTER DS    CL4                                                              
BCFILTER DS    CL4                                                              
OFFILTER DS    CL2                                                              
MDFILTER DS    CL1                                                              
*                                                                               
PIDNAME  DS    CL8                                                              
PIDNUM   DS    XL2                                                              
SECALPHA DS    CL2                                                              
DISPED   DS    CL1                                                              
KEY2     DS    CL50                                                             
ADMGPTR  DS    F                                                                
DELMGPTR DS    F                                                                
MGNAME   DS    CL24                                                             
SVBUBL   DS    CL10                                                             
DAYS     DS    CL1                                                              
SDPROF   DS    CL16                                                             
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
**** ONLINE LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSOFFCD  DS    CL2                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL5                                                              
LSBYRCD  DS    CL4                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL3                                                              
LSBYRNM  DS    CL21                                                             
         DS    CL2                                                              
LSBYRMED DS    CL1                 MEDIA WITH MARKETS                           
         DS    CL4                                                              
LSBYRFIL DS    CL1                 FILTER                                       
         DS    CL3                                                              
LSBYRSPV DS    CL4                 SUPERVISOR NAME (IN E-MAIL FORM)             
         DS    CL2                                                              
LSBYRSNM DS    CL19                SUPERVISOR NAME                              
         EJECT                                                                  
*                                                                               
DMKTEL   DSECT                            FOR DISPLAYING A MARKET LIST          
DMKTHD   DS    CL8                      OUTPUT FIELD HEADER                     
DMKTCD   DS    CL4                      MARKET CODE                             
         DS    CL2                                                              
DMKTNM   DS    CL17                     MARKET NAME                             
MKDLQ    EQU   *-DMKTHD                 LENGTH                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039SPSFM4C   05/20/10'                                      
         END                                                                    
