*          DATA SET NESFM15S   AT LEVEL 063 AS OF 05/01/02                      
*PHASE T31C15A                                                                  
*INCLUDE MSPACK                                                                 
*INCLUDE MSUNPK                                                                 
         TITLE 'T31C15  NETWORK PRODUCT/INDEX MAINTENANCE'                      
T31C15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C15,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,1(RB)            R7-2ND BASE REGISTER                         
         LA    R7,4095(R7)                                                      
         USING T31C15,RB,R7                                                     
         ST    R2,RELO                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
* BUILD KEY IN SVKEY / MOVE TO KEY AT EXIT                                      
*                                                                               
         SPACE                                                                  
VK       DS    0H                                                               
         LA    R6,SVKEY                                                         
         USING NINDKEY,R6                                                       
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D01'                                                
         SPACE                                                                  
         LA    R2,WORK                * MEDIA                                   
         MVI   8(R2),C'N'          SET FOR NETWORK                              
         GOTO1 VALIMED                                                          
         MVC   NINDKAGM,BAGYMD                                                  
         SPACE                                                                  
         LA    R2,SFMCLTH            * CLIENT                                   
         GOTO1 VALICLT                                                          
         MVC   NINDKCLT,BCLT                                                    
         SPACE                                                                  
         LA    R2,SFMNETH            * NETWORK                                  
         GOTO1 VALINTWK                                                         
         MVC   MYWORK(4),=C'1234'  FUDGE MKT                                    
         MVC   MYWORK+4(4),QNET                                                 
         MVI   MYWORK+8,C'N'                                                    
         GOTO1 =V(MSPACK),DMCB,MYWORK,MYWORK+4,MYWORK+10,RR=RELO                
         MVC   NINDKNET,MYWORK+12                                               
         SPACE                                                                  
         LA    R2,SFMPROH          *PROGRAM                                     
         CLI   5(R2),0                                                          
         BNE   VK5                                                              
         CLI   ACTNUM,ACTLIST      PROG OPTIONAL IN LIST                        
         BE    VKX                                                              
VK5      GOTO1 VALIFLD                                                          
         XC    FULL+2,FULL+2       FOR CALL TO PROGREC                          
         MVC   MYWORK(6),NFLD                                                   
         BAS   RE,PROGREC                                                       
         BNE   PRGERR                                                           
         MVC   NINDKPRG,NFLD                                                    
         SPACE                                                                  
         LA    R2,SFMDATH            * EFFECTIVE DATE                           
         CLI   5(R2),0                                                          
         BNE   VK12                                                             
         CLI   ACTNUM,ACTLIST            IN LIST EFF DATE IS OPTIONAL           
         BE    VKX                                                              
         GOTO1 VALIFLD                                                          
VK12     GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(2,FULL+2)        COMPRESSED               
         BAS   RE,PROGREC                 CHECK DATE AGAINST PROG REC           
         BNE   PRGERR                                                           
         XC    FULL+2(2),=X'FFFF'                  COMPLEMENT DATE              
         MVC   NINDKEDT,FULL+2                                                  
         SPACE                                                                  
VKX      DS    0H                                                               
         LA    R2,SFMCLTH                                                       
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING NINDKEY,R6                                                       
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,NINDKCLT,SFMCLT   * CLIENT                           
         FOUT  SFMCLTH                                                          
         SPACE                                                                  
         XC    MYWORK,MYWORK                 * NETWORK                          
         MVC   MYWORK+2(3),NINDKNET                                             
         GOTO1 =V(MSUNPK),DMCB,MYWORK,MYWORK+5,MYWORK+10,RR=RELO                
         MVC   SFMNET,MYWORK+10                                                 
         FOUT  SFMNETH                                                          
         SPACE                                                                  
         MVC   SFMPRO,NINDKPRG               * PROGRAM                          
         FOUT  SFMPROH                                                          
         SPACE                                                                  
         DS    0H                           * EFFECTIVE DATE                    
         MVC   FULL(2),NINDKEDT            (IN COMPLEMENT FORM)                 
         XC    FULL(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,FULL),(5,SFMDAT)                                  
         FOUT  SFMDATH                         IN PRINTABLE FORMAT              
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE RECORD                                                               
VR       DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'01'        CLEAR ELEMENTS                               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM                                                          
         LA    RE,PRDLIST                                                       
         LA    RF,WORKEQU                                                       
         XCEF                                                                   
*                                                                               
         LA    R2,SFMTYPEH                                                      
         CLI   5(R2),0             IF TYPE NOT GIVEN                            
         BNE   VR4                                                              
         MVC   SFMTYPE(3),=C'BTI'                                               
         FOUT  (R2)                                                             
*                                                                               
VR4      DS    0H                                                               
         XC    ELEM,ELEM           BUILD CONTROL ELEM                           
         USING NINDCTEL,R6                                                      
         LA    R6,ELEM                                                          
         MVI   NINDCTEL,X'01'                                                   
         MVI   NINDCELN,5                                                       
         CLC   SFMTYPE(3),=C'BTI'                                               
         BE    VR4B                                                             
         CLC   SFMTYPE(3),=C'BRP'                                               
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         OI    NINDCCTL,NINDCBRP   SET DOING BRP'S                              
*                                                                               
VR4B     DS    0H                                                               
         GOTO1 ADDELEM                                                          
         MVC   CNTRL,NINDCCTL      SAVE CONTROL BYTE                            
         DROP  R6                                                               
*                                                                               
         LA    R2,SFMPRD1H                                                      
         MVI   ALLSW,0             SET ALL BRAND SWITCH OFF                     
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         LA    R6,ELEM                                                          
         USING NINDELEM,R6                                                      
*                                                                               
DOELEM   XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'                                                       
         MVI   ELEM+1,5                                                         
         BAS   RE,CHKPRD                 VALIDATE PRODUCT                       
         CLI   MYWORK,X'FF'        TEST POL                                     
         BNE   VR5                                                              
         MVI   ALLSW,C'Y'                                                       
         TM    CNTRL,NINDCBRP      ONLY FOR BRP'S                               
         BO    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VR5      DS    0H                                                               
         MVC   NINDPRD,MYWORK                                                   
*                                                                               
         ZIC   R3,0(R2)                  GET INDEX FIELD                        
         AR    R2,R3                                                            
*                                                                               
         TM    CNTRL,NINDCBRP      IF BRP'S DO DIFFERENT VALIDATION             
         BO    VR6                                                              
*                                  FOR BTI'S                                    
         GOTO1 VALIFLD                                                          
         MVI   ERROR,NOTNUM                                                     
         LTR   R0,R0                                                            
         BZ    TRAPERR             NOT NUMERIC                                  
         MVI   ERROR,INVALID                                                    
         C     R0,=F'255'                                                       
         BH    TRAPERR                                                          
         STC   R0,NINDIND                                                       
         B     VR8                                                              
*                                                                               
VR6      DS    0H                  FOR BRP'S                                    
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(1,8(R2)),(R0)                                      
         CLI   DMCB,0              TEST OK                                      
         BE    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BNP   TRAPERR                                                          
         C     R1,=F'1000'         UPPER LIMIT FOR BRP                          
         BH    TRAPERR                                                          
         STCM  R1,3,NINDBRP                                                     
*                                                                               
VR8      DS    0H                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         CLI   0(R2),9             TEST END OF SCREEN                           
         BH    DOELEM                                                           
*                                                                               
VR10     DS    0H                                                               
         TM    CNTRL,NINDCBRP      IF DOING BRP'S                               
         BZ    VRX                                                              
         CLI   ALLSW,C'Y'          NEED ALL BRAND ENTRY                         
         BNE   ALLERR                                                           
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
********************************************************                        
* CHK 3 BYTE PRODUCT CODE                                                       
*     EXPECTS (R2) TO POINT TO SCREEN FIELD CONTAINING PROD CODE                
*                                                                               
CHKPRD   NTR1                                                                   
         LA    R3,SVCLIST          CHECK CLIST                                  
         GOTO1 VALIFLD                                                          
         CLC   =C'ALL',NFLD        ALL IS OK                                    
         BNE   CP2                                                              
         MVI   MYWORK,X'FF'                                                     
         B     CP6                                                              
CP2      CLC   0(3,R3),NFLD                                                     
         BE    CP5                                                              
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   CP2                                                              
         B     PRDERR                                                           
CP5      MVC   MYWORK(1),3(R3)                                                  
CP6      LA    R3,PRDLIST          CHECK DUPLICATE PRD                          
CP7      CLC   0(3,R3),NFLD                                                     
         BE    DUPRDERR                                                         
         LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   CP7                                                              
         MVC   0(3,R3),NFLD                                                     
CPX      B     EXIT                                                             
         SPACE 2                                                                
********************************************************                        
* READ PROGRAM RECORD TO VALIDATE PROGRAM NAME                                  
*      INPUT:  MYWORK HAS PROG CODE                                             
*              FULL+2 HAS DATE                                                  
PROGREC  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),QNETMKT                                                 
         MVC   KEY+5(6),MYWORK                                                  
         MVC   KEY+11(2),FULL+2                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(11),KEYSAVE                                                  
         XIT1                                                                   
         BNE   PRGERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       DS    0H                        *  CLEAR SCREEN                        
         LA    R2,SFMPRD1H                                                      
DR2      ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         BCTR  R1,0                                                             
         EX    R1,DROC                                                          
         EX    R1,DRCLC                                                         
         BE    DR5                                                              
         EX    R1,DRXC                                                          
         FOUT  (R2)                                                             
         B     DR5                                                              
DROC     OC    8(0,R2),SPACES                                                   
DRCLC    CLC   8(0,R2),SPACES                                                   
DRXC     XC    8(0,R2),8(R2)                                                    
*                                                                               
DR5      ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DR2                                                              
*                                                                               
         LA    R4,=C'BTI'          ASSUME BTI DATA TYPE                         
         MVI   CNTRL,0                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        LOOK FOR CONTROL ELEM                        
         BAS   RE,GETEL                                                         
         BNE   DR6                 NO ELEM MEANS BTI                            
         USING NINDCTEL,R6                                                      
         MVC   CNTRL,NINDCCTL      SAVE CONTROL BYTE                            
         TM    NINDCCTL,NINDCBRP   TEST DATA TYPE                               
         BZ    *+8                                                              
         LA    R4,=C'BRP'                                                       
*                                                                               
DR6      DS    0H                                                               
         DROP  R6                                                               
         MVC   SFMTYPE,SPACES                                                   
         MVC   SFMTYPE(3),0(R4)                                                 
         FOUT  SFMTYPEH                                                         
*                                                                               
         LA    R2,SFMPRD1H                * PRODUCT                             
         L     R6,AIO                                                           
         USING NINDELEM,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DR10     DS    0H                                                               
         MVC   MYWORK(1),NINDPRD                                                
         BAS   RE,GET3PRD                                                       
         MVC   8(3,R2),MYWORK                                                   
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R1,0(R2)                   * INDEX                               
         AR    R2,R1                                                            
         TM    CNTRL,NINDCBRP      IF BRP'S                                     
         BO    DR11                                                             
*                                                                               
         EDIT  (B1,NINDIND),(4,8(R2)),ALIGN=LEFT                                
         B     DR12                                                             
*                                                                               
DR11     DS    0H                                                               
         EDIT  (B2,NINDBRP),(5,8(R2)),1,ALIGN=LEFT                              
*                                                                               
DR12     DS    0H                                                               
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                 TEST END OF SCREEN                       
         BNH   DRX                                                              
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
GET3PRD  NTR1                                                                   
         CLI   MYWORK,X'FF'        POL IS ALL                                   
         BNE   *+14                                                             
         MVC   MYWORK(3),=C'ALL'                                                
         B     EXIT                                                             
         LA    R3,SVCLIST                                                       
GPD2     CLC   3(1,R3),MYWORK                                                   
         BE    GPDX                                                             
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   GPD2                                                             
         MVC   MYWORK(3),=C'***'   NOT FOUND                                    
         B     EXIT                                                             
GPDX     MVC   MYWORK(3),0(R3)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS *                                                                
         SPACE                                                                  
LR       DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR02               ********8 FOR NOW ********                    
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
LR02     MVI   NLISTS,14           SET NUM OF LIST LINES                        
         LA    R3,KEY                                                           
         USING NINDKEY,R3                                                       
         OC    KEY(20),KEY                                                      
         BNZ   *+10                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                                                              
         SPACE                                                                  
LR22     DS    0H                                                               
         CLC   KEY(5),SVKEY               ID/AM/CLIENT                          
         BNE   LRX                                                              
         CLC   SVKEY+5(3),=3X'00'            NETWORK                            
         BE    LR22B                                                            
         CLC   SVKEY+5(3),KEY+5                                                 
         BNE   LR20                                                             
         CLI   SVKEY+8,0                  PROGRAM                               
         BE    LR22B                                                            
         CLC   KEY+8(6),SVKEY+8                                                 
         BNE   LR20                                                             
         CLI   SVKEY+14,0                 DATE                                  
         BE    LR22B                                                            
         CLC   KEY+14(3),SVKEY+14                                               
         BNE   LR20                                                             
LR22B    DS    0H                                                               
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
         LA    R5,LISTAR                                                        
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING NINDKEY,R6                                                       
         MVC   0(6,R5),NINDKPRG                                                 
         MVC   MYWORK(2),NINDKEDT                                               
         BAS   RE,GETPROG                                                       
         MVC   7(16,R5),MYWORK                                                  
         MVC   FULL(2),NINDKEDT                                                 
         XC    FULL(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,FULL),(5,29(5))                                   
*                                                                               
         MVI   CNTRL,0                                                          
         LA    RF,=C'BTI'          SET TO BTI'S                                 
         LA    R2,NINDELS          FIRST ELEM                                   
         CLI   0(R2),X'01'         DO WE HAVE A CONTROL ELEM                    
         BNE   LR24                NO                                           
         MVC   CNTRL,NINDCCTL-NINDCTEL(R2)  YES,  SAVE CONTROL                  
         TM    CNTRL,NINDCBRP       TEST IS BRP'S                               
         BZ    *+8                                                              
         LA    RF,=C'BRP'          SET TO BRP                                   
*                                                                               
LR24     DS    0H                                                               
         MVC   40(3,R5),0(RF)                                                   
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR30                                                             
         BAS   RE,PRINTIT                                                       
         B     LR20                                                             
*                                                                               
LR30     GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
************************************************                                
* PRINT FULL RECORD                                                             
*                                                                               
PRINTIT  NTR1                                                                   
         MVC   MYKEY,KEY           SAVE KEY                                     
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         XC    P,P                                                              
         XC    MYWORK,MYWORK                 * NETWORK                          
         MVC   MYWORK+2(3),NINDKNET                                             
         GOTO1 =V(MSUNPK),DMCB,MYWORK,MYWORK+5,MYWORK+10,RR=RELO                
         MVC   P+1(4),MYWORK+10                                                 
         MVC   P+10(L'LISTAR),LISTAR                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ELCODE,X'02'                                                     
         USING NINDELEM,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   PRTX                                                             
PRT5     MVC   MYWORK,SPACES                                                    
         MVC   MYWORK(1),NINDPRD                                                
         BAS   RE,GET3PRD                                                       
         MVC   LPRD,MYWORK                                                      
         TM    CNTRL,NINDCBRP      DOING BRP'S?                                 
         BO    PRT6                                                             
         EDIT  (B1,NINDIND),(4,LINDX),ALIGN=LEFT                                
         B     PRT7                                                             
PRT6     DS    0H                                                               
         EDIT  (B2,NINDBRP),(4,LINDX),1,ALIGN=LEFT                              
PRT7     DS    0H                                                               
         CLC   MYWORK(3),=C'ALL'                                                
         BE    *+8                                                              
         BAS   RE,GETPROD                                                       
         MVC   LPRDNM,MYWORK                                                    
         LA    R1,P                ARE WE DONE WITH PRINT LINE                  
         CR    R1,R2                                                            
         BNE   PRT12               YES                                          
         LA    R2,40(R2)           NO/BUMP IT                                   
         B     PRT12B                                                           
PRT12    GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
PRT12B   BAS   RE,NEXTEL                                                        
         BE    PRT5                                                             
PRTX     DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   KEY,MYKEY           RESET KEY                                    
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
GETPROD  NTR1                                                                   
         DS    0H                  READ PRODUCT REC FOR PRODUCT NAME            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),MYWORK                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(7),KEYSAVE                                                   
         BE    PRD10                                                            
         MVC   MYWORK(20),=CL20'** UNKNOWN **'                                  
         B     PRDX                                                             
PRD10    L     R6,AIO2                                                          
         LA    R3,KEY+14                                                        
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'SPTFILE',(R3),(R6),DMWORK         
         USING PRDHDR,R6                                                        
         MVC   MYWORK(20),PNAME                                                 
PRDX     DS    0H                                                               
         B     EXIT                                                             
*        DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
*                                                                               
*   MYWORK HAS EFFECTIVE DATE IN COMPLEMENT FORM                                
*   CHECK EFF DATE AGAINST DATE IN PROGRAM REC                                  
GETPROG  NTR1                                                                   
         MVC   MYKEY,KEY           SAVE KEY                                     
         XC    MYWORK(2),=X'FFFF'                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),QNETMKT                                                 
         MVC   KEY+5(6),0(R5)                                                   
         MVC   KEY+11(2),MYWORK                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(11),KEYSAVE                                                  
         BNE   GPRERR                                                           
         CLC   KEY+11(2),MYWORK                                                 
         BL    GPRERR                                                           
         L     R6,AIO2                                                          
         LA    R3,KEY+14                                                        
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'SPTFILE',(R3),(R6),DMWORK         
         USING NPGKEY,R6                                                        
         LA    R6,NPGMAINL                                                      
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GPRERR                                                           
         USING NPGELEM,R6                                                       
         MVC   MYWORK(16),NPGNAME                                               
         B     GPRX                                                             
*                                                                               
GPRERR   MVC   MYWORK(20),=C'*****  UNKNOWN *****'                              
         B     GPRX                                                             
*                                                                               
GPRX     MVC   KEY,MYKEY           RESET KEY                                    
         GOTO1 HIGH                AND SEQUENTIAL READ                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK PROG/INDEX RECORDS'                              
         SSPEC H2,46,C'--------------------------'                              
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
PRDERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** ERROR - INVALID PRODUCT CODE'                 
         B     MYERR                                                            
ALLERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'*** ERROR - ''ALL'' BRAND ENTRY REQUIRED'         
         B     MYERR                                                            
*                                                                               
DUPRDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** ERROR - DUPLICATE PRODUCT   '                 
         B     MYERR                                                            
*                                                                               
PRGERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(11),=C'*** ERROR -'                                      
         MVC   CONHEAD+12(6),MYWORK                  MOVE IN PROG NAME          
         MVC   CONHEAD+19(22),=C'-HAS NO PROGRAM RECORD'                        
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENPROGA                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMCBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMCCD                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C15 WORK AREA  *******                  
WORKAREA DS    0CL400                                                           
RELO     DS    F                                                                
MYWORK   DS    CL40                                                             
MYKEY    DS    CL48                                                             
CNTRL    DS    XL1                 CONTROL                                      
ALLSW    DS    XL1                                                              
PRDLIST  DS    CL300               FOR 100 PRODUCTS                             
WORKEQU  EQU   *-MYWORK                                                         
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LPRDNM   DS    CL20                                                             
         DS    CL1                                                              
LINDX    DS    CL4                                                              
         SPACE                                                                  
*                                                                               
INDXREC  DSECT                                                                  
       ++INCLUDE NEGENIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063NESFM15S  05/01/02'                                      
         END                                                                    
