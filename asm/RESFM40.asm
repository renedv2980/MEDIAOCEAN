*          DATA SET RESFM40    AT LEVEL 128 AS OF 11/20/00                      
*PHASE T81840A                                                                  
         TITLE 'T81840 - RESFM40 - DARE NOTIFICATION ASSIGNMENT RECORD'         
***********************************************************************         
*                                                                     *         
*  RESFM40 (T81840) --- MAINTENANCE/LIST OF DARE NOTICE RECORDS       *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 20NOV00 (SKU) FIX SALESPERSON 2-LETTER BUG                          *         
* 01MAR99 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
*HERE******************************************************************         
T81840   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81840*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         OI    DNASTA1H+1,X'01'    SET ALWAYS MODIFIED                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS?                               
         BE    LR                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK20                                                             
         LA    R2,LDNTYPEH                                                      
         CLI   8(R2),C'S'          MUST BE SALESPERSON OR                       
         BE    VK10                                                             
         CLI   8(R2),C'T'          TEAM                                         
         BE    VK15                                                             
         B     INVLFLD                                                          
*                                                                               
VK10     DS    0H                                                               
         TM    LDNSALH+4,X'20'                                                  
         BNZ   VKX                                                              
         MVI   PREVFLAG,C'N'                                                    
         OI    LDNSALH+4,X'20'                                                  
         XC    KEY,KEY                                                          
         B     VKX                                                              
*                                                                               
VK15     DS    0H                                                               
         TM    LDNTEAMH+4,X'20'                                                 
         BNZ   VKX                                                              
         MVI   PREVFLAG,C'N'                                                    
         OI    LDNTEAMH+4,X'20'                                                 
         XC    KEY,KEY                                                          
         B     VKX                                                              
*                                                                               
VK20     DS    0H                  DISPLAY EITHER SALESPERSON OR TEAM           
         CLI   DNASALH+5,0                                                      
         BNE   VK30                                                             
         CLI   DNATEAMH+5,0                                                     
         BNE   VK40                                                             
         LA    R2,DNASALH                                                       
         B     MISSFLD                                                          
*                                                                               
* SALESPERSON                                                                   
*                                                                               
VK30     DS    0H                                                               
         CLI   DNATEAMH+5,0                                                     
         BNE   INVLFLD                                                          
*                                                                               
         BAS   RE,VALSAL                                                        
*                                                                               
         L     R6,AIO                                                           
         USING RDNAKEY,R6                                                       
         XC    RDNAKEY,RDNAKEY                                                  
         MVI   RDNAKTYP,RDNATYPQ   X'53' TYPE                                   
         MVC   RDNAKREP,AGENCY                                                  
         MVC   RDNAKSAL,DNASAL                                                  
*                                                                               
         MVC   KEY,RDNAKEY                                                      
         B     VKX                                                              
         DROP  R6                                                               
*                                                                               
* TEAM                                                                          
*                                                                               
VK40     DS    0H                                                               
         BAS   RE,VALTEAM                                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDNAKEY,R6                                                       
         XC    RDNAKEY,RDNAKEY                                                  
         MVI   RDNAKTYP,RDNATYPQ   X'53' TYPE                                   
         MVC   RDNAKREP,AGENCY                                                  
         MVI   RDNAKSAL,X'FF'                                                   
         MVC   RDNAKTEM,DNATEAM                                                 
*                                                                               
         MVC   KEY,RDNAKEY                                                      
         B     VKX                                                              
         DROP  R6                                                               
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         MVC   SVDMWORK,DMWORK+4   NEED TO PRESERVE FOR LATER PUTREC            
*                                                                               
         LA    R2,DNASTA1H                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VR10     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR20                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALISTA                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RDNASTED,R6                                                      
         MVI   RDNASTCE,X'10'                                                   
         MVI   RDNASTLN,RDNASTOV                                                
         MVC   RDNASTST,WORK                                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR20     DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         LA    R1,DNASTALH                                                      
         CR    R2,R1                                                            
         BNH   VR10                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    VR30                MUST HAVE AT LEAST ONE STATION               
         CLI   DNATEAM,0           UNLESS THIS IS TEAM NOTIFICATION             
         BNE   VR30                                                             
         LA    R2,DNASTA1H                                                      
         B     MISSFLD                                                          
*                                                                               
VR30     DS    0H                                                               
         LA    R2,DNAAGY1H                                                      
*                                                                               
VR50     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR60                                                             
*                                                                               
         BAS   RE,VALAGY                                                        
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RDNAAGED,R6                                                      
         MVI   RDNAAGCE,X'20'                                                   
         MVI   RDNAAGLN,RDNAAGOV                                                
         MVC   RDNAAGAG,WORK                                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR60     DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         LA    R1,DNAAGYLH                                                      
         CR    R2,R1                                                            
         BNH   VR50                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    VRX                 MUST HAVE AT LEAST ONE AGENCY                
         LA    R2,DNAAGY1H                                                      
         B     MISSFLD                                                          
*                                                                               
VRX      DS    0H                                                               
         CLI   ACTNUM,ACTADD       GETREC BEFORE THE PUTREC                     
         BE    VRXX                                                             
         MVC   AIO,AIO2            PUT IT IN AIO2                               
         MVC   KEY+28(4),SVDMWORK                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            NEW RECORD IS IN AIO1                        
*                                                                               
VRXX     DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SALESPERSON CODE                                                     
***********************************************************************         
VALSAL   NTR1                                                                   
*                                                                               
         LA    R2,DNASALH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,8(R2)                                                   
         OC    RSALKSAL,SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
         MVC   KEYSAL,RSALKSAL                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         MVC   KEYTEAM,RSALTEAM                                                 
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE TEAM CODE                                                            
***********************************************************************         
VALTEAM  NTR1                                                                   
*                                                                               
         LA    R2,DNATEAMH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING RTEMKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,AGENCY                                                  
         MVC   RTEMKTEM,8(R2)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
         MVC   KEYTEAM,RTEMKTEM                                                 
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY FIELD                                                         
* ON INPUT R2 POINTS TO FIELD HEADER TO BE VALIDATED                            
***********************************************************************         
VALAGY   NTR1                                                                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING RAGKDKEY,R6                                                      
         XC    KEY,KEY                                                          
         MVI   RAGKDTYP,X'9A'                                                   
         MVC   RAGKDREP,AGENCY                                                  
         MVC   RAGKDDAG,8(R2)                                                   
         CLI   9(R2),C'-'                                                       
         BNE   VAGY10                                                           
         MVC   RAGKDDAG+1,=C'  '                                                
         MVC   RAGKDDAO,10(R2)                                                  
         B     VAGY50                                                           
*                                                                               
VAGY10   DS    0H                                                               
         CLI   10(R2),C'-'                                                      
         BNE   VAGY20                                                           
         MVI   RAGKDDAG+2,C' '                                                  
         MVC   RAGKDDAO,11(R2)                                                  
         B     VAGY50                                                           
*                                                                               
VAGY20   DS    0H                                                               
         CLI   11(R2),C'-'                                                      
         BNE   VAGY50                                                           
         MVC   RAGKDDAO,12(R2)                                                  
*                                                                               
VAGY50   DS    0H                                                               
         OC    RAGKDDAO,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
         MVC   WORK(5),RAGKDDAG                                                 
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVI   PREVFLAG,C'Y'                                                    
         MVC   PREVKEY,KEY         RETURN KEY AFTER LIST SELECT                 
*                                                                               
         L     R6,AIO                                                           
         USING RDNAREC,R6                                                       
         CLI   RDNAKSAL,X'FF'                                                   
         BE    DK10                                                             
         MVC   DNASAL,RDNAKSAL                                                  
         OI    DNASALH+6,X'80'    XMIT FIELD                                    
         B     DKX                                                              
*                                                                               
DK10     DS    0H                                                               
         MVC   DNATEAM,RDNAKTEM                                                 
         OI    DNATEAMH+6,X'80'    XMIT FIELD                                   
*                                                                               
DKX      DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC DNASTA1H,DNASTALH                                                
         TWAXC DNAAGY1H,DNAAGYLH                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR05                                                             
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,DNADATE)                             
         OI    DNADATEH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
DR05     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR50                                                             
         USING RDNASTED,R6                                                      
*                                                                               
         LA    R2,DNASTA1H                                                      
*                                                                               
DR10     DS    0H                                                               
         MVC   8(5,R2),RDNASTST                                                 
         OI    6(R2),X'80'         XMIT                                         
         BRAS  RE,NEXTEL                                                        
         BNE   DR50                                                             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DR10                                                             
         DROP  R6                                                               
*                                                                               
DR50     DS    0H                                                               
         LA    R2,DNAAGY1H                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRX                                                              
         USING RDNAAGED,R6                                                      
*                                                                               
DR60     DS    0H                                                               
         MVC   8(3,R2),RDNAAGAG                                                 
         CLI   10(R2),C' '                                                      
         BNE   DR80                                                             
         MVI   10(R2),C'-'                                                      
         MVC   11(2,R2),RDNAAGAG+3                                              
         B     DR90                                                             
*                                                                               
DR80     DS    0H                                                               
         MVI   11(R2),C'-'                                                      
         MVC   12(2,R2),RDNAAGAG+3                                              
*                                                                               
DR90     DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         BRAS  RE,NEXTEL                                                        
         BNE   DRX                                                              
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DR60                                                             
         DROP  R6                                                               
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR03                                                             
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LR03     DS    0H                                                               
         CLI   PREVFLAG,C'N'       PREVIOUS KEY EXAMINED?                       
         BE    LR05                                                             
         MVC   KEY,PREVKEY         USE SELECTED KEY                             
         MVI   PREVFLAG,C'N'                                                    
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(L'RDNAKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RDNAKEY,R6                                                       
         MVI   RDNAKTYP,RDNATYPQ                                                
         MVC   RDNAKREP,AGENCY                                                  
         CLI   LDNTYPE,C'T'                                                     
         BNE   *+8                                                              
         MVI   RDNAKSAL,X'FF'                                                   
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RDNAKEY,R6                                                       
         CLI   KEY,X'53'                                                        
         BNE   LRX                                                              
         CLC   RDNAKREP,AGENCY      MATCHING REC TYPE AND REP CODE              
         BNE   LRX                                                              
         CLI   LDNTYPE,C'T'                                                     
         BNE   LR22                                                             
         MVC   LDNHDR+5(11),=C'Team       '                                     
         OI    LDNHDRH+6,X'80'     XMIT                                         
         CLI   RDNAKSAL,X'FF'                                                   
         BNE   LRX                                                              
         B     LR23                                                             
*                                                                               
LR22     DS    0H                                                               
         MVC   LDNHDR+5(11),=C'Salesperson'                                     
         OI    LDNHDRH+6,X'80'     XMIT                                         
         CLI   RDNAKSAL,X'FF'                                                   
         BE    LRX                                                              
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
LR23     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
LR25     DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LDSAL,RDNAKSAL                                                   
         CLI   LDNTYPE,C'T'                                                     
         BNE   LR30                                                             
         MVC   LISTAR,SPACES                                                    
         MVC   LDTEAM,RDNAKTEM                                                  
         DROP  R6                                                               
*                                                                               
LR30     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR40                                                             
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,LDUPDATE)                            
         DROP  R6                                                               
*                                                                               
LR40     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LRPRT                                                            
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRPRT    DS    0H                                                               
         MVC   P(LDLEN),LISTAR                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRSEQ                                                            
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONVERT SCREEN INPUT TO PACK W/O SIGN                                         
***********************************************************************         
PACK     NTR1                                                                   
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BZ    INVLFLD                                                          
                                                                                
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*********************************************************************           
* HEADING FOR REPORT                                                            
*********************************************************************           
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,46,C'LABEL RECORDS'                                           
         SSPEC H2,46,C'-------------'                                           
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HOOK     NTR1                                                                   
         MVC   H8(L'HEADER),HEADER                                              
         MVC   H9(70),DASHES                                                    
         B     XIT                                                              
*                                                                               
DASHES   DC    70C'-'                                                           
HEADER   DC    CL21'NAME      DESCRIPTION'                                      
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         GETELN R5,DATADISP,ELCODE2,2                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMA7D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMA8D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENDNA                                                       
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENTEM                                                       
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            APPLICATION SAVE STORAGE                     
SVDMWORK DS    F                                                                
ELCODE2  DS    X                                                                
KEYSTA   DS    CL5                                                              
KEYAGY   DS    CL5                                                              
KEYSAL   DS    CL3                                                              
KEYTEAM  DS    CL2                                                              
PREVKEY  DS    CL(L'KEY)           FOR LIST TO RETURN TO                        
PREVFLAG DS    C                   (Y/N) PREVIOUS KEY USED INDICATOR            
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LDSAL    DS    CL3                 SALESPERSON                                  
         ORG   LDSAL                                                            
LDTEAM   DS    CL2                 TEAM                                         
         DS    C                                                                
         DS    CL12                                                             
LDUPDATE DS    CL8                 LAST UPDATE DATE                             
         DS    C                                                                
LDUPTIME DS    CL5                 LAST UPDATE TIME                             
LDLEN    EQU   *-LDSAL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128RESFM40   11/20/00'                                      
         END                                                                    
