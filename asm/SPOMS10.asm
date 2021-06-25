*          DATA SET SPOMS10    AT LEVEL 002 AS OF 01/09/15                      
*PHASE T23410A                                                                  
T23410   TITLE 'SPOMS10 - ORDER PATCH'                                          
T23410   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23410*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                   STANDARD CODING                       
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA             BASE SCREEN + OUR SCREEN              
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                R5=A(LOCAL SAVED STORAGE)             
         USING LSSD,R5                                                          
*                                                                               
         BAS   RE,GETPF                   GET PFKEYS                            
         ST    R3,RELO                                                          
         ST    RC,BASERC                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         LA    R3,RELOTAB                                                       
INIT3    CLI   0(R3),X'FF'                                                      
         BE    INIT5                                                            
         ICM   RF,15,0(R3)                                                      
         A     RF,RELO                                                          
         ICM   RE,15,4(R3)                                                      
         LA    RE,LSSD(RE)                                                      
         STCM  RF,15,0(RE)                                                      
         LA    R3,L'RELOTAB(R3)                                                 
         B     INIT3                                                            
*                                                                               
INIT5    OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         OI    GENSTAT3,MULTFILS                                                
         MVI   NLISTS,1                   ONLY 1 RECORD LISTED PER LIST         
*                                                                               
         CLI   MODE,VALKEY                VALIDATE KEY?                         
         BE    VK                                                               
         CLI   MODE,LISTRECS              LIST RECORDS?                         
         BE    LST                                                              
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
*                                                                               
XIT      XIT1                                                                   
***********************************************************************         
*                       SET FILE                                      *         
***********************************************************************         
SF       BAS   RE,SSV                                                           
         B     XIT                                                              
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
VK       DS    0H                                                               
         BRAS  RE,RSV                                                           
*                                                                               
         NI    DMINBTS,X'FF'-X'08'        NO DELETED RECORDS                    
*                                                                               
         LA    R2,ORLMEDH                 MEDIA HEADER                          
         TM    4(R2),X'20'                DID THIS FIELD CHANGE?                
         BO    VKORD00                    NO                                    
         MVI   MISCFLG3,X'FF'-MF3RADIO                                          
         CLI   5(R2),0                    ANY INPUT?                            
         BNE   VKMED05                    YES                                   
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
         CLI   8(R1),0                    HAVE MEDIA?                           
         JNE   NEEDFLDS                   NO - ERROR                            
*                                                                               
VKMED05  GOTO1 VALIMED                    VALIDATE MEDIA                        
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
         CLI   8(R1),0                    ANY ERROR AFTER PUTTING MED?          
         BE    *+6                        NO                                    
         DC    H'0'                       YES - DEATH                           
         OI    4(R2),X'20'                PREVIOUSLY VALIDATED                  
*                                                                               
         MVC   BYTE,BAGYMD                A/M                                   
         NI    BYTE,X'0F'                 TURN OFF AGENCY NIBBLE                
         CLI   BYTE,X'02'                 MEDIA R?                              
         BNE   *+8                        NO                                    
         OI    MISCFLG3,MF3RADIO          YES - FLAG MEDIA R                    
*                                                                               
VKORD00  LA    R2,ORLORDRH                ORDER NUMBER HEADER                   
         TM    4(R2),X'20'                DID THIS FIELD CHANGE?                
         JO    VKGETORD                   NO                                    
         CLI   5(R2),0                    ORDER NUMBER INPUT?                   
         JE    NEEDFLDS                    NO                                   
*                                                                               
         TM    4(R2),X'08'                VALID NUMERIC?                        
         JZ    INVLFLD                    NO - ERROR                            
         CLI   5(R2),8                    8 CHARACTER ORDER?                    
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    4(R2),X'20'                VALIDATED                             
*                                                                               
         GOTOR BINORDR,DMCB,8(R2)         GET BINARY ORDER NUMBER               
*                                                                               
VKGETORD LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DAREORDD,R6                DARE ORDER DSECT                      
         MVI   DOKTYPE,DOKTYPQ            X'0D'                                 
         MVI   DOKSUBTY,DOKSTYPQ          X'34'                                 
         MVC   DOKAGMD,BAGYMD             A/M                                   
         MVC   DOKORDER,BINORDER          ORDER NUMBER                          
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(9),KEYSAVE             KEY MATCHES?                          
         BNE   ERRNOORD                   NO - NO ORDER FOUND                   
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,WORK,12,0,0                                      
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVSPREQ                          
         CLI   8(R1),0                    ANY ERROR?                            
         BE    *+6                        NO                                    
         DC    H'0'                       YES - DEATH                           
*                                                                               
         OI    ORLTITLH+6,X'80'           SET TRANSMIT PF FIELD                 
         MVC   ORLTITL+75(3),SPACES                                             
         LA    R2,ORLMKGDH                MAKEGOOD HEADER                       
         CLI   5(R2),0                    ANY INPUT?                            
         BE    VKX10                      NO                                    
         MVC   ORLTITL+75(3),=C'Grp'                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,DOKSTA,WORK,3,0,0                                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,6,GLVSPSTA                           
         CLI   8(R1),0                    ANY ERROR?                            
         BE    *+6                        NO                                    
         DC    H'0'                       YES - DEATH                           
*                                                                               
         GOTO1 GETREC                     GET THE ORDER RECORD                  
*                                                                               
         L     R6,AIO                     R6 = ORDER RECORD                     
         MVI   ELCODE,DOIDELQ             X'01' ELEMENT                         
         BAS   RE,GETEL                   HAVE AN X'01' ELEMENT?                
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
         USING DOIDELD,R6                 ID ELEMENT DSECT                      
         MVC   FULL(3),DOIDBYR            BUYER                                 
         MVC   SVSTATN,DOISTA                                                   
         DROP  R6                                                               
*                                                                               
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
         CLI   SVSTATN,X'E8'        CABLE?                                      
         BL    VKGETMG                                                          
*                                                                               
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
*                                                                               
         LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING MNXKEY,R6                  DARE MAKEGOOD DSECT                   
         MVI   MNXKTYPE,MNXKTYPQ          X'0D'                                 
         MVI   MNXKSBTY,MNXKSBTQ          X'36'                                 
         MVC   MNXKAGMD,BAGYMD            A/M                                   
         MVC   MNXKORDR,BINORDER          ORDER NUMBER                          
         MVC   MNXKGRP,8(R2)              MAKEGOOD GROUP                        
         OC    MNXKGRP,SPACES             SPACE PAD                             
         DROP  R6                         DROP R6                               
*                                                                               
         MVC   KEYSAVE,KEY                PASS BACK DELETED RECORDS             
         GOTO1 HIGH                                                             
****     GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY               
         CLC   KEY(L'MNXKEY),KEYSAVE      KEY MATCHES?                          
         BNE   ERRNOORD                   NO - NO ORDER FOUND                   
         B     VKX10                                                            
*                                                                               
VKGETMG  LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DAREMGND,R6                DARE MAKEGOOD DSECT                   
         MVI   MNKTYPE,MNKTYPQ            X'0D'                                 
         MVI   MNKSUBTY,MNKSTYPQ          X'36'                                 
         MVC   MNKAGMD,BAGYMD             A/M                                   
         MVC   MNKBYR,FULL                BUYER                                 
         MVC   MNKORDER,BINORDER          ORDER NUMBER                          
         MVC   MNKGROUP,8(R2)             MAKEGOOD GROUP                        
         OC    MNKGROUP,SPACES            SPACE PAD                             
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            KEY MATCHES?                          
         BNE   ERRNOORD                   NO - NO ORDER FOUND                   
         DROP  R6                         DROP R6                               
*                                                                               
VKX10    MVC   SAVEKEY,KEY                SAVE THE KEY                          
*                                                                               
VKX      B     XIT                        EXIT                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                              *         
***********************************************************************         
LST      MVI   RDUPDATE,C'N'              TURN OFF RDUPDATE                     
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
*                                                                               
         MVC   KEY,SAVEKEY                KEY BUILT IN VK                       
*                                                                               
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
         CLI   KEY+1,X'36'                MAKEGOOD RECORD?                      
         BNE   LST05                                                            
         CLI   SVSTATN,X'E8'       CABLE?                                       
         BL    LST05                                                            
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
*                                                                               
         GOTO1 HIGH                                                             
****     GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY               
         JNE   *+2                                                              
         MVC   DAD,KEY+36                                                       
*                                                                               
         GOTO1 GETREC                                                           
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',KEY+36,      +        
               AIO,DMWORK                                                       
*&&                                                                             
         JNE   *+2                                                              
         BAS   RE,CLRLINE                 CLEAR LIST LINE                       
         MVC   DMDSKADD,DAD               SET DISK ADDRESS                      
         LA    R2,ORLSEL1H                R2=A(SCREEN LINE)                     
         USING LINDSECT,R2                LIST LINE DSECT                       
         MVC   LINADT,=C'CABLE'                                                 
         MVC   LINGRP,ORLMKGD             MOVE MAKEGOOD CODE TO LIST            
         MVC   LINORD(8),ORLORDR          MOVE ORDER NUM TO LIST LINE           
         B     LST20                      GO TO LISTMON                         
         DROP  R2                                                               
*                                                                               
LST05    GOTO1 HIGH                       READ HIGH                             
         JNE   *+2                                                              
*                                                                               
         MVC   DAD,KEY+14                 D/A                                   
*                                                                               
         GOTO1 GETREC                     GET THE RECORD                        
*                                                                               
         CLI   KEY+1,X'36'                MAKEGOOD RECORD?                      
         BNE   LST10                      NO                                    
         BAS   RE,CLRLINE                 CLEAR LIST LINE                       
         LA    R2,ORLSEL1H                R2=A(SCREEN LINE)                     
         USING LINDSECT,R2                LIST LINE DSECT                       
         MVC   LINGRP,ORLMKGD             MOVE MAKEGOOD CODE TO LIST            
         MVC   LINORD(8),ORLORDR          MOVE ORDER NUM TO LIST LINE           
         B     LST20                      GO TO LISTMON                         
         DROP  R2                         DROP R2                               
*                                                                               
LST10    BAS   RE,FLTREC                  FILTER THE RECORD                     
         BAS   RE,DSPLR                   DISPLAY RECORDS                       
LST20    GOTO1 LISTMON                                                          
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',TOTDOL,L'TOTDOL,GLVSPID                   
         CLI   8(R1),0                    ANY ERROR?                            
         BE    *+6                        NO                                    
         DC    H'0'                       YES - DEATH                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',TOTSPOT,L'TOTSPOT,GLVDQU                  
         CLI   8(R1),0                    ANY ERROR?                            
         BE    *+6                        NO                                    
         DC    H'0'                       YES - DEATH                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ORLSTT1+1,L'ORLSTT1-1,GLVPGM              
         CLI   8(R1),0                    ANY ERROR?                            
         BE    *+6                        NO                                    
         DC    H'0'                       YES - DEATH                           
*                                                                               
         BAS   RE,SETPF                   SET PF KEYS                           
*                                                                               
         L     R1,ATIOB                   SET CURSOR                            
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         SR    R2,R2                                                            
         ICM   R2,3,CURSOUT               CURSOR TO A SELECTED LINE ?           
         BNZ   *+8                                                              
         LHI   R2,(ORLSEL1H-T234FFD)                                            
         STCM  R2,3,TIOBCURD                                                    
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         J     XIT                                                              
***********************************************************************         
* ROUTINE TO DISPLAY RECORD                                           *         
***********************************************************************         
DSPLR    NTR1  ,                                                                
         LA    R2,ORLSEL1H                R2=A(SCREEN LINE)                     
         USING LINDSECT,R2                                                      
         LA    R3,LSTTAB                  R3=A(LIST OF ORDERS)                  
         USING LSTD,R3                                                          
         L     R6,AIO                     A(ORDER RECORD)                       
         USING DAREORDD,R6                DARE ORDER DSECT                      
*                                                                               
         BAS   RE,CLRLINE                 CLEAR LIST LINE                       
         MVC   BINORDER,DOKORDER          SHOW THE ORDER NUMBER                 
         BAS   RE,SHWORDER                SHOW ORDER NUMBER                     
         MVC   LINORD(8),CHRORDER         MOVE ORDER NUM TO LIST LINE           
*                                                                               
         OC    LSTODT,LSTODT              ANY DATE?                             
         BZ    DSPLR05                    NO                                    
         GOTO1 DATCON,DMCB,(8,LSTODT),(7,LINODT)                                
*                                                                               
DSPLR05  MVC   BCLT,LSTCLT                CLIENT                                
         MVC   BPRD,LSTPRD                PRODUCT                               
         GOTO1 GETQPRD                    GET PRD                               
*                                                                               
         L     R6,AIO                     AIO = A(CLIENT RECORD)                
         USING CLTHDRD,R6                 CLT REC DSECT                         
DSPLR07  GOTO1 CLUNPK,DMCB,(CPROF+6,BCLT),LINCLT                                
         DROP  R6                         DROP R6                               
*                                                                               
         MVC   LINPRD(3),=C'***'          PRD                                   
         CLI   BPRD,X'FF'                 POL?                                  
         BE    *+10                       YES                                   
         MVC   LINPRD(L'QPRD),QPRD        NO - MOVE IN BRD PRD                  
*                                                                               
         CLI   LSTPR2,0                   PIGGYBACK?                            
         BE    DSPLR10                    NO                                    
         MVC   BPRD,LSTPR2                PIGGY                                 
         GOTO1 GETQPRD                    GET PRD                               
         MVI   LINPRD+3,C'-'                                                    
         MVC   LINPRD+4(L'QPRD),QPRD                                            
*                                                                               
DSPLR10  MVC   LINSTC,LSTSTC              STATUS CODE                           
*                                                                               
         MVC   LINBYR,LSTBYR              BUYER                                 
         OC    LINBYR,SPACES                                                    
         EDIT  (B1,LSTEST),(3,LINEST),FILL=0                                    
*                                                                               
         MVC   QCLT,LINCLT                                                      
         GOTO1 GETMKT,LSTSTA                                                    
         MVC   LSTMKT,BMKT                MARKET NUMBER TO LIST                 
         MVC   MKT,QMKT                   MARKET TO PASS                        
         MVC   LINMKN,MKTNM               MARKET NAME TO SCREEN                 
*                                                                               
         MVC   LINSTA,QSTA                                                      
         TM    MISCFLG3,MF3RADIO   ARE WE RADIO?                                
         BNZ   *+8                                                              
         MVI   LINSTA+4,C' '       NO, REMOVE THE C'T' FOR TV                   
*                                                                               
         CLI   LINSTA+3,C'-'                                                    
         BNE   *+8                                                              
         MVI   LINSTA+3,C' '                                                    
         OI    LINSTAH+(FLDIIND-FLDHDRD),FINPALF  MAKE ALPHA                    
         MVI   LINSTAH+(FLDILEN-FLDHDRD),4        INPUT LENGTH                  
         CLI   LINSTA+4,C' '                      TEST BAND                     
         BL    *+8                                                              
         MVI   LINSTAH+(FLDILEN-FLDHDRD),5        INPUT LENGTH                  
*                                                                               
DSPLR20  MVC   LINFLT,SPACES            DEFAULT TO SPACES                       
         CLI   LSTFLT,0                   FLIGHT                                
         BE    DSPLR25                                                          
         EDIT  (B1,LSTFLT),(2,LINFLT),FILL=0                                    
*                                                                               
DSPLR25  MVI   LINSTT1,C' '                                                     
         TM    LSTFLG,LSTFSTR             TEST DISPLAY '*'                      
         BNO   *+8                                                              
         MVI   LINSTT1,C'*'                                                     
         MVI   LINOFI,C' '                                                      
         TM    LSTFLG,LSTFMGO             TEST MG OFFER                         
         BNO   *+8                                                              
         MVI   LINOFI,C'X'                                                      
         MVI   LINORD+8,C' '                                                    
         TM    LSTFLG,LSTTRDE             TEST TRADE                            
         BNO   *+8                                                              
         MVI   LINORD+8,C'T'                                                    
*                                                                               
         XC    LSTACT,FFS                 REVERSE THE BITS                      
         CLC   LSTACT,FFS                                                       
         BE    DSPLR30                                                          
*                                                                               
         OC    LSTACT,LSTACT                                                    
         BZ    DSPLR30                                                          
         GOTO1 DATCON,DMCB,(8,LSTACT),(7,LINADT)                                
*                                                                               
DSPLR30  MVC   DMDSKADD,LSTDAD            SET DISK ADDRESS                      
         J     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
CLRLINE  LA    R2,ORLSEL1H                R2=A(SCREEN LINE)                     
         USING LINDSECT,R2                LIST SCREEN DSECT                     
         OI    LINSTTH+6,X'80'                                                  
         OI    LINOFIH+6,X'80'                                                  
         OI    LINSTAH+6,X'80'                                                  
         OI    LINMKNH+6,X'80'                                                  
         OI    LINCLTH+6,X'80'                                                  
         OI    LINPRDH+6,X'80'                                                  
         OI    LINESTH+6,X'80'                                                  
         OI    LINFLTH+6,X'80'                                                  
         OI    LINODTH+6,X'80'                                                  
         OI    LINADTH+6,X'80'                                                  
         OI    LINBYRH+6,X'80'                                                  
         OI    LINORDH+6,X'80'                                                  
         OI    LINGRPH+6,X'80'                                                  
         XC    LINSTT1,LINSTT1                                                  
         XC    LINSTC,LINSTC                                                    
         XC    LINOFI,LINOFI                                                    
         XC    LINSTA,LINSTA                                                    
         XC    LINMKN,LINMKN                                                    
         XC    LINCLT,LINCLT                                                    
         XC    LINPRD,LINPRD                                                    
         XC    LINEST,LINEST                                                    
         XC    LINFLT,LINFLT                                                    
         XC    LINODT,LINODT                                                    
         XC    LINADT,LINADT                                                    
         XC    LINBYR,LINBYR                                                    
         XC    LINORD,LINORD                                                    
         XC    LINGRP,LINGRP                                                    
         BR    RE                         RETURN TO CALLER                      
***********************************************************************         
* ROUTINE TO FILTER RECORD                                            *         
*  ON ENTRY: AIO CONTAINS RECORD                                      *         
*                                                                     *         
*  ON EXIT:  YES: LSTTAB CONTAINS ONE ENTRY (LSTD DSECT)              *         
*            NO: DID NOT PASS FILTER REQS                             *         
*                                                                     *         
***********************************************************************         
FLTREC   NTR1  ,                                                                
         LA    R2,LSTTAB                  R2=A(LIST TABLE)                      
         USING LSTD,R2                    LSTD DSECT                            
         XC    LSTD(LSTLNQ),LSTD          CLEAR FIRST SPOT                      
*                                                                               
         MVI   RECFLG,0                   INIT RECORD FLAG                      
         XC    CLRDATE,CLRDATE            CLEAR COLOR DATE                      
         NI    MISCFLG4,X'FF'-MF4CNFCM-MF4AMEND                                 
         NI    MISCFLG1,X'FF'-MF1XDOSP                                          
         NI    MISCFLG2,X'FF'-MF2VAROR-MF2REVOR-MF2OFFER                        
         NI    MISCFLG2,X'FF'-MF2FAXED-MF2EMAIL-MF2CANCF                        
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         USING DOKEY,R6                   DOKEY DSECT                           
         MVC   LSTORD,DOKORDER            SAVE ORDER NUMBER                     
         MVC   LSTSTA,DOKSTA              STATION CODE                          
         MVI   ELCODE,DOIDELQ             X'01' ELEMENT CODE                    
         BRAS  RE,GETEL                   HAVE A X'01' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       NO - MEGA DEATH!                      
*                                                                               
         USING DOIDELD,R6                 X'01' ELEMENT DSECT                   
         MVC   LSTBYR,DOIDBYR             BUYER                                 
         OC    LSTBYR,SPACES              SPACE PAD                             
         MVC   LSTCLT,DOIDCLT             CLIENT                                
         MVC   LSTPRD,DOIDPRD             PRODUCT                               
         MVC   LSTPR2,DOIDPRD2            SECOND PRODUCT                        
         MVC   LSTEST,DOIDEST             ESTIMATE                              
         MVC   LSTFLT,DOIDFLTN            FLIGHT                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOI2ELQ             SECONDARY ELEMENT(POL)                
         BRAS  RE,GETEL                                                         
         BNE   FLTRC100                                                         
*                                                                               
         USING DOI2ELD,R6                                                       
         TM    DOI2FLG1,DOI2FVAR          VAR ORDER YET?                        
         BZ    *+8                                                              
         OI    MISCFLG2,MF2VAROR          YES                                   
         DROP  R6                                                               
*                                                                               
FLTRC100 XC    HALF,HALF                                                        
         EDIT  (B2,HALF),(10,TOTDOL),2,ALIGN=LEFT                               
         EDIT  (B2,HALF),(4,TOTSPOT),2,ALIGN=LEFT                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   FLTRC150                                                         
*                                                                               
         USING DOSPELD,R6          SUPPLEMENTARY ID ELEM DSECT                  
         OC    DOSPTOTL,DOSPTOTL   HAVE TOTAL DOLLARS?                          
         BZ    FLTRC101            NO                                           
         EDIT  (P6,DOSPTOTL),(10,TOTDOL),2,ALIGN=LEFT                           
*                                                                               
FLTRC101 OC    DOSPSPTS,DOSPSPTS   HAVE TOTAL SPOTS?                            
         BZ    FLTRC102            NO                                           
         EDIT  (B4,DOSPSPTS),(4,TOTSPOT),ALIGN=LEFT                             
*                                                                               
FLTRC102 TM    DOSPFLG1,DOSPTRDE   TRADE ORDER ?                                
         BZ    *+8                                                              
         OI    LSTFLG,LSTTRDE                                                   
*                                                                               
         CLI   DOSPREVN,0          GOT A REVISION NUMBER?                       
         BE    *+8                                                              
         OI    MISCFLG2,MF2REVOR   YES, REVISED ORDER                           
*                                                                               
         CLI   DOSPMTHD,0          WAS ANYTHING EVER ENTERED?                   
         BNE   FLTRC140                                                         
FLTRC130 OI    MISCFLG1,MF1XDOSP                                                
*                                                                               
FLTRC140 CLI   DOSPMTHD,C'F'       WAS I PREVIOUSLY FAXED?                      
         BNE   *+8                                                              
         OI    MISCFLG2,MF2FAXED                                                
*                                                                               
         CLI   DOSPMTHD,C'E'       WAS I PREVIOUSLY EMAILED?                    
         BNE   *+8                                                              
         OI    MISCFLG2,MF2EMAIL                                                
         DROP  R6                                                               
*                                                                               
FLTRC150 MVI   LSTCLR,C'K'                DEFAULT IS BLACK                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COLELQ              FIND COLOR ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   FLTRC160                                                         
         USING COLOREL,R6                                                       
         MVC   LSTCLR,COLCOL              SAVE THE COLOR                        
         MVC   HALF,COLDATE               AND COLOR DATE                        
         XC    HALF,FFS                   COLDATE IS FF'S COMP                  
         GOTO1 DATCON,DMCB,(2,HALF),(19,CLRDATE)                                
*                                                                               
FLTRC160 L     R6,AIO                                                           
         MVI   ELCODE,MGCOLELQ            MAKEGOOD COLOR ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   FLTRC170                                                         
         USING MGCOLELD,R6                                                      
         OI    LSTFLG,LSTFMGO              SET MG FLAG                          
         OI    MISCFLG2,MF2OFFER                                                
*                                                                               
FLTRC170 L     R6,AIO                                                           
         MVI   BYTE,QUNSENT        ASSUME 'UNSENT'                              
         MVI   ELCODE,DOSTELQ      FIND STATUS ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   FLTRC300                                                         
         OI    RECFLG,RFSTEL       HAVE STATUS ELEMENT                          
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,DDLVRD     SKIP DELIVRED STATUS?                        
         BNE   FLTRC180                                                         
         OI    RECFLG,RFDLDT       HAVE DELIVERED DATE & TIME                   
         MVC   LSTACT,DOSTDATE                                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT DOSTELEM                        
*                                                                               
FLTRC180 ST    R6,ADOSTETL         SAVE ADDRESS OF STATUS ELEMENT               
         TM    RECFLG,RFDLDT       DID WE HAVE A DELNOT?                        
         BNZ   *+10                                                             
         MVC   LSTACT,DOSTDATE                                                  
*                                                                               
         CLC   LSTACT,CLRDATE      LAST ACTIVITY VS. COLOR                      
         BH    *+10                                                             
         MVC   LSTACT,CLRDATE                                                   
*                                                                               
         CLI   DOSTSTAT,DSENT      AM I SENT?                                   
         BNE   FLTRC190                                                         
         MVC   LSTODT,DOSTDATE     SAVE THIS DATE                               
         TM    RECFLG,RFDLDT       HAVE DELIVERED DATE & TIME??                 
         BNZ   FLTRC290            YES                                          
         OI    LSTFLG,LSTFSTR      DISPLAY A '*'                                
         B     FLTRC290                                                         
***************                                                                 
* SPECIAL CODE FOR CONFIRMED STATUS                                             
***************                                                                 
FLTRC190 CLI   DOSTSTAT,QCFMD      CONFIRMED STATUS?                            
         BNE   FLTRC210            NO                                           
         CLI   DOSTLEN,DOSTLNQ3    IS THERE AN TYPE FIELD?                      
         BNE   FLTRC200            NO, THEN MUST CHECK DOSPELEM                 
         TM    DOSTTYPE,DCNFMCOM   CONFIRM WITH COMMENTS?                       
         BZ    *+8                                                              
         OI    MISCFLG4,MF4CNFCM   YES!!                                        
         TM    DOSTTYPE,DCNFMCAN   CANCELLED CONFIRM?                           
         BZ    *+8                                                              
         OI    MISCFLG2,MF2CANCF   YES!!                                        
         B     FLTRC240                                                         
*                                                                               
FLTRC200 L     R6,AIO              CHECK DOSPELEM FOR CNFM W/COMMENTS           
         MVI   ELCODE,DOSPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   FLTRC240                                                         
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENT?                        
         BZ    *+8                                                              
         OI    MISCFLG4,MF4CNFCM   YES                                          
         L     R6,ADOSTETL         RESTORE R6                                   
         B     FLTRC240                                                         
***************                                                                 
* SPECIAL CODE FOR CONFIRMED STATUS (END)                                       
*================================================                               
* SPECIAL CODE FOR REJECT/AMEND STATUS (START)                                  
***************                                                                 
         USING DOSTELD,R6                                                       
FLTRC210 CLI   DOSTSTAT,QRJCT      AM I REJECT?                                 
         BNE   FLTRC220                                                         
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   *+8                                                              
         OI    MISCFLG4,MF4AMEND                                                
         B     FLTRC240                                                         
*                                                                               
***************                                                                 
* SPECIAL CODE FOR REJECT/AMEND STATUS (END)                                    
*================================================                               
* SPECIAL CODE FOR RECALL STATUS                                                
***************                                                                 
FLTRC220 MVI   BYTE,RFRLDLV        SET FOR 'RECALL DELIVERED'                   
         TM    RECFLG,RFDLDT       HAVE DELIVERED DATE & TIME??                 
         BNZ   *+8                                                              
         MVI   BYTE,RFRLNDL        SET 'RECALL NOT DELIVERED'                   
         CLI   DOSTSTAT,QRECALL    IT IT A RECALL ?                             
         BNE   FLTRC240                                                         
         OC    RECFLG,BYTE         SET RECALL BIT                               
***                                                                             
* SPECIAL CODE FOR RECALL STATUS (END)                                          
***                                                                             
FLTRC240 CLI   DOSTSTAT,DSENT      AM I SENT?                                   
         BE    FLTRC270                                                         
         CLI   DOSTSTAT,DEMSENT    AM I EMAIL?                                  
         BNE   FLTRC250                                                         
         TM    MISCFLG1,MF1XDOSP   DID I FIND THE METHOD IN DOSPELEM?           
         BZ    FLTRC270                                                         
         OI    MISCFLG2,MF2EMAIL                                                
         B     FLTRC270                                                         
*                                                                               
FLTRC250 CLI   DOSTSTAT,DFXSENT    AM I FAX?                                    
         BE    *+12                                                             
         CLI   DOSTSTAT,DFXRSNT      OR FAX RESENT?                             
         BNE   FLTRC260                                                         
         TM    MISCFLG1,MF1XDOSP   DID I FIND THE METHOD IN DOSPELEM?           
         BZ    FLTRC270                                                         
         OI    MISCFLG2,MF2FAXED                                                
         B     FLTRC270                                                         
*                                                                               
FLTRC260 ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT DOSTELEM                        
         CLI   DOSTEL,DORPELQ2                                                  
         BE    FLTRC260                                                         
         CLI   DOSTEL,DOSTELQ                                                   
         BE    FLTRC240                                                         
         B     FLTRC280                                                         
*                                                                               
FLTRC270 MVC   LSTODT,DOSTDATE            MOVE IN SENT DATE!                    
*                                                                               
FLTRC280 L     R6,ADOSTETL                                                      
*                                                                               
FLTRC290 MVC   BYTE,DOSTSTAT                                                    
*                                                                               
FLTRC300 L     R3,AORDSTAB                R3=ORDER STATUS TABLE                 
         USING ORDD,R3                                                          
         SR    R0,R0                                                            
*                                                                               
FLTRC310 CLC   BYTE,ORDSTAT               MATCH STATUS                          
         BNE   FLTRC350                                                         
         CLI   ORDTYP,0                                                         
         BE    FLTRC360                                                         
***************                                                                 
* SPECIAL CODE FOR REJECT/AMEND STATUS (START)                                  
***************                                                                 
         TM    ORDTYP,ORDTAMND                                                  
         BNO   FLTRC320                                                         
         TM    MISCFLG4,MF4AMEND                                                
         BO    FLTRC360                                                         
*                                                                               
FLTRC320 TM    ORDTYP,ORDTRJCT                                                  
         BNO   FLTRC330                                                         
         TM    MISCFLG4,MF4AMEND                                                
         BNO   FLTRC360                                                         
***************                                                                 
* SPECIAL CODE FOR REJECT/AMEND STATUS (END)                                    
*=======================                                                        
* SPECIAL CODE FOR CONFIRM/PARTIAL CONFIRM STATUS (START)                       
***************                                                                 
FLTRC330 TM    ORDTYP,ORDTCMTS            TEST WITH COMMENTS                    
         BNO   FLTRC340                                                         
         TM    MISCFLG4,MF4CNFCM          YES, ARE THERE COMMENTS ?             
         BO    FLTRC360                                                         
*                                                                               
FLTRC340 TM    ORDTYP,ORDTNCMT            TEST WITH NO COMMENTS                 
         BNO   FLTRC350                                                         
         TM    MISCFLG4,MF4CNFCM          YES, ARE THERE COMMENTS ?             
         BNO   FLTRC360                                                         
***                                                                             
* SPECIAL CODE FOR CONFIRM/PARTIAL CONFIRM STATUS (END)                         
***                                                                             
FLTRC350 IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         CLI   ORDSTAT,X'FF'              EOT - ERR999                          
         BNE   FLTRC310                                                         
*                                                                               
FLTRC360 MVC   LSTSTC,ORDDFLT             DEFAULT DISPLAY STATUS CODE           
         ICM   R0,1,ORDNFLG               R0=NUMBER OF CNTLS TO TEST            
         BZ    FLTRC420                                                         
         LA    R4,ORDDATA                                                       
         USING ORDDATA,R4                                                       
         SR    R1,R1                                                            
FLTRC400 IC    R1,ORDFLG                  TEST STATUS BITS                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    MISCFLG2,0                                                       
         BO    FLTRC410                                                         
         LA    R4,L'ORDDATA(R4)                                                 
         BCT   R0,FLTRC400                                                      
         B     FLTRC420                                                         
*                                                                               
FLTRC410 MVC   LSTSTC,ORDCODE             STATUS CODE FOR DISPLAY               
         DROP  R4                                                               
*                                                                               
FLTRC420 TM    ORDIND,ORDISENT                                                  
         BNO   *+8                                                              
         OI    LSTFLG,LSTFSTR             SET TO DISPLAY '*'                    
         TM    RECFLG,RFRLNDL             TEST RECALLED - NOT DELIVERED         
         BNO   *+8                                                              
         OI    LSTFLG,LSTFSTR             ALSO GETS A '*'                       
*                                                                               
FLTRC560 MVC   LSTDAD,DAD                 SAVE DISK ADDRESS                     
         XC    LSTACT,FFS                 MOST RECENT FIRST                     
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 GETMKT,LSTSTA              GET THE MARKET#                       
         MVC   LSTMKT,BMKT                                                      
         MVC   LSTMKN,MKTNM                                                     
         MVC   KEY,SAVEKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 HIGH                       RE-ESTABLISH SEQUENCE                 
         GOTO1 GETREC                     RE-ESTABLISH SEQUENCE                 
*                                                                               
FLTRC600 OI    RECFLG,RFOK                                                      
         J     XIT                                                              
         DROP  R2,R3,R6                                                         
***********************************************************************         
* CONVERT BINARY ORDER TO CHARACTER                                   *         
***********************************************************************         
SHWORDER MVC   FULL,BINORDER                                                    
         XC    FULL,FFS                                                         
*                                                                               
         TM    FULL,X'80'                 NEW STYLE?                            
         BNZ   SHWO20                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,FULL                  DATE PORTION                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CHRORDDT,DUB                                                     
*                                                                               
         ICM   R1,3,FULL+2                SEQUENCE PORTION                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CHRORDSQ,DUB                                                     
         BR    RE                                                               
*                                                                               
SHWO20   NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  CHRORDER,DUB                                                     
         BR    RE                                                               
***********************************************************************         
* GET MARKET NUMBER INTO BMKT                                         *         
***********************************************************************         
GETMKT   LR    R0,RE                                                            
         XC    BMKT,BMKT                  CLEAR MARKET                          
         XC    FLDH,FLDH                  CLEAR FIELD HEADER                    
         MVC   FLD,SPACES                 AND FIELD                             
         MVC   BSTA,0(R1)                 PASS STATION                          
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,FLD                                     
         ST    R2,SVR2                                                          
         LA    R2,FLDH                                                          
         USING FLDHDRD,R2                                                       
         MVI   FLDILEN,4                  SET FIELD LENGTH                      
         CLI   FLD+4,C' '                 TEST BAND                             
         BNH   *+8                                                              
         MVI   FLDILEN,5                  FIX LENGTH FOR BAND                   
         GOTO1 VALISTA                                                          
         L     R2,SVR2                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1  ,                                                                
*                                                                               
         MVI   CALLSP,0                   CLEAR PF KEY STACK                    
         GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE THE PFKEYS                 
*                                                                               
GETPF20  CLI   PFKEY,PFENTQ               ENTER?                                
         JE    XIT                        YES                                   
*                                                                               
         LH    R2,CURDISP                 FOR PFKEYS                            
         AR    R2,RA                                                            
         LA    R0,ORLSEL1H                CURSOR SHOULD BE WITHIN LIST          
         CR    R2,R0                                                            
         JL    INVLDPF                    SET INVALID PF KEY                    
         LA    R0,ORLPFLNH                PFLINE                                
         CR    R2,R0                      PAST PFLINE?                          
         JNL   INVLDPF                    YES - ERROR                           
         CLI   PFKEY,PFCMGQ               CANCEL A MAKEGOOD?                    
         BE    *+12                       YES                                   
         CLI   PFKEY,PFCMGAQ              CANCEL A MAKEGOOD?                    
         BNE   GETPF50                    NO                                    
         CLI   KEY+1,X'36'                MAKEGOOD RECORD?                      
         BNE   INVLDPF                    NO - ERROR                            
         B     GETPF60                    DONE                                  
*                                                                               
GETPF50  CLI   KEY+1,X'36'                MAKEGOOD RECORD?                      
         BE    INVLDPF                    YES - ERROR                           
*                                                                               
GETPF60  SR    R0,R0                      GET FIRST BYTE                        
         LH    R1,CURDISP                 OF SELECT FIELD FOR CURSOR            
         AHI   R1,-(ORLSEL1H-T234FFD)                                           
         LA    R3,LINLNQ                                                        
         DR    R0,R3                                                            
         MHI   R1,LINLNQ                                                        
         AHI   R1,(ORLSEL1H-T234FFD)                                            
         STCM  R1,3,CURSOUT                                                     
*                                                                               
         NI    TRNSTAT,X'FF'-(RACHANG)    GLOBBER/MULTI SELECT PROB             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         AHI   R0,PFACTLQ                                                       
         STC   R0,PFKEY                                                         
*                                                                               
         OI    CTLRFLG1,CF1TSELQ          DON'T TEST THE SEL CODES              
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         DC    H'0'                       DIDN'T EXPECT TO GET HERE             
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
SSV      NTR1                                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
                                                                                
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
                                                                                
***********************************************************************         
* ROUTINE TO SET PFKEYS                                              *          
***********************************************************************         
SETPF    NTR1  ,                                                                
         XC    ORLPFLN(PFDATAQ),ORLPFLN   SET PF KEY TEXT                       
         MVC   ORLPFLN(PFDATA1Q),PFDATA1  SET PF KEY TEXT                       
         CLI   KEY+1,X'36'                MAKEGOOD RECORD?                      
         BE    *+10                       YES                                   
         MVC   ORLPFLN(PFDATAQ),PFDATA    SET PF KEY TEXT                       
         OI    ORLPFLNH+6,X'80'           SET TRANSMIT PF FIELD                 
         J     XIT                        RETUNR TO CALLER                      
*                                                                               
RELOTAB  DS    0D                                                               
         DC    AL4(ORDSTAB),AL4(AORDSTAB-LSSD)                                  
         DC    X'FF'                                                            
*                                                                               
PFDATA   DC C'PF2=Order Status PF3=Send Method PF4=Supplementary Data '         
         DC C'PF6=Order History'                                                
PFDATAQ  EQU   *-PFDATA                                                         
PFDATA1  DC C'PF5=Cancel MakeGood'                                              
PFDATA1Q EQU   *-PFDATA1                                                        
*                                                                               
         LTORG                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                        *         
***********************************************************************         
PFENTQ   EQU   0                          'ENTER'                               
PFSTAQ   EQU   2                          STATUS                                
PFOSMQ   EQU   3                          SEND METHOD                           
PFSDAQ   EQU   4                          SUPPLEMENTARY DATA                    
PFCMGQ   EQU   5                          CANCEL MAKEGOOD                       
PFHISQ   EQU   6                          PATCH HISTORY                         
PFRTNQ   EQU   12                         RETURN                                
PFACTLQ  EQU   12                         'SHIFT' KEY ADDS 12                   
PFSTAAQ  EQU   PFSTAQ+PFACTLQ             STATUS          14                    
PFSMDAQ  EQU   PFOSMQ+PFACTLQ             SEND METHOD     15                    
PFSDAAQ  EQU   PFSDAQ+PFACTLQ             SUPP DATA       16                    
PFCMGAQ  EQU   PFCMGQ+PFACTLQ             CANCEL MAKEGOOD 17                    
PFHISAQ  EQU   PFHISQ+PFACTLQ             PATCH HISTORY   18                    
*                                                                               
PFTABLE  DS    0C                                                               
* ORDER STATUS                                                                  
         DC    AL1(PF02X-*,PFSTAQ,0,0,0,PFTRETRN)                               
         DC    CL3'STA',CL8' ',CL8' '                                           
PF02X    EQU   *                                                                
                                                                                
* ORDER SEND METHOD                                                             
         DC    AL1(PF03X-*,PFOSMQ,0,0,0,PFTRETRN)                               
         DC    CL3'OSM',CL8' ',CL8' '                                           
PF03X    EQU   *                                                                
                                                                                
* SUPPLEMENTARY DATA                                                            
         DC    AL1(PF04X-*,PFSDAQ,0,0,0,PFTRETRN)                               
         DC    CL3'SDA',CL8' ',CL8' '                                           
PF04X    EQU   *                                                                
                                                                                
* CANCEL MAKEGOOD                                                               
         DC    AL1(PF05X-*,PFCMGQ,0,0,0)                                        
         DC    CL3'CMG',CL8' ',CL8' '                                           
PF05X    EQU   *                                                                
                                                                                
* ORDER PATCH HISTORY                                                           
         DC    AL1(PF06X-*,PFHISQ,0,0,0)                                        
         DC    CL3'HIS',CL8' ',CL8' '                                           
PF06X    EQU   *                                                                
                                                                                
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,PFRTNQ,PFTRPROG,0,0,0)                               
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
* ACTUAL STATUS                                                                 
*                                                                               
         DC    AL1(PF14X-*,PFSTAAQ,PFTCPROG,0,(PF14X-PF14)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'PSTATUS'                                   
PF14     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'ORLORDR-1),AL2(ORLORDR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'LINCLT-1),AL2(LINCLT-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINPRD-1),AL2(LINPRD-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINEST-1),AL2(LINEST-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINFLT-1),AL2(LINFLT-LINSTT1)                     
         DC    AL1(KEYTYWS,L'MKT-1),AL2(MKT-LSSD)                               
         DC    AL1(KEYTYCUR,L'LINSTA-1),AL2(LINSTA-LINSTT1)                     
PF14X    EQU   *                                                                
*                                                                               
* ACTUAL SEND METHOD                                                            
*                                                                               
         DC    AL1(PF15X-*,PFSMDAQ,PFTCPROG,0,(PF15X-PF15)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'PSEND'                                     
PF15     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'ORLORDR-1),AL2(ORLORDR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'LINCLT-1),AL2(LINCLT-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINPRD-1),AL2(LINPRD-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINEST-1),AL2(LINEST-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINFLT-1),AL2(LINFLT-LINSTT1)                     
         DC    AL1(KEYTYWS,L'MKT-1),AL2(MKT-LSSD)                               
         DC    AL1(KEYTYCUR,L'LINSTA-1),AL2(LINSTA-LINSTT1)                     
PF15X    EQU   *                                                                
*                                                                               
* ACTUAL SUPPLEMENTARY DATA                                                     
*                                                                               
         DC    AL1(PF16X-*,PFSDAAQ,PFTCPROG,0)                                  
         DC    AL1((PF16X-PF16)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'PSUPP'                                     
PF16     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'ORLORDR-1),AL2(ORLORDR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'LINCLT-1),AL2(LINCLT-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINPRD-1),AL2(LINPRD-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINEST-1),AL2(LINEST-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINFLT-1),AL2(LINFLT-LINSTT1)                     
         DC    AL1(KEYTYWS,L'MKT-1),AL2(MKT-LSSD)                               
         DC    AL1(KEYTYCUR,L'LINSTA-1),AL2(LINSTA-LINSTT1)                     
PF16X    EQU   *                                                                
*                                                                               
* CANCEL A MAKEGOOD                                                             
*                                                                               
         DC    AL1(PF17X-*,PFCMGAQ,PFTCPROG,0)                                  
         DC    AL1((PF17X-PF17)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'PMKGD'                                     
PF17     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'ORLORDR-1),AL2(ORLORDR-T234FFD)                   
         DC    AL1(KEYTYTWA,L'ORLMKGD-1),AL2(ORLMKGD-T234FFD)                   
PF17X    EQU   *                                                                
*                                                                               
* PATCH HISTORY                                                                 
*                                                                               
         DC    AL1(PF18X-*,PFHISAQ,PFTCPROG,0)                                  
         DC    AL1((PF18X-PF18)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'PHISTORY'                                  
PF18     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'ORLORDR-1),AL2(ORLORDR-T234FFD)                   
PF18X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                 *         
* ON ENTRY:    PARAM 1             A(EBCDIC ORDER NUMBER)             *         
* ON EXIT:     BINORDER            BINARY ORDER NUMBER                *         
***********************************************************************         
BINORDR  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         CLI   1(R2),C'3'                                                       
         BNH   BNORDR10                                                         
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         J     XIT                                                              
*                                                                               
BNORDR10 GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
*                                                                               
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,(R2),DUB,8   SAVE AS IF ENTRY WAS HEX                 
         MVC   PACKOF4B,DUB            CONVERT IT TO PACK                       
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),DUB   STICK IN DAYS IN YEAR                        
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
         J     XIT                                                              
*                                                                               
         LTORG                            LTORG                                 
         DROP  R7,RB                                                            
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
INVLFLD  MVI   GERROR1,INVALID                                                  
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
ERRNOORD MVI   GERROR1,215         CANNOT FIND THAT ORDER                       
         J     ERREXIT                                                          
*                                                                               
INVLDPF  MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         J     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         J     INFEXIT                                                          
***********************************************************************         
* MESSAGE ROUTINES                                                              
***********************************************************************         
ERRRTEXT LA    R1,1                                                             
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    R3,GETTXTCB                                                      
         USING GETTXTD,R3                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         LTR   R1,R1                                                            
         JZ    MYINFXIT                                                         
         J     ERREXIT                                                          
         DROP  R3                                                               
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
* ORDER STATUS TABLE                                                            
***********************************************************************         
ORDSTAB  DS    0H                                                               
*                                  ** UNSENT **                                 
ORDUNST  DC    AL1(QUNSENT,ORDUNSTX-*+1),CL6'UNSENT'                            
         DC    AL1(0,0,ORD2XREV)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDUNSTX-*)/L'ORDDATA)                                      
ORDUNSTX EQU   *                                                                
*                                  ** SENT **                                   
ORDSENT  DC    AL1(DSENT,ORDSENTX-*+1),CL6'SENT'                                
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSENTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORDSENTX EQU   *                                                                
*                                  ** EMAIL SENT **                             
ORDEMLS  DC    AL1(DEMSENT,ORDEMLSX-*+1),CL6'EMSENT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REMSNT'                                          
         DC    AL1(MF2VAROR),C'VEMSNT'                                          
ORDEMLSX EQU   *                                                                
*                                  ** FAX SENT **                               
ORDFAXS  DC    AL1(DFXSENT,ORDFAXSX-*+1),CL6'FAXSNT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'RFXSNT'                                          
         DC    AL1(MF2VAROR),C'VFXSNT'                                          
ORDFAXSX EQU   *                                                                
*                                  ** FAX RESENT ONLY SHOWS AS FAX SENT         
ORDFAXRS DC    AL1(DFXRSNT,ORDFXRSX-*+1),CL6'FAXSNT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFXRSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'RFXSNT'                                          
         DC    AL1(MF2VAROR),C'VFXSNT'                                          
ORDFXRSX EQU   *                                                                
*                                  ** APPROVED **                               
ORDAPPR  DC    AL1(QAPP,ORDAPPRX-*+1),CL6'OPENED'                               
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDAPPRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVOPN'                                          
         DC    AL1(MF2VAROR),C'VAROPN'                                          
ORDAPPRX EQU   *                                                                
*                                  ** CONFIRM PENDING **                        
ORDCFPD  DC    AL1(QCFMDPND,ORDCFPDX-*+1),CL6'CFMPND'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCFPDX-*)/L'ORDDATA)                                      
ORDCFPDX EQU   *                                                                
*                                  ** CONFIRMED **                              
ORDCNFM  DC    AL1(QCFMD,ORDCNFMX-*+1),CL6'CNFRMD'                              
         DC    AL1(ORDTNCMT,ORDIFTST+ORDINSTS,ORD2ACFM)                         
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFMX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
         DC    AL1(MF2CANCF),C'CANCFM'                                          
         DC    AL1(MF2REVOR),C'REVCNF'                                          
         DC    AL1(MF2VAROR),C'VARCNF'                                          
ORDCNFMX EQU   *                                                                
*                                  ** CONFIRMED WITH COMMENTS **                
ORDCNFC  DC    AL1(QCFMD,ORDCNFCX-*+1),CL6'**PCFM'                              
         DC    AL1(ORDTCMTS,ORDINSTS,ORD2ACFM)                                  
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFCX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
         DC    AL1(MF2REVOR),C'**RPCF'                                          
         DC    AL1(MF2VAROR),C'**VPCF'                                          
ORDCNFCX EQU   *                                                                
*                                  ** ERROR **                                  
ORDERR   DC    AL1(QERRORED,ORDERRX-*+1),CL6'ERROR'                             
         DC    AL1(0,ORDIXREV+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDERRX-*)/L'ORDDATA)                                       
         DC    AL1(MF2EMAIL),C'EMERR '                                          
         DC    AL1(MF2FAXED),C'FXERR '                                          
ORDERRX  EQU   *                                                                
*                                  ** EMAIL DELIVERED **                        
ORDEMLD  DC    AL1(DEMDLVD,ORDEMLDX-*+1),CL6'EMDLVD'                            
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDEMLDX-*)/L'ORDDATA)                                      
ORDEMLDX EQU   *                                                                
*                                  ** FAX DELIVERED **                          
ORDFAXD  DC    AL1(DFXDLVD,ORDFAXDX-*+1),CL6'FXDLVD'                            
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXDX-*)/L'ORDDATA)                                      
ORDFAXDX EQU   *                                                                
*                                  ** FAX CANCELLED **                          
ORDFAXC  DC    AL1(QFAXCNCL,ORDFAXCX-*+1),CL6'FXERR '                           
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXCX-*)/L'ORDDATA)                                      
ORDFAXCX EQU   *                                                                
*                                  ** MANUAL CONFIRMED **                       
ORDCNFX  DC    AL1(QBYRCNFM,ORDCNFXX-*+1),CL6'BYRCNF'                           
         DC    AL1(0,ORDINSTS+ORDIFTST,ORD2ACFM)                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFXX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
ORDCNFXX EQU   *                                                                
*                                  ** REJECTED **                               
ORDRJCT  DC    AL1(QRJCT,ORDRJCTX-*+1),CL6'RJCTED'                              
         DC    AL1(ORDTRJCT,0,ORD2REJ)                                          
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRJCTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVREJ'                                          
         DC    AL1(MF2VAROR),C'VARREJ'                                          
ORDRJCTX EQU   *                                                                
*                                                                               
ORDAMND  DC    AL1(QRJCT,ORDAMNDX-*+1),CL6'AMEND'                               
         DC    AL1(ORDTAMND,0,ORD2REJ)                                          
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDAMNDX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVAMD'                                          
         DC    AL1(MF2VAROR),C'REVAMD'                                          
ORDAMNDX EQU   *                                                                
*                                  ** EMPTY **                                  
ORDEMPT  DC    AL1(QEMPTY,ORDEMPTX-*+1),CL6'EMPTY'                              
         DC    AL1(0,ORDIXCLD,0)                                                
         DC    C'K'                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDEMPTX-*)/L'ORDDATA)                                      
ORDEMPTX EQU   *                                                                
*                                  ** NOT DARED **                              
ORDNODA  DC    AL1(QNODARE,ORDNODAX-*+1),CL6'NTDARE'                            
         DC    AL1(0,ORDIFTST+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDNODAX-*)/L'ORDDATA)                                      
ORDNODAX EQU   *                                                                
*                                  ** RECALLED **                               
ORDRECA  DC    AL1(QRECALL,ORDRECAX-*+1),CL6'RECALL'                            
         DC    AL1(0,ORDIRCDA+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRECAX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRECAX EQU   *                                                                
*                                  ** RECALLED APPROVED **                      
ORDRCAP  DC    AL1(QRCLAPPR,ORDRCAPX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCAPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCAPX EQU   *                                                                
*                                  ** RECALLED - CONFIRMED **                   
ORDRCCF  DC    AL1(QRCLCONF,ORDRCCFX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDISENT+ORDISRCL+ORDINSTS,0)                              
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCCFX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCCFX EQU   *                                                                
*                                  ** RECALLED - DELIVERED **                   
ORDRCDE  DC    AL1(QRCLDELN,ORDRCDEX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCDEX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCDEX EQU   *                                                                
*                                  ** RECALLED - REJECTED **                    
ORDRCRJ  DC    AL1(QRCLREJD,ORDRCRJX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDISENT+ORDISRCL+ORDINSTS,0)                              
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCRJX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCRJX EQU   *                                                                
*                                  ** RECALLED UNKNOWN **                       
ORDRCUK  DC    AL1(QRCLUNKN,ORDRCUKX-*+1),CL6'ERROR'                            
         DC    AL1(0,ORDIXREV+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCUKX-*)/L'ORDDATA)                                      
ORDRCUKX EQU   *                                                                
*                                  ** UNDARED **                                
ORDUNDA  DC    AL1(QUNDARE,ORDUNDAX-*+1),CL6'UNDARD'                            
         DC    AL1(0,ORDIFTST+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDUNDAX-*)/L'ORDDATA)                                      
ORDUNDAX EQU   *                                                                
*                                  ** RECALLED - TRANSMITTED **                 
ORDRCTR  DC    AL1(QRCLTRNS,ORDRCTRX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCTRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCTRX EQU   *                                                                
*                                  ** RECALLED - WIP **                         
ORDRCWP  DC    AL1(QRCLWIP,ORDRCWPX-*+1),CL6'RECALL'                            
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCWPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCWPX EQU   *                                                                
*                                  ** SENT PENDING **                           
ORDSNTP  DC    AL1(QSNTPNDG,ORDSNTPX-*+1),CL6'SENT'                             
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2FAXED),C'FAXSNT'                                          
         DC    AL1(MF2EMAIL),C'EMLSNT'                                          
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORDSNTPX EQU   *                                                                
*                                  ** SENT CANCELLED PCFM **                    
ORDSNTC  DC    AL1(QSNTXCNF,ORDSNTCX-*+1),CL6'**PCFM'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTCX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'**RPCF'                                          
         DC    AL1(MF2VAROR),C'**VPCF'                                          
ORDSNTCX EQU   *                                                                
*                                  ** SENT CANCELLED RJCT **                    
ORDSNTR  DC    AL1(QSNTXREJ,ORDSNTRX-*+1),CL6'RJCTED'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVREJ'                                          
         DC    AL1(MF2VAROR),C'VARREJ'                                          
ORDSNTRX EQU   *                                                                
*                                  ** SENT **                                   
ORD2BSN  DC    AL1(QTOBESNT,ORD2BSNX-*+1),CL6'SENT'                             
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORD2BSNX-*)/L'ORDDATA)                                      
         DC    AL1(MF2FAXED),C'FAXSNT'                                          
         DC    AL1(MF2EMAIL),C'EMLSNT'                                          
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORD2BSNX EQU   *                                                                
*                                                                               
ORDEND   DC    AL1(255,ORDENDX-*+1),CL6'ERR999'                                 
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDENDX-*)/L'ORDDATA)                                       
ORDENDX  EQU   *                                                                
*                                                                               
FFS      DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
***********************************************************************         
* LOCAL SAVED STORAGE                                                           
***********************************************************************         
LSSD     DSECT                                                                  
MISCFLG1 DS    XL1                 MISCELLANEOUS CNTLS                          
MF1KYCHG EQU   X'80'                 A KEY FIELD HAS BEEN CHANGED               
MF1RECNF EQU   X'40'                 RECORD WAS NOT FOUND                       
MF1CHNGD EQU   X'20'                 RECORD WAS CHANGED                         
MF1NODAR EQU   X'10'                 SEND A NOTDARE ON PQ                       
MF1XDOSP EQU   X'02'                 DOSP EXT. FIELDS NOT PRESENT/SETUP         
MISCFLG2 DS    XL1                 MISCELLANEOUS CNTLS 2                        
MF2VAROR EQU   X'80'                 WE GOT A VAR ORDER HERE                    
MF2REVOR EQU   X'40'                 WE GOT A REVISED ORDER HERE                
MF2OFFER EQU   X'20'                 CONFIRMED ORDER HAS AN OFFER               
MF2FAXED EQU   X'10'                 PREVIOUSLY FAXED                           
MF2EMAIL EQU   X'08'                 PREVIOUSLY EMAILED                         
MF2AMEND EQU   X'04'                 AMEND STATUS                               
MF2CANCF EQU   X'02'                 CANCELLED CONFIRMED                        
MISCFLG3 DS    XL1                 MISCELLANEOUS CNTLS 3                        
MF3RADIO EQU   X'80'                 WE ARE DOING RADIO                         
MISCFLG4 DS    XL1                                                              
MF4CNFCM EQU   X'80'                 WE GOT A CONFIRM WITH COMMENT              
MF4AMEND EQU   X'40'                 AMEND STATUS                               
RECFLG   DS    XL1                 RECORD CNTLS                                 
RFSTEL   EQU   X'80'                 HAVE STATUS ELEMENT                        
RFRLDLV  EQU   X'40'                 RECALL DELIVERED                           
RFRLNDL  EQU   X'20'                 RECALL NOT DELIVERED                       
RFSTDT   EQU   X'10'                 HAVE A STATUS DATE AND TIME                
RFDLDT   EQU   X'08'                 HAVE A DELIVERED DATE AND TIME             
RFOK     EQU   X'04'                 RECORD PASSED FILTER TEST                  
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                  DATE PORTION                                
BINORDSQ DS    XL2                  SEQUENCE PORTION                            
CHRORDER DS    0CL8                CHARACTER ORDER NUMBER                       
CHRORDDT DS    CL4                  DATE PORTION                                
CHRORDSQ DS    CL4                  SEQUENCE PORTION                            
CURSOUT  DS    XL2                 DISPLACEMENT TO OUTPUT CURSOR                
SVR2     DS    F                                                                
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
SAVEKEY  DS    XL(L'KEY)                                                        
FLDH     DS    CL8                 FIELD HEADER AND FIELD DATA                  
FLD      DS    CL60                                                             
BASERC   DS    A                                                                
RELO     DS    A                   A(RELO)                                      
VGLOBBER DS    A                   A(GLOBBER)                                   
AORDSTAB DS    A                   A(ORDER TYPE TABLE)                          
ADOSTETL DS    A                   A(DOSTETEL)                                  
TOTDOL   DS    CL10                TOTAL DOLLARS                                
TOTSPOT  DS    CL4                 TOTAL SPOTS                                  
MKT      DS    CL4                 MARKET                                       
DAD      DS    XL4                 DISK ADDRESS                                 
CLRDATE  DS    XL3                 DATE ON COLOR ELEMENT                        
SVSTATN  DS    XL3                                                              
LSTTAB   DS    XL(LSTLNQ)          LIST TABLE                                   
*                                                                               
         ORG   LSSD+L'SYSSPARE                                                  
***********************************************************************         
* DSECT TO COVER ORDER LIST ENTRY                                     *         
***********************************************************************         
LSTD     DSECT                                                                  
LSTORD   DS    XL(L'DOKORDER)             ORDER                                 
LSTSTC   DS    XL6                        STATUS CODE                           
LSTBYR   DS    XL(L'DOIDBYR)              BUYER                                 
LSTCLT   DS    XL(L'DOIDCLT)              CLIENT                                
LSTPRD   DS    XL(L'DOIDPRD)              PRODUCT NUMBER                        
LSTPR2   DS    XL(L'DOIDPRD2)             PRODUCT - 2 NUMBER                    
LSTPRDC  DS    CL3                        PRODUCT CODE                          
LSTEST   DS    XL(L'DOIDEST)              ESTIMATE                              
LSTSTA   DS    XL(L'DOISTA)               STATION                               
LSTMKT   DS    XL(L'BMKT)                 MARKET                                
LSTMKN   DS    XL(L'MKTNM)                MARKET NAME                           
LSTFLT   DS    XL(L'DOIDFLTN)             FLIGHT                                
LSTACT   DS    0XL(L'DOSTDATE+L'DOSTTIME)  ACTIVITY DATE + TIME                 
LSTACTD  DS    XL(L'DOSTDATE)              ACTIVITY DATE                        
LSTACTT  DS    XL(L'DOSTTIME)                       TIME                        
*                                                                               
LSTODT   DS    XL(L'DOSTDATE)             ORDER DATE                            
LSTFLG   DS    XL1                        STATUS - CNTLS                        
LSTFSTR  EQU   X'80'                       DISPLAY A '*'                        
LSTFMGO  EQU   X'40'                       MG OFFER - DISPLAY 'X'               
LSTTRDE  EQU   X'20'                       TRADE ORDER DISPLAY 'T'              
LSTCLR   DS    CL1                        COLOR                                 
LSTCLS   DS    XL1                        COLOR COLLATING SEQUENCE              
LSTDAD   DS    XL4                        DISK ADDRESS                          
LSTLNQ   EQU   *-LSTD                                                           
***********************************************************************         
* DSECT TO COVER SCREEN LINE                                          *         
***********************************************************************         
*                                                                               
LINDSECT DSECT                                                                  
LINSELH  DS    CL(L'ORLSEL1H)                                                   
LINSEL   DS    CL(L'ORLSEL1)                                                    
LINSTTH  DS    CL(L'ORLSTT1H)                                                   
LINSTT1  DS    C                                                                
LINSTC   DS    CL(L'ORLSTT1-1)                                                  
LINOFIH  DS    CL(L'ORLOFI1H)                                                   
LINOFI   DS    CL(L'ORLOFI1)                                                    
LINSTAH  DS    CL(L'ORLSTA1H)                                                   
LINSTA   DS    CL(L'ORLSTA1)                                                    
LINMKNH  DS    CL(L'ORLMKN1H)                                                   
LINMKN   DS    CL(L'ORLMKN1)                                                    
LINCLTH  DS    CL(L'ORLCLT1H)                                                   
LINCLT   DS    CL(L'ORLCLT1)                                                    
LINPRDH  DS    CL(L'ORLPRD1H)                                                   
LINPRD   DS    CL(L'ORLPRD1)                                                    
LINESTH  DS    CL(L'ORLEST1H)                                                   
LINEST   DS    CL(L'ORLEST1)                                                    
LINFLTH  DS    CL(L'ORLFLT1H)                                                   
LINFLT   DS    CL(L'ORLFLT1)                                                    
LINODTH  DS    CL(L'ORLODT1H)                                                   
LINODT   DS    CL(L'ORLODT1)                                                    
LINADTH  DS    CL(L'ORLADT1H)                                                   
LINADT   DS    CL(L'ORLADT1)                                                    
LINBYRH  DS    CL(L'ORLBYR1H)                                                   
LINBYR   DS    CL(L'ORLBYR1)                                                    
LINORDH  DS    CL(L'ORLORD1H)                                                   
LINORD   DS    CL(L'ORLORD1)                                                    
LINGRPH  DS    CL(L'ORLGRP1H)                                                   
LINGRP   DS    CL(L'ORLGRP1)                                                    
LINNEXTL DS    0C                                                               
LINLNQ   EQU   *-LINDSECT                                                       
*                                                                               
***********************************************************************         
* DSECT TO COVER ORDER STATUS TABLE                                             
***********************************************************************         
QUNSENT  EQU   1                                                                
QSENT    EQU   2                                                                
QFAXSNT  EQU   3                                                                
*                                                                               
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER STATUS                                 
ORDLN    DS    XL1                 LENGTH OF TABLE ENTRY                        
ORDDFLT  DS    CL6                 DEFAULT CODE TO DISPLAY                      
ORDTYP   DS    XL1                 SECONDARY TYPE COMPARE                       
ORDTCMTS EQU   X'80'               WITH COMMENTS                                
ORDTNCMT EQU   X'40'               NO COMMENTS                                  
ORDTAMND EQU   X'20'               AMEND                                        
ORDTRJCT EQU   X'10'               REJECT                                       
ORDIND   DS    XL1                 INDICATORS                                   
ORDIFTST EQU   X'80'               STATUS FILTER TEST                           
ORDISENT EQU   X'40'               SEND '*' TO LINSTT1                          
ORDIRCL  EQU   X'20'               INCLUDE IF RECALL FILTER                     
ORDISRCL EQU   X'10'               INCLUDE IF *RECALL FILTER                    
ORDIRCDA EQU   X'08'               INCLUDE/EXCLUDE RECALL BY DATE               
ORDIXREV EQU   X'04'               EXCLUDE IF 'REVISED' FILTER                  
ORDIXCLD EQU   X'02'               EXCLUDE UNLESS FILTERED BY                   
ORDINSTS EQU   X'01'               NO STATUS TEST                               
ORDIND2  DS    XL1                 SECOND INDICATOR                             
ORD2XREV EQU   X'80'               EXCLUDE FROM REVISED                         
ORD2ACFM EQU   X'40'               INCLUDE ON CONFIRM FILTER                    
ORD2REJ  EQU   X'20'               INCLUDE ON REJECT FILTER                     
ORDCOLR  DS    CL1                 COLOR CODE                                   
         DS    XL2                 N/D                                          
ORDNFLG  DS    XL1                 NUMBER OF CNTLS TO TEST                      
ORDDATA  DS    0XL7                                                             
ORDFLG   DS    XL1                 FLAG                                         
ORDCODE  DS    CL6                 CODE                                         
*                                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSB0D          (OUR PATCH SCREEN)                           
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* SPOMSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
* SPGENCLT                                                                      
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
* SPGENDRMKN                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRMKN                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
*                                                                               
GEND     DSECT                                                                  
*                                                                               
         ORG   LISTAR                DEFAULT                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPOMS10   01/09/15'                                      
         END                                                                    
