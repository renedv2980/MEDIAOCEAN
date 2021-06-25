*          DATA SET SRNWK00    AT LEVEL 041 AS OF 09/27/19                      
*PHASE T13500A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE TIMBER                                                                 
*INCLUDE WKSCAN                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE DECODE                                                                 
         TITLE '$WK - ROOT CONTROLLER'                                          
         PRINT NOGEN                                                            
WRKF000  CSECT                                                                  
         NMODL NWKWKX-NWKWKD,**$WK0**,RA,R9,RR=R4,CLEAR=YES                     
         USING NWKWKD,RC                                                        
         ST    RB,ABASE                                                         
         STM   R9,RB,ABASES        SAVE BASE VALUES                             
         ST    RD,SAVERD                                                        
         ST    R1,APARM                                                         
         ST    R4,RELOBASE                                                      
*                                                                               
WRKF010  L     R2,4(R1)                                                         
         ST    R2,ATIA             SAVE A(TIA)                                  
         LA    R2,SRCOMWRK-SRSD(R2)                                             
         ST    R2,ASAVESTR                                                      
         USING WKSAVED,R2          R2=A(SAVE STORAGE)                           
*                                                                               
WRKF020  L     RF,8(R1)                                                         
         ST    RF,AUTL             SAVE A(UTL)                                  
         L     RF,28(R1)                                                        
         ST    RF,ATIOB            SAVE A(TIOB)                                 
         L     RF,12(R1)                                                        
         ST    RF,ACOMFACS         SAVE A(COMFACS)                              
         L     R3,20(R1)                                                        
         USING SRNWKFFD,R3         R3=A(TWA)                                    
         L     R8,00(R1)                                                        
         ST    R8,ASYSFAC                                                       
         USING SYSFACD,R8          R8=A(SYS FAC LIST)                           
*                                                                               
WRKF030  L     RF,ACOMFACS         SAVE COMFACS ROUTINES                        
         USING COMFACSD,RF                                                      
         MVC   AGETFACT,CGETFACT                                                
         MVC   ADATCON,CDATCON                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   AGETTXT,CGETTXT                                                  
         MVC   AGETHELP,CGETHELP                                                
         MVC   ADICTATE,CDICTATE                                                
         MVC   ATERMVAL,CTERMVAL                                                
         MVC   AUNSCAN,CUNSCAN                                                  
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ADATVAL,CDATVAL                                                  
         MVC   APERVERT,CPERVERT                                                
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   AHEXIN,CHEXIN                                                    
         DROP  RF                                                               
WRKF040  MVC   AUTL1,VUTL          SAVE SYSFACS ADDRESSES                       
         MVC   ASSB,VSSB                                                        
         MVC   FIWENQ,VISGENQ                                                   
*                                                                               
         GOTO1 ADATAMGR,DMCB,=CL8'SHMUSS',(0,=CL8'ATTACH'),            +        
               (0,=CL8'WRKF'),0,0                                               
         ICM   R1,15,DMCB+12       A(SHARED MEMORY)                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,FIWSHA                                                        
*                                                                               
WRKF050  L     RF,=V(SQUASHER)     SAVE OTHER ROUTINES                          
         A     RF,RELOBASE                                                      
         ST    RF,ASQUASH                                                       
         L     RF,=V(NUMVAL)                                                    
         A     RF,RELOBASE                                                      
         ST    RF,ANUMVAL                                                       
         L     RF,=V(TIMBER)                                                    
         A     RF,RELOBASE                                                      
         ST    RF,ATIMBER                                                       
         L     RF,=V(WKSCAN)                                                    
         A     RF,RELOBASE                                                      
         ST    RF,AWKSCAN                                                       
         L     RF,=V(DECODE)                                                    
         A     RF,RELOBASE                                                      
         ST    RF,ADECODE                                                       
         GOTO1 VCALLOV,DMCB,0,X'D9000AFA',0                                     
         MVC   AGETIDS,0(R1)                                                    
         GOTO1 VCALLOV,DMCB,0,X'D9000AFC',0                                     
         MVC   AGENIDS,0(R1)                                                    
*                                                                               
WRKF060  LA    RF,GETUSER          COMMON SUBROUTINES                           
         ST    RF,AGETUSER                                                      
         LA    RF,LOADSCR                                                       
         ST    RF,ALOADSCR                                                      
         LA    RF,SELTABL                                                       
         ST    RF,ASELTABL                                                      
         LA    RF,RIDXPND                                                       
         ST    RF,ARIDXPND                                                      
         LA    RF,WFLOCK                                                        
         ST    RF,AWFLOCK                                                       
         LA    RF,WKUPDT                                                        
         ST    RF,AWKUPDT                                                       
*                                                                               
WRKF070  L     RF,=A(CTREC-NWKWKD) SAVE OUT OF RANGE WORK                       
         LA    RF,NWKWKD(RF)                                                    
         ST    RF,ACTREC                                                        
         L     RF,=A(CXREC-NWKWKD)                                              
         LA    RF,NWKWKD(RF)                                                    
         ST    RF,ACXREC                                                        
         L     RF,=A(CIREC-NWKWKD)                                              
         LA    RF,NWKWKD(RF)                                                    
         ST    RF,ACIREC                                                        
         ST    RF,AFILTAB                                                       
*                                                                               
WRKF080  MVI   DDS,0                                                            
         MVI   INTFLAG,0                                                        
         XC    STATCH,STATCH                                                    
         MVI   PAGFLAG,FF                                                       
         MVC   HELPKEY,HELPID                                                   
         MVC   WRKFID,=CL8'WRKF1'                                               
*                                                                               
WRKF090  L     RF,VSSB             SAVE SSB DATA                                
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL      TWA LEN                                      
         L     R1,SSBTKADR                                                      
         ST    R1,ATCB             SAVE A(TCB)                                  
         SR    R1,R1                                                            
         IC    R1,SSBSYSID                                                      
         SLL   R1,4                                                             
         STC   R1,SYSID            SET FACPAK SYSID                             
         DROP  RF                                                               
*                                                                               
WRKF100  L     RF,AUTL             SAVE UTL DATA                                
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         MVC   TRMTYP,TSTAT                                                     
         MVC   TRMTYP1,TTYPE                                                    
         MVC   TRMCTRY,TAGCTRY                                                  
         MVC   LOGONID,TUSER                                                    
         MVC   LOGONAG,TAGY                                                     
*                                                                               
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
         OI    DDS,DDSNEW          FORCE INIT                                   
         DROP  RF                                                               
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         ICM   R0,3,TIOBCURD                                                    
         AR    R0,R3               ADD TWA ADDRESS                              
         ST    R0,CURSOR                                                        
         MVC   CURADR,TIOBCURS                                                  
*                                                                               
WRKF130  SR    R0,R0               READ PF KEY VALUE                            
         IC    R0,TIOBAID                                                       
         CH    R0,=H'12'           CONVERT 13-24 TO 1-12                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE IN PFKEY                                
         DROP  RF                                                               
*                                                                               
WRKF140  L     RF,=A(DDDCLST)      RESOLVE DICTIONARY REFERENCES                
         A     RF,RELOBASE                                                      
         GOTO1 ADICTATE,DMCB,C'LU  ',(RF),DDDSLST                               
*                                                                               
         L     RF,AUTL             SAVE UTL DATA                                
         USING UTLD,RF                                                          
*                                                                               
         OC    SRVOPTN,=CL80' '                                                 
         CLC   =C'STRATA ',SRVOPTN    STRATA REQUEST?                           
         BNE   WRKF148                NO                                        
*                                                                               
         MVC   USERID,=H'6183'     SET STRATA ID                                
         TM    TSTAT1,X'60'        TEST DDS TERMINAL                            
         BZ    *+8                                                              
         OI    DDS,DDSTRM                                                       
         B     WRKF150                                                          
*                                                                               
WRKF148  TM    TSTAT1,X'60'        TEST DDS TERMINAL                            
         BZ    ERR5                NO - SHOULD HAVE BEEN STRATA                 
         OI    DDS,DDSTRM                                                       
         MVC   USERID,TUSER        YES - SET TO CONNECTED ID                    
         DROP  RF                                                               
                                                                                
*----------------------------------------------------------------------         
* CREATE LIST OF AVAILABLE WRKF FILES                                           
*----------------------------------------------------------------------         
WRKF150  LA    RF,L'W_INDEX        SAVE WRKFL DATA                              
         STH   RF,CINDXLN                                                       
         L     RF,VENQDEQ                                                       
         ST    RF,CIENQDEQ                                                      
*                                                                               
WRKF160  L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,WKDMCB,(X'00',GLIST),WRKFID,NDX,ACTREC,(R5)             
         ICM   RE,15,NXUSRINF                                                   
         CLI   0(RE),0             CHECK NUM OF WRKF FILES                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF NOT IN MY RANGE                       
         CLI   0(RE),17                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               R1=NUM OF FILES IN LIST                      
         IC    R1,0(RE)                                                         
         SR    R0,R0                                                            
         IC    R0,1(RE)            NUMBER OF EXTRA FILES                        
         AR    R1,R0                                                            
         LA    R1,2(R1)            ADD TWO FOR HDR AND TRL                      
         SLL   R1,3                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WRKFLST(0),0(RE)    COPY WRKF LIST TO MY OWN AREA                
         XC    WRKFLSTX,WRKFLSTX   SET END OF MAXIMUM LIST                      
                                                                                
*----------------------------------------------------------------------         
* GET FILE FOR USERID & INITIALISE BUFFER                                       
*----------------------------------------------------------------------         
WRKF170  LA    R4,SRVIDH           SET R4 FOR ERR EXIT                          
         OC    USERID,USERID                                                    
         BNZ   WRKF175             GET WKFILE FOR USERID                        
         TM    DDS,DDSTRM                                                       
         BZ    ERR0                NON DDS TRMS MUST BE CONNECTED               
*                                                                               
         MVC   WRKFCHR,WRKFNTRY+1  DEFAULT TO FIRST WRKF FILE                   
         B     WRKF176                                                          
*                                                                               
WRKF175  MVC   NXSRCID,USERID      GET WRKF ID FROM USER ID                     
         GOTO1 VDATAMGR,WKDMCB,(X'00',GFILE)                                    
         MVC   WRKFID,NXUSRINF                                                  
         MVC   WKPASS,WRKFCHR                                                   
*                                                                               
WRKF176  BAS   RE,SETBUFF          INIT BUFFER                                  
                                                                                
*----------------------------------------------------------------------         
* SET UP DATES (TODAYS)                                                         
*----------------------------------------------------------------------         
WRKF180  GOTO1 ADATCON,DMCB,(5,0),(0,DATE)                                      
         GOTO1 (RF),(R1),,(1,DATE1)                                             
         GOTO1 (RF),(R1),,(2,DATE2)                                             
         GOTO1 (RF),(R1),,(30,DATE30)   TODAY NCD                               
                                                                                
*----------------------------------------------------------------------         
* SET UP TIA AND SAVED STORAGE                                                  
*----------------------------------------------------------------------         
WRKF190  L     R5,ATIA             READ AND LOCK TWA11 INTO TIA                 
         USING SRSD,R5                                                          
         LA    R4,SRPAGENO                                                      
         SLL   R4,32-8                                                          
         ICM   R4,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R4),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRKF192  DS    0H                                                               
*                                                                               
         CLC   WKSID,=C'T135'      ID MUST BE GOOD ELSE INIT                    
         BE    *+8                                                              
         OI    DDS,DDSNEW          FLAG NEW SESSION                             
*                                                                               
         LA    R0,WKSAVED          TEST FOR INITIALISE SAVE STORAGE             
         LH    R1,=Y(WKSLEN)                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         TM    DDS,DDSNEW                                                       
         BZ    *+6                                                              
         MVCL  R0,RE               CLEAR SAVE STORAGE                           
*                                                                               
         LA    RE,SV$NWK                                                        
         AH    RE,=Y(NWKBITTB-SV$NWK)                                           
         LA    RF,2048                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TM    DDS,DDSNEW                                                       
         BZ    *+6                                                              
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,SV$NWK                                                        
         AH    RE,=Y(NWKMODE-SV$NWK)                                            
         TM    DDS,DDSNEW                                                       
         BZ    *+8                                                              
         MVI   0(RE),0                                                          
*                                                                               
         MVC   WKSID,=C'T135'      SET OR RESET WF ID                           
                                                                                
*----------------------------------------------------------------------         
* MAIN PROGRAM  (SCAN FOR HELP & VALIDATE P1)                                   
*----------------------------------------------------------------------         
MAIN000  BAS   RE,HELPSCAN         SCAN FOR ?                                   
*                                                                               
MAIN010  GOTO1 P1VAL,SRVP1H        VALIDATE P1                                  
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY                                    
*                                                                               
         CLI   WKACT,X'31'                                                      
         BE    BROW010                                                          
         CLI   WKACT,X'32'                                                      
         BE    BROW010                                                          
*                                                                               
         CLI   WKACT,X'2D'         IF SELECT                                    
         BNE   MAIN011                                                          
         TM    WKSFLAG,WKSRET      THIS CAN BE A RETURN FROM SEL                
         BO    RETURN                                                           
*                                                                               
MAIN011  LA    R1,SRVP2H           TEST FOR CURSOR POS                          
         CLC   CURADR,2(R1)                                                     
         BNL   MAIN012                                                          
         OI    DDS,DDSNEW          REREAD QUEUE IF CURSOR IN P1                 
*                                                                               
MAIN012  CLC   WKACT,WKSACT        TEST THIS ACT WITH LAST ACT                  
         BE    MAIN013                                                          
         OI    DDS,DDSNEW          CHANGED ACTION                               
*                                                                               
         LA    RF,SRVP4H           NO TEST P4 FOR CURSOR                        
         CLC   CURADR,2(RF)                                                     
         BH    MAIN013             CURSOR IS PAST P4                            
         XC    SRVP4,SRVP4         ELSE LOSE P4                                 
         MVI   SRVP4H+5,0                                                       
         LA    RF,SRVP3H           TEST P3 FOR CURSOR                           
         CLC   CURADR,2(RF)                                                     
         BH    MAIN013             CURSOR IS PAST P3                            
         XC    SRVP3,SRVP3         ELSE LOSE P3                                 
         MVI   SRVP3H+5,0                                                       
*                                                                               
MAIN013  MVC   WRKSACT,WRKACT      COPY VALUES TO SAVE                          
                                                                                
*----------------------------------------------------------------------         
* VALIDATE P2 CHECK P3 & P4                                                     
*----------------------------------------------------------------------         
MAIN020  GOTO1 P2VAL,SRVP2H        VALIDATE P2                                  
MAIN021  GOTO1 RIDXPND,SRVP2H      REDISPLAY                                    
*                                                                               
         CLC   WKFILID,WKSFILID    TEST WITH PREV FILE ID                       
         BE    *+8                                                              
         OI    DDS,DDSNEW          CHANGED FILE ID                              
         MVC   WKSFILID,WKFILID    SAVE THIS FILE ID                            
*                                                                               
         CLI   WKDDSFN,C'T'        TEST T=INPUT                                 
         BNE   *+8                                                              
         OI    DDS,DDSTOTL DDSNEW  FLAG TOTALS (FORCE NEW AS WELL)              
         EJECT                                                                  
*                                                                               
P3CHECK  MVI   WKSCHP3,X'FF'       HAS P3 CHANGED                               
         MVC   WORK,SPACES         MOVE SPACES TO WORK                          
         SR    R1,R1                                                            
         ICM   R1,1,SRVP3H+5       GET LEN INTO R1                              
         BZ    P3CHK1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE DATA TO WORK                            
         B     P3CHK1                                                           
         MVC   WORK(0),SRVP3                                                    
P3CHK1   CLC   WKSP3FLD,WORK       TEST WITH PREVIOUS                           
         BE    P3CHKX                                                           
         MVC   WKSP3FLD,WORK       SAVE NEW FIELD                               
         B     *+8                                                              
P3CHKX   MVI   WKSCHP3,0                                                        
*                                                                               
P4CHECK  MVI   WKSCHP4,X'FF'       HAS P4 CHANGED                               
         MVC   WORK,SPACES         MOVE SPACES TO WORK                          
         SR    R1,R1                                                            
         ICM   R1,1,SRVP4H+5       GET LEN INTO R1                              
         BZ    P4CHK1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE DATA TO WORK                            
         B     P4CHK1                                                           
         MVC   WORK(0),SRVP4                                                    
P4CHK1   CLC   WKSP4FLD,WORK       TEST WITH PREVIOUS                           
         BE    P4CHKX                                                           
         MVC   WKSP4FLD,WORK       SAVE NEW FIELD                               
         B     *+8                                                              
P4CHKX   MVI   WKSCHP4,0                                                        
*                                                                               
         OC    WKSCHANG,WKSCHANG   HAVE P1-P4 CHANGED                           
         BZ    *+8                                                              
         OI    DDS,DDSNEW          SET RE-READ FLAG                             
*                                                                               
         BAS   RE,SETUSR           CHECK FOR RESET USERID                       
*                                                                               
         CLI   HLPFLD,6            INTERCEPT HELP CALL                          
         BNL   LOADER                                                           
*                                                                               
         CLI   WKACT,X'21'         IS ACTION DISPLAY                            
         BNE   LOADER                                                           
         OC    WKSCHANG,WKSCHANG   ARE ALL FIELDS UNCHANGED                     
         BNZ   LOADER                                                           
                                                                                
*----------------------------------------------------------------------         
* CHECK ACTN FIELDS FOR SELECT ACTIONS                                          
*----------------------------------------------------------------------         
VALSEL   CLI   WKACT,X'21'         CHECK ACTION IS DISPLAY TYPE                 
*                                                                               
VALSEL1  SR    RE,RE               POINT R7 TO CISAVED AREA                     
         IC    RE,WKSPAGE                                                       
         MHI   RE,16*L'CISAVE      RE=INDEX TO CI ADDRS                         
         LA    R7,WKSCIADS                                                      
         AR    R7,RE               R7=CI ADDRS                                  
         USING CISAVED,R7                                                       
         OC    CISADR(2),CISADR    TEST WE HAVE A CIADDR                        
         BZ    VALSELX                                                          
*                                                                               
         LA    R6,SRVSA1H          R6=FIRST SEL FIELD                           
         XC    HALF,HALF                                                        
         XC    BYTE1,BYTE1         CLR CONTINUATION FLAG                        
*                                                                               
VALSEL2  LA    R1,SELTABL                                                       
         TM    DDS,DDSTOTL                                                      
         BO    *+22                                                             
         XC    CISACTN,CISACTN     CLEAR ACTN FIELDS                            
         XC    CISNUM,CISNUM                                                    
         XC    CISFLAG,CISFLAG                                                  
*                                                                               
         CLI   PFKEY,4             WAS PF4 HIT                                  
         BNE   VALSEL3                                                          
         MVI   BYTE,3                                                           
         CLC   2(2,R6),CURADR      ARE WE ON THE FIELD                          
         BE    ACTNOW              YES - GO DO IT                               
*                                                                               
VALSEL3  ST    R1,FULL             SAVE A(TABLE ENTRY)                          
         SR    RE,RE               RE=INPUT LEN                                 
         ICM   RE,1,5(R6)                                                       
         BZ    VALSEL10            ZERO FORGET IT                               
         BCTR  RE,0                                                             
         TM    5(R1),X'40'         IF NUMERIC TABLE ENTRY                       
         BNO   *+6                                                              
         SR    RE,RE               JUST VALIDATE 1 CHR                          
         EX    0,0(R1)             RF=A(KEYWORD)                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R6),0(RF)       KEYWORK COMPARE                              
         BNE   VALSEL4                                                          
         IC    RF,5(R1)            GET MIN LEN FROM TABLE                       
         N     RF,=F'3'                                                         
         CR    RE,RF               CHECK MIN LENGTH                             
         BL    VALSEL4                                                          
         TM    5(R1),X'80'         DDS ONLY ACTION                              
         BNO   *+12                                                             
         TM    DDS,DDSTRM          CHECK DDS TERM                               
         BNO   VALSEL4                                                          
         B     VALSEL5             SUBACT IS VALID                              
*                                                                               
VALSEL4  LA    R1,L'SELTABL(R1)    NEXT TABLE ENTRY                             
         CLI   0(R1),0                                                          
         BNE   VALSEL3             LOOP BACK TO VALIDATION                      
         B     VALSEL10                                                         
*                                                                               
VALSEL5  TM    5(R1),X'40'         IF NUMERIC TABLE ENTRY                       
         BZ    VALSEL6                                                          
*                                                                               
         LA    RF,9(R6)                                                         
         SR    RE,RE               GET I/P LEN-1                                
         ICM   RE,1,5(R6)                                                       
         BCTR  RE,0                                                             
         GOTO1 ANUMVAL,PARMS,(RF),(RE)                                          
         CLI   PARMS,0             VALIDATE NUMBER                              
         BNE   VALSEL10                                                         
         SR    RF,RF               GET VALUE IN RF                              
         ICM   RF,15,PARMS+4                                                    
         BZ    VALSEL6                                                          
         BP    VALSEL5A            POSITIVE NUMBER IS OK                        
         L     R1,FULL                                                          
         TM    5(R1),X'20'         IF NEGATIVE TEST THIS IS OK                  
         BNO   VALSEL10                                                         
*                                                                               
VALSEL5A LPR   RE,RF               RE=ABS VALUE                                 
         CH    RE,=H'255'          RANGE IS -127 TO 255                         
         BH    VALSEL10                                                         
         STC   RE,CISNUM           PUT NUMBER INTO CI TAB                       
         LTR   RF,RF                                                            
         BNM   *+8                                                              
         OI    CISNUM,X'80'        SET -VE BIT                                  
*                                                                               
VALSEL6  L     R1,FULL                                                          
         SR    RF,RF                                                            
         IC    RF,7(R1)                                                         
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALRT1              U SET USERID                                 
         B     VALRT2              T SET T=ALL                                  
         B     VALRT3              * SET TO DONE                                
         B     VALRT4              " COPY PREV ACTION                           
         B     VALRT5              '+' TO PRINT & CONTINUATION                  
         B     VALRT6              NORMAL ACTION SEL/DIS/ACT/HOL                
*                                                                               
VALRT1   MVI   BYTE,1              USER ACTION                                  
         B     ACTNOW                                                           
*                                                                               
VALRT2   MVI   BYTE,2              TOTAL ACTION                                 
         B     ACTNOW                                                           
*                                                                               
VALRT3   OI    CISFLAG,ACTDONE                                                  
         B     VALSEL10                                                         
*                                                                               
VALRT4   MVC   CISACTN(2),HALF     RESTORE PREVIOUS ACTION DETAILS              
         NI    CISFLAG,ACTPNTG                                                  
         B     VALSEL10                                                         
*                                                                               
VALRT5   DC    H'0'                PROBABLY DEAD                                
*                                                                               
VALRT6   MVC   CISACTN,4(R1)       ACTION                                       
         NI    CISFLAG,ACTPNTG     JUST REMEMBER IF PRINTING                    
         MVC   HALF+0(1),CISACTN   SAVE DETAILS IN CASE OF DITTO                
         MVC   HALF+1(1),CISNUM    SAVE DETAILS IN CASE OF DITTO                
         OI    INTFLAG,INTRUN      FLAG SOMETHING TO DO                         
         B     VALSEL10                                                         
*                                                                               
* NEXT FIELD NEXT CI OR EXIT                                                    
*                                                                               
VALSEL10 LA    R6,(2*L'SRVSA1H)+L'SRVSA1+L'SRVRL1(R6)                           
         LA    R1,SRVPFKH          R1=END OF SCREEN                             
         CR    R6,R1                                                            
         BNL   VALSELX             OK END OF SCREEN                             
*                                                                               
         LA    R7,L'CISAVE(R7)     NEXT CI-SAVED ADDR                           
         CLC   CISAVE,FFS                                                       
         BNE   VALSEL2             STILL MORE CIS                               
*                                                                               
VALSELX  TM    INTFLAG,INTRUN      ANY ACTIONS?                                 
         BO    ACT00                                                            
         B     LOADER                                                           
                                                                                
***********************************************************************         
* TAKE IMMEDIATE ACTION ON THIS COMMAND                                         
***********************************************************************         
ACTNOW   XC    8(4,R6),8(R6)       CLR FIELD                                    
         ST    R6,CURSOR                                                        
         MVC   WRKFCHR,CISWRKF                                                  
         BAS   RE,GETFILT          READ RECORD INTO INDEX AREA                  
         L     R6,ACXREC                                                        
         USING W_RECD,R6                                                        
*                                                                               
         CLI   BYTE,1                                                           
         BE    ACTUSR                                                           
         CLI   BYTE,2                                                           
         BE    ACTTOT                                                           
         CLI   BYTE,3                                                           
         BE    ACTBRO                                                           
         DC    H'0'                                                             
*                                                                               
ACTUSR   MVI   INTFLAG,0                                                        
         NI    DDS,255-DDSTOTL     REMOVE TOTALS FLAG IF SET                    
         OI    DDS,DDSNEW          MUST REREAD PQ                               
         MVI   WKSPAGE,0                                                        
         MVC   WKUSER,W_USRID                                                   
         MVI   WKDDSFN,C'U'                                                     
         B     MAIN021                                                          
*                                                                               
ACTTOT   MVI   INTFLAG,0                                                        
         OI    DDS,DDSTOTL         SET TOTALS FLAG                              
         OI    DDS,DDSNEW          MUST REREAD PQ                               
         MVI   WKSPAGE,0                                                        
         XC    WKUSER,WKUSER                                                    
         MVI   WKDDSFN,C'T'                                                     
         B     MAIN021                                                          
*                                                                               
ACTBRO   MVI   WKACT,X'31'         SET BROWSE                                   
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY P1                                 
         XC    SRVP3,SRVP3         LOSE P3                                      
         MVI   SRVP3H+5,0                                                       
         XC    SRVP4,SRVP4         LOSE P4                                      
         MVI   SRVP4H+5,0                                                       
         MVC   CIPASS,CIADDR                                                    
         L     R1,CURSOR                                                        
         SR    R1,R3                                                            
         STCM  R1,3,WKSCURS        SAVE CURSOR OFFSET                           
         B     LOADER                                                           
                                                                                
***********************************************************************         
* HANDLE BROWSE ACTION                                                          
***********************************************************************         
BROW010  GOTO1 P2VAL,SRVP2H        VALIDATE P2                                  
         GOTO1 RIDXPND,SRVP2H      REDISPLAY                                    
         BAS   RE,SETUSR                                                        
         CLI   PFKEY,4             PF4 CAN RETURN FROM BROWSE                   
         BNE   LOADER                                                           
                                                                                
*----------------------------------------------------------------------         
* RETURN FROM INTERNAL ACTION                                                   
*----------------------------------------------------------------------         
RETURN   NI    WKSFLAG,255-WKSRET  TURN OFF RETURN FLAG                         
*                                                                               
         CLI   WKACT,X'2D'         SELECT ?                                     
         BNE   RETURN1                                                          
*                                                                               
         LA    R1,SRVP3H           GET NEW STATUS                               
         CLI   HLPFLD,4            TEST STATUS HELP                             
         BE    HELPOUT                                                          
         CLI   5(R1),0             IF ANYTHING IN P3                            
         BE    RETURN1                                                          
*                                                                               
         OI    INTFLAG,INTCONT+INTRUN+INTSEL                                    
         GOTO1 P1VAL,SRVP3H        GET NEW STATUS                               
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         GOTO1 P2VAL,SRVP2H        VALIDATE FILE ID                             
*                                                                               
         BAS   RE,SETUSR                                                        
         B     LOADER                                                           
*                                                                               
RETURN1  MVC   WKACT,WKSACT        RESTORE P1-P2                                
         MVC   WKFILID,WKSFILID                                                 
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY P1                                 
         GOTO1 RIDXPND,SRVP2H      REDISPLAY P2                                 
*                                                                               
         BAS   RE,SETUSR           CHECK FOR RESET USERID                       
                                                                                
*----------------------------------------------------------------------         
* PERFORM INTERNAL ACTIONS                                                      
*----------------------------------------------------------------------         
ACT00    MVI   SBYTE,1             SET PASS1                                    
ACT00A   XC    CIPASS,CIPASS       CLEAR CIPASS                                 
         LA    R7,WKSCIADS                                                      
         TM    INTFLAG,INTERR      TEST FOR ERROR                               
         BO    ACTXIT              PREMATURE EXIT                               
*                                                                               
         MVI   INTFLAG,0           CLEAR ALL INTERNALS                          
*                                                                               
ACT001   OC    CISACTN,CISACTN     TEST FOR ACTION                              
         BZ    ACT001A                                                          
         TM    CISFLAG,ACTDONE     SEE IF PROCESSED                             
         BNO   ACT002                                                           
ACT001A  LA    R7,L'CISAVE(R7)                                                  
         CLC   CISAVE,FFS                                                       
         BNE   ACT001                                                           
         CLI   SBYTE,2             EXIT IF PASS2                                
         BE    ACTXIT                                                           
         MVI   SBYTE,2             OR SET TO PASS2                              
         B     ACT00A                                                           
*                                                                               
ACT002   MVC   WKACT,CISACTN       COPY ACTION AND NUMBER                       
         MVC   WKACT2,CISNUM                                                    
         MVC   WKPASS,CISWRKF                                                   
*                                                                               
         MVC   CIPASS(2),CISADR                                                 
         MVC   CIPASS+2(2),=X'0100'                                             
*                                                                               
ACT00X   L     R1,ASELTABL         POINT TO SELTABL                             
         OI    INTFLAG,INTRUN      SET FOR INTERNAL RUNNING                     
*                                                                               
ACT00X1  CLC   WKACT,4(R1)         FIND SEL ACTION                              
         BE    ACT00X2                                                          
         LA    R1,L'SELTABL(R1)    NEXT                                         
         CLI   0(R1),0                                                          
         BNE   ACT00X1                                                          
         DC    H'0'                UNKNOWN SEL ACTION                           
*                                                                               
ACT00X2  CLI   SBYTE,2             ARE WE ON PASS2                              
         BE    LOADER              GO DO IT                                     
*                                                                               
         TM    5(R1),X'10'         CONTINUE AFTER ACTION                        
         BNO   ACT001A             SAVE THESE FOR PASS2                         
         OI    INTFLAG,INTCONT     SET CONTINUE FLAG                            
         B     LOADER                                                           
*                                                                               
ACTXIT   MVI   WKACT,X'21'         FORCE TO DISPLAY                             
         GOTO1 RIDXPND,SRVP2H      REDISPLAY P2                                 
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY P1                                 
*                                                                               
         MVC   SRVP3(15),WKSP3FLD  RESTORE P3 & P4                              
         OI    SRVP3H+6,X'80'                                                   
         MVC   SRVP4,WKSP4FLD                                                   
         OI    SRVP4H+6,X'80'                                                   
*                                                                               
         MVI   PAGFLAG,X'FF'       ZAP PAGE FLAG                                
         NI    INTFLAG,INTERR      CLR ALL FLAGS EXCEPT ERROR                   
         OI    INTFLAG,INTRUN      SET INTRUN                                   
         OI    INTFLAG,INTLAST     AND SET LAST                                 
         NI    DDS,255-DDSNEW      ENSURE RELOAD FLAG HAS CLEARED               
                                                                                
*----------------------------------------------------------------------         
* LOAD OVERLAY AND EXECUTE IT                                                   
*----------------------------------------------------------------------         
LOADER   CLI   HLPFLD,0            CHECK HELP ACTION                            
         BNE   HELPOUT                                                          
*                                                                               
         TM    INTFLAG,INTRUN      TEST INTERNAL                                
         BNO   LOADER1                                                          
         TM    INTFLAG,INTCONT     TEST CONTINUE FLAG                           
         BO    LOADER1                                                          
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY ACTION                             
*                                                                               
LOADER1  IC    R0,WKACT            LOAD OVERLAY PHASE                           
         SRL   R0,4                                                             
         SLL   R0,28                                                            
         SRL   R0,4                                                             
         GOTO1 VCALLOV,DMCB,(R0),(R3)                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            PASS CONTROL TO OVERLAY                      
         LR    R1,RC               R1=A(ROOT WORKING STORAGE)                   
         BASR  RE,RF                                                            
                                                                                
*----------------------------------------------------------------------         
* CLEANUP & EXIT OR LOOP BACK FOR MORE                                          
*----------------------------------------------------------------------         
         TM    INTFLAG,INTSEL      NEW STATUS                                   
         BNO   *+20                                                             
         MVC   WKACT,WKSACT        RESTORE P1-P2                                
         MVC   WKFILID,WKSFILID                                                 
         B     ACT00               CONTINUE                                     
*                                                                               
         TM    INTFLAG,INTRUN                                                   
         BNO   INTSAVE             EXIT NORMAL ACTION                           
         TM    INTFLAG,INTLAST                                                  
         BO    INTSAVE             EXIT REDISPLAY LAST                          
         TM    INTFLAG,INTERR                                                   
         BNO   INTOK                                                            
*                                                                               
         OI    CISFLAG,ACTERR+ACTDONE     FLAG ENTRY ERROR DONE                 
         B     ACT00               RETURN TO DISPLAY SCREEN                     
*                                                                               
INTOK    OI    CISFLAG,ACTDONE     SET ACTION OK                                
         TM    INTFLAG,INTCONT     TEST CONTINUE                                
         BO    ACT00                                                            
*                                                                               
         OI    WKSFLAG,WKSRET      SET RETURN FLAG                              
*                                                                               
INTSAVE  L     R5,ATIA             WRITE SAVE PAGE                              
         USING SRSD,R5                                                          
         CLC   =C'STRATA ',SRVOPTN    STRATA REQUEST?                           
         BNE   INTSAV10               NO                                        
*                                                                               
         LA    RE,SV$NWK                                                        
         AH    RE,=Y(NWKMODE-SV$NWK)                                            
         MVI   0(RE),NWKMDEMO                                                   
*                                                                               
INTSAV10 DS    0H                                                               
         LA    R4,SRPAGENO                                                      
         SLL   R4,32-8                                                          
         ICM   R4,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(R4),SRSD                            
*                                                                               
EXIT     OC    MSGNUM,MSGNUM       CHECK FOR NO MESSAGE                         
         BZ    EXIT1                                                            
         XC    DMCB(24),DMCB       PUT OUT PRESET MESSAGE                       
         MVC   DMCB+2(2),MSGNUM                                                 
         MVC   DMCB+8(1),MSGTYP                                                 
         MVC   DMCB+12(4),TXTADR                                                
         LA    R1,MSGXTRA                                                       
         ST    R1,DMCB+16                                                       
         MVI   DMCB+21,1                                                        
         GOTO1 AGETTXT,DMCB                                                     
EXIT1    L     R4,CURSOR           SET CURSOR BIT                               
         OI    6(R4),X'40'                                                      
*                                                                               
         TM    CURFLAG,X'80'       TEST CURSOR FLAG                             
         BZ    EXITX                                                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR FROM CURADR                       
         XC    TIOBCURI,TIOBCURI                                                
         XC    TIOBCURD,TIOBCURD                                                
         MVC   TIOBCURS,CURADR                                                  
         DROP  RF                                                               
*                                                                               
EXITX    XMOD1                                                                  
                                                                                
***********************************************************************         
* CALL GETHELP AND EXIT                                                         
***********************************************************************         
HELPOUT  LA    R1,HLPIND7                                                       
         CLI   HLPFLD,7            CHECK FOR SEL FIELDS                         
         BNL   HELP005                                                          
         SR    RF,RF                                                            
         IC    RF,HLPFLD           READ HELP FIELD                              
         SLL   RF,2                                                             
         EX    0,HLPIND0(RF)       SELECT WHICH TABLE                           
         B     HELP010                                                          
HLPIND0  DC    XL4'00'             NO FIELD NUMBER                              
         LA    R1,HLPIND1                                                       
         LA    R1,HLPIND2                                                       
         LA    R1,HLPIND3                                                       
         LA    R1,HLPIND4                                                       
         LA    R1,HLPIND5                                                       
         LA    R1,HLPIND6                                                       
*                                                                               
HELP005  L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
         MVI   TIOBCURI,X'01'      ONE PAST                                     
         LA    RE,SRVP4H           FILE FILT                                    
         SR    RE,R3                                                            
         STCM  RE,3,TIOBCURD                                                    
         XC    TIOBCURS,TIOBCURS                                                
         DROP  RF                                                               
*                                                                               
HELP010  CLC   0(2,R1),=H'0'                                                    
         BE    INF0                                                             
         CLC   0(1,R1),WKACT       MATCH ACTION                                 
         BE    HELP020                                                          
         CLI   0(R1),X'FF'         FF MATCHES ANY                               
         BE    HELP020                                                          
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     HELP010                                                          
HELP020  MVC   SBYTE,1(R1)         SBYTE=INDEX                                  
         LA    R1,HELPTAB                                                       
HELP030  CLC   SBYTE,0(R1)                                                      
         BNE   HELP040                                                          
         TM    1(R1),X'80'         DDS PANEL ?                                  
         BNO   HELP031                                                          
         TM    HLPFLG,X'80'                                                     
         BNO   HELP040                                                          
HELP031  MVC   HELPNUM,2(R1)                                                    
         MVC   HELPPAG,HLPPAG                                                   
         SR    RF,RF                                                            
         TM    1(R1),X'40'         EXTRA TEXT ?                                 
         BNO   *+8                                                              
         L     RF,ACIREC           EXTRA TEXT IS IN CIREC                       
         L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         GOTO1 AGETHELP,DMCB,(X'50',HELPKEY),QHDR,(C'B',0),(RF),0               
         DC    H'0'                GETHELP EXITS TO MONITOR                     
HELP040  LA    R1,L'HELPTAB(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   HELP030                                                          
         B     INF0                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ON ENTRY CISADR NEEDED                                                        
***********************************************************************         
GETFILT  NTR1                                                                   
*                                                                               
         L     R6,ACXREC                                                        
         USING W_RECD,R6                                                        
         OC    CISADR(2),CISADR                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CIADDR(2),CISADR                                                 
         MVC   CIADDR+2(2),=X'0100'                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',BUFFER),WRKFID,NDX,ACTREC,(R6)              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R6)                                                   
         MVC   CIDATA(L'CIDATA),12(R6)                                          
         L     RE,8(R6)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R6                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFWKINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUWFFILE                                   
         XC    CTREC(12),CTREC                                                  
         MVC   CTREC+4(4),=C'REC '                                              
         GOTO1 VDATAMGR,DMCB,(X'00',RANDOM)                                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ACTION EXPAND    NTR  R1=A(FLDHDR)                                            
***********************************************************************         
ACTNXPND NTR1                                                                   
*                                                                               
         ST    R1,SFULL                                                         
         LA    R4,SWORK            USE SWORK FOR EXPAND                         
         XC    SWORK,SWORK                                                      
*                                                                               
         SR    RE,RE               XC O/P FIELD                                 
         IC    RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         MVI   5(R1),0                                                          
*                                                                               
         CLI   WKQUEUE,0           TEST FOR Q=U                                 
         BE    ACTX010                                                          
         MVC   0(4,R4),=C'Q=U '                                                 
         MVC   2(1,R4),WKQUEUE                                                  
         LA    R4,4(R4)                                                         
*                                                                               
ACTX010  LA    R7,ACTNTBL          EXPAND ACTION                                
ACTX011  CLC   4(1,R7),WKACT                                                    
         BE    ACTX020                                                          
         LA    R7,L'ACTNTBL(R7)    TRY NEXT                                     
         CLI   0(R7),0                                                          
         BNE   ACTX011                                                          
         DC    H'0'                                                             
ACTX020  EX    0,0(R7)             RF=A(ACTION)                                 
         MVC   0(8,R4),0(RF)       MOVE KEYWORD                                 
         MVI   9(R4),C' '                                                       
         LA    R4,9(R4)                                                         
*                                                                               
         CLI   WKACT1,0            ARE WE FINISHED YET                          
         BE    ACTX990                                                          
         CLI   WKACT1,X'FF'        TEST FOR NUMERIC SUB ACTION                  
         BNE   ACTX030                                                          
         EDIT  (B1,WKACT2),(3,0(R4)),WRK=SWORK1,DUB=SDUB                        
         B     ACTX990                                                          
*                                                                               
ACTX030  ICM   R7,7,7(R7)          LOCATE SUB ACTION KEYWORD                    
         A     R7,RELOBASE                                                      
ACTX031  CLC   5(1,R7),WKACT1                                                   
         BE    ACTX040                                                          
         LA    R7,L'SACTLEN(R7)                                                 
         CLI   0(R7),0                                                          
         BNE   ACTX031                                                          
         DC    H'0'                                                             
*                                                                               
ACTX040  EX    0,0(R7)             RF=A(SACTION)                                
         MVC   0(8,R4),0(RF)       MOVE KEYWORD                                 
         MVI   8(R4),C' '                                                       
*                                                                               
ACTX990  L     R4,SFULL            RECOVER FIELD                                
         GOTO1 ASQUASH,DMCB,SWORK,(C',',25)                                     
         LA    RF,16                                                            
         CLI   7(R1),16            15 IS MAXIMUM LEN                            
         BH    *+8                                                              
         IC    RF,7(R1)                                                         
         STC   RF,5(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),SWORK       MOVE SWORK INTO FIELD                        
XIT1     XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* ACTION VALIDATION    R1=A(FLDHDR)                                             
***********************************************************************         
P1VAL    NTR1                                                                   
*                                                                               
         LR    R4,R1                                                            
         ST    R4,CURSOR                                                        
         XC    WRKACT,WRKACT       CLEAR RETURN AREA                            
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BE    INF1                MISSING ACTION                               
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(3,(R6))                                      
         MVC   SBYTE,4(R1)                                                      
         CLI   SBYTE,0                                                          
         BE    ERR2                INVALID ACTION                               
         TM    DDS,DDSTRM                                                       
         BNO   P1V1                                                             
*                                                                               
P1VPQ    CLI   1(R6),0             FIRST PARM CAN BE Q=WRKFID                   
         BE    P1V1                                                             
         CLI   12(R6),C'Q'                                                      
         BNE   ERR2                                                             
         CLI   1(R6),1             WRKFID IS 1 CHR FIELD                        
         BNE   ERR2                                                             
         CLI   22(R6),C'U'         Q=U MEANS THIS USERS QUEUE                   
         BE    P1V015                                                           
         LA    RE,WRKFLST+8        MATCH CHR AGAINST WRKF LIST                  
P1V010   CLI   0(RE),0                                                          
         BE    ERR10                                                            
         CLC   22(1,R6),1(RE)                                                   
         BE    P1V015                                                           
         LA    RE,8(RE)                                                         
         B     P1V010                                                           
*                                                                               
P1V015   MVC   WKQUEUE,22(R6)      SAVE Q=CHR                                   
         SR    RF,RF               DROP Q=WRKFID FIELD                          
         IC    RF,SBYTE                                                         
         SH    RF,=H'1'            MUST HAVE AT LEAST ONE MORE                  
         BZ    ERR2                                                             
         STC   RF,SBYTE            RESET NUMBER OF FIELDS                       
         LA    R6,32(R6)                                                        
*                                                                               
P1V1     ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         CLI   1(R6),0             PART#1 IS ACTION                             
         BNE   ERR2                                                             
         CH    R1,=H'2'            ACTION IS 3 THRU 8 CHRS                      
         BL    ERR2                                                             
         CH    R1,=H'7'                                                         
         BH    ERR2                                                             
         LA    R7,ACTNTBL                                                       
P1V1A    CLI   0(R7),0             SEARCH ACTION TABLE                          
         BE    ERR2                                                             
         CLM   R1,1,5(R7)          IS I/P LEN > KEYWORD                         
         BNL   P1V1A1              IF SO DONT COMPARE                           
         TM    6(R7),X'40'         TEST FOR DDS ONLY ACTION                     
         BNO   *+12                                                             
         TM    DDS,DDSTRM          MUST BE DDS TERMINAL                         
         BNO   P1V1A1                                                           
         TM    6(R7),X'80'         TEST FOR VALID NEW STATUS                    
         BO    *+12                                                             
         TM    INTFLAG,INTRUN      IF INTRUN ONLY ACCEPT NEW STATUS             
         BO    P1V1A1                                                           
         EX    0,0(R7)             RF=A(ACTION)                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)      COMPARE KEYWORD                              
         BNE   P1V1A1                                                           
         MVC   WKACT,4(R7)         SAVE ACTION VALUE                            
         B     P1V2                                                             
*                                                                               
P1V1A1   LA    R7,L'ACTNTBL(R7)    TRY NEXT                                     
         B     P1V1A                                                            
*                                                                               
P1V2     CLI   SBYTE,1             PART#2 IS SUB ACTION                         
         BE    P1VX                                                             
         LA    R6,32(R6)                                                        
         ICM   R7,7,7(R7)          TEST IF SUB ACTION ALLOWED                   
         BZ    ERR8                                                             
         C     R7,=F'1'            1 MEANS INTEGER VALUE ALLOWED                
         BE    P1V3                                                             
         A     R7,RELOBASE         ELSE IS ADDRESS OF A TABLE                   
         CLI   1(R6),0                                                          
         BNE   ERR8                                                             
         CLI   0(R6),1             SUB ACTION IS 1 THRU 8 CHRS                  
         BL    ERR8                                                             
         CLI   0(R6),8                                                          
         BH    ERR8                                                             
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
P1V2A    CLI   0(R7),0             SEARCH SUB ACTION TABLE                      
         BE    ERR8                                                             
         EX    0,0(R7)             RF=A(SACTION)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P1V2B                                                            
         LA    R7,L'SACTLEN(R7)                                                 
         B     P1V2A                                                            
P1V2B    MVC   WKACT1,5(R7)        SAVE SUB ACTION VALUE                        
         B     P1VX                                                             
*                                                                               
P1V3     MVI   WKACT1,X'FF'        SET INTEGER VALUE IN ACTN2                   
         CLI   1(R6),0                                                          
         BNE   ERR8                                                             
         CLI   0(R6),1             SUB ACTION IS INTEGER 1-3 DIGITS             
         BL    ERR2                                                             
         CLI   0(R6),3                                                          
         BH    ERR8                                                             
         TM    2(R6),X'80'         MUST BE NUMERIC IN RANGE 1-255               
         BZ    ERR8                                                             
         L     RE,4(R6)                                                         
         C     RE,=F'1'                                                         
         BL    ERR8                                                             
         C     RE,=F'255'                                                       
         BH    ERR8                                                             
         STC   RE,WKACT2           SAVE INTEGER VALUE IN ACTN2                  
P1VX     B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXPAND FILE ID      R1=A(FLDHDR)                                              
***********************************************************************         
RIDXPND  NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         L     R8,ASYSFAC                                                       
*                                                                               
         ST    R1,SFULL                                                         
         LA    R4,SWORK            USE SWORK FOR EXPAND                         
         XC    SWORK,SWORK                                                      
*                                                                               
         CLC   8(3,R1),=C'DA='     IGNORE IF DA= IN FIELD                       
         BE    XIT1                                                             
*                                                                               
         SR    RE,RE               XC O/P FIELD                                 
         IC    RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         MVI   5(R1),0                                                          
*                                                                               
         CLI   WKDDSFN,0           ANY U= OR T=                                 
         BE    RIDX020                                                          
RIDX001  MVC   0(1,R4),WKDDSFN                                                  
         MVI   1(R4),C'='                                                       
*                                                                               
         OC    WKUSER,WKUSER       TEST FOR ALL                                 
         BZ    RIDX011                                                          
         MVC   GIUSER,WKUSER                                                    
         BAS   RE,GETUSER          GET USER ID                                  
         MVC   2(8,R4),GIUSERID                                                 
         B     RIDX01X                                                          
*                                                                               
RIDX011  MVC   2(8,R4),SR8ALL      SET U= T= ALL                                
RIDX01X  LA    R4,11(R4)                                                        
*                                                                               
RIDX020  OC    WKSYSPG(4),WKSYSPG  TEST FOR ZEROS                               
         BZ    RIDX021                                                          
         MVC   0(4,R4),WKSYSPG     MOVE IN CHRS                                 
         LA    R4,4(R4)                                                         
         B     RIDX030                                                          
RIDX021  MVC   0(7,R4),SR8ALL      MOVE IN ALL                                  
         LA    R4,9(R4)                                                         
*                                                                               
RIDX030  CLI   WKDAY,0             TEST FOR ZERO DAY                            
         BE    RIDX039                                                          
         CLI   WKDAY,X'5C'         TEST FOR "*"                                 
         BE    RIDX038                                                          
         CLI   WKDAY,X'C1'         TEST FOR A-9                                 
         BNL   RIDX038                                                          
         IC    R1,WKDAY            MUST BE PACKED DAY                           
         MVC   1(1,R4),WKDAY                                                    
         OC    1(1,R4),=X'F0'                                                   
         SRL   R1,4                                                             
         STC   R1,0(R4)                                                         
         OC    0(1,R4),=X'F0'                                                   
         LA    R4,2(R4)                                                         
         B     RIDX040                                                          
RIDX038  MVC   0(1,R4),WKDAY                                                    
RIDX039  LA    R4,1(R4)                                                         
*                                                                               
RIDX040  CLI   WKCLASS,0           ANY CLASS ?                                  
         BE    RIDX050                                                          
         MVC   0(1,R4),WKCLASS     MOVE IN CLASS VALUE                          
         TM    WKCLASS,X'40'                                                    
         BO    RIDX04X                                                          
         MVI   0(R4),C'-'          IF LOWER CASE SET -CLASS                     
         MVC   1(1,R4),WKCLASS                                                  
         OI    1(R4),X'40'                                                      
         LA    R4,1(R4)                                                         
RIDX04X  LA    R4,2(R4)                                                         
*                                                                               
RIDX050  OC    WKFILEN,WKFILEN     EDIT SEQUENCE NUMBER                         
         BZ    RIDX060                                                          
         SR    R0,R0                                                            
         ICM   R0,3,WKFILEN                                                     
         EDIT  (R0),(6,0(R4)),ZERO=NOBLANK,DUB=SDUB,WRK=SWORK1                  
         B     RIDX990                                                          
*                                                                               
RIDX060  OC    WKTIMES,WKTIMES     OR TIMBER OUT TIME VALUES                    
         BZ    RIDX990                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,WKTIMES                                                     
         SLL   RF,2                * 4                                          
         D     RE,=F'180'                                                       
         STCM  RF,3,SDUB                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,WKTIMES+2                                                   
         SLL   RF,2                * 4                                          
         D     RE,=F'180'                                                       
         STCM  RF,3,SDUB+2                                                      
         LA    R1,20                                                            
         ST    R1,PARMS                                                         
         OC    WKTIMES(2),WKTIMES                                               
         BNZ   *+14                                                             
         OI    PARMS,X'40'                                                      
         MVC   SDUB(2),SDUB+2                                                   
         GOTO1 ATIMBER,PARMS,,(X'02',SDUB),(4,SWORK1)                           
         MVC   0(12,R4),SWORK1                                                  
*                                                                               
RIDX990  L     R4,SFULL            RECOVER FIELD                                
         GOTO1 ASQUASH,DMCB,SWORK,(C',',32)                                     
         LA    RF,16                                                            
         CLI   7(R1),32            32 IS MAXIMUM LEN                            
         BH    *+8                                                              
         ICM   RF,1,7(R1)                                                       
         STC   RF,5(R4)                                                         
         BZ    XIT1                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),SWORK       MOVE SWORK INTO FIELD                        
         B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE FILE ID    R1=A(FLDHDR)                                              
***********************************************************************         
P2VAL    NTR1                                                                   
*                                                                               
         LR    R4,R1                                                            
         ST    R4,CURSOR                                                        
         XC    WKFILID,WKFILID      CLEAR RETURN AREA                           
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BNE   P2V001                                                           
         CLI   WKACT,X'28'          SIZE DOES NOT NEED P2                       
         BE    P2V991                                                           
         B     ERR4                 MUST FILE ACTIONS                           
*                                                                               
P2V001   L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(5,(R6))                                      
         MVC   SBYTE,4(R1)                                                      
         CLI   SBYTE,0                                                          
         BE    ERR3                INVALID FILE ID                              
*                                                                               
         CLI   1(R6),0                                                          
         BE    P2V010                                                           
         CLC   12(2,R6),=C'DA'     IS THIS DA=TTTTBB00                          
         BNE   P2V002                                                           
         CLI   1(R6),8             MUST BE 8 CHRS                               
         BNE   P2V002                                                           
         GOTO1 AHEXIN,DMCB,22(R6),CIPASS,8                                      
         CLI   15(R1),4                                                         
         BNE   P2V002                                                           
         B     P2VXX                                                            
P2V002   CLC   12(1,R6),SR@PSWD    1ST PARM CAN BE P=PASSWORD                   
         BNE   P2V010                                                           
         CLI   1(R6),1             PASSWORD IS 1 THRU 6 CHRS                    
         BL    P2V010                                                           
         CLI   1(R6),6                                                          
         BH    P2V010                                                           
         MVC   WKPSWD,22(R6)                                                    
         TM    DDS,DDSTRM          IF DDS TERMINAL                              
         BNO   P2VPSWX                                                          
         CLC   WKPSWD,DDSPSWD      DDSPSWD MATCHES ALL                          
         BNE   P2VPSWX                                                          
         MVC   WKPSWD,FFS                                                       
P2VPSWX  SR    RF,RF               DROP P=PASSWORD FIELD                        
         IC    RF,SBYTE                                                         
         SH    RF,=H'1'                                                         
         BZ    P2V990                                                           
         STC   RF,SBYTE            RESET NUMBER OF FIELDS                       
         LA    R6,32(R6)                                                        
*                                                                               
P2V010   CLI   1(R6),0             TEST FOR X=Y                                 
         BE    P2V020                                                           
         TM    DDS,DDSTRM          MUST BE A DDS TERMINAL                       
         BNO   ERR3                                                             
         CLI   12(R6),C'T'         MUST BE T=                                   
         BE    *+12                                                             
         CLI   12(R6),C'U'         OR U=                                        
         BNE   ERR3                                                             
         MVC   WKDDSFN,12(R6)                                                   
         TM    3(R6),X'80'         TEST U=NNNNNN                                
         BZ    P2V011                                                           
         CLI   1(R6),6                                                          
         BNE   ERR6                MUST BE 6 CHRS                               
         MVC   WKUSER,10(R6)       SAVE U=VALUE                                 
         B     P2V01X                                                           
*                                                                               
P2V011   CLC   22(8,R6),SR8ALL     TEST U=ALL                                   
         BE    P2V01X                                                           
*                                                                               
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,22(R6)                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         BNE   ERR6                                                             
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
P2V012   AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST NUMBER ELEMENT                          
         BNE   *+10                                                             
         MVC   WKUSER,2(R7)        SAVE NUMBER                                  
*                                                                               
         CLI   0(R7),X'07'         TEST 07 ELEMENT                              
         BNE   P2V013                                                           
         TM    2(R7),X'40'         TEST GENERIC ID                              
         BZ    *+8                                                              
         OI    WKUSER,X'80'        SET GENERIC FLAG                             
P2V013   ICM   RE,1,1(R7)                                                       
         BNZ   P2V012                                                           
*                                                                               
P2V01X   BAS   RE,DROPFLD          DROP U=USERID FIELD                          
*                                                                               
P2V020   CLI   1(R6),0             PART#1 IS SYSPRG:SUBPRG                      
         BNE   ERR3                                                             
         CLI   0(R6),2             MUST BE > 2                                  
         BNH   P2V026                                                           
         CLI   0(R6),7             MUST BE < 8                                  
         BH    ERR3                                                             
         CLC   12(4,R6),SR8ALL     IF ALL JUST DROP IT                          
         BE    P2V025                                                           
*                                                                               
P2V021   SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         CLI   0(R6),4             4 IS MOST FOR MOVE                           
         BNH   *+8                                                              
         LA    R1,4                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8              EX INTO SYSPRG FOR IPLEN                     
         B     *+10                                                             
         MVC   WKSYSPG(0),12(R6)                                                
*                                                                               
         CLI   0(R6),5             4 CHRS OR LESS IS OK                         
         BL    P2V025                                                           
         TM    16(R6),X'F0'        DO WE HAVE NUMERIC DAYS                      
         BNO   P2V022                                                           
         TM    17(R6),X'F0'                                                     
         BNO   P2V022                                                           
         MVO   17(1,R6),16(1,R6)                                                
         MVC   WKDAY,17(R6)        MOVE PACKED DAY                              
         CLI   0(R6),6                                                          
         BE    P2V026X                                                          
         MVC   WKCLASS,18(R6)      CLASS AS WELL PERHAPS                        
         B     P2V02X                                                           
*                                                                               
P2V022   MVC   WKDAY,16(R6)        MOVE NON NUMERIC DAY                         
         CLI   0(R6),5                                                          
         BE    P2V026X                                                          
         MVC   WKCLASS,17(R6)      CLASS AS WELL PERHAPS                        
         B     P2V02X                                                           
*                                                                               
P2V025   BAS   RE,DROPFLD          DROP SPPS FIELD                              
*                                                                               
P2V026   TM    12(R6),X'F0'        TEST NUMERIC DAY                             
         BNO   P2V027                                                           
         TM    13(R6),X'F0'                                                     
         BNO   P2V027                                                           
         MVO   13(1,R6),12(1,R6)                                                
         MVC   WKDAY,13(R6)        MOVE PACKED DAY                              
         CLI   0(R6),3                                                          
         BL    P2V026X                                                          
         MVC   WKCLASS,14(R6)      CLASS AS WELL PERHAPS                        
         B     P2V02X                                                           
*                                                                               
P2V026X  BAS   RE,DROPFLD          DROP PACKED DAY FIELD                        
*                                                                               
P2V027   CLI   0(R6),1                                                          
         BNE   P2V028                                                           
         MVC   WKCLASS,12(R6)      SINGLE CHR IS POSITIVE CLASS FILTER          
         CLI   WKCLASS,C'*'                                                     
         BE    P2V02X                                                           
         CLI   WKCLASS,C'A'                                                     
         BL    ERR5                                                             
         B     P2V02X                                                           
*                                                                               
P2V028   CLI   0(R6),2                                                          
         BNE   P2V030                                                           
         CLI   12(R6),C'-'         ALLOW NEGATIVE CLASS FILTER                  
         BNE   P2V030                                                           
         MVC   WKCLASS,13(R6)                                                   
         CLI   WKCLASS,C'*'                                                     
         BE    *+12                                                             
         CLI   WKCLASS,C'A'                                                     
         BL    ERR5                                                             
         NI    WKCLASS,X'BF'       NEGATIVE FILTERS ARE LOWER CASE              
*                                                                               
P2V02X   BAS   RE,DROPFLD          DROP CLASS FIELD                             
*                                                                               
P2V030   TM    2(R6),X'80'         TEST NUMERIC                                 
         BZ    P2V040                                                           
         MVC   WKFILEN,6(R6)       EXTRACT FILENO                               
*                                                                               
P2V03X   BAS   RE,DROPFLD                                                       
*                                                                               
P2V040   SR    RF,RF               ELSE LOOK FOR TIME                           
         IC    RF,0(R6)                                                         
         MVC   SWORK1(22),12(R6)                                                
         GOTO1 ATIMBER,PARMS,(X'80',(RF)),(X'02',SFULL),SWORK1                  
         CLI   0(R1),0             WAS IT A VALID TIME PERIOD                   
         BE    P2V041                                                           
         OI    0(R1),X'40'         TRY FOR SINGLE TIME                          
         OI    DDS,DDSTIME         AND FLAG THIS FACT                           
         GOTO1 (RF),(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   ERR3                                                             
         MVC   SFULL+2(2),SFULL                                                 
         XC    SFULL(2),SFULL                                                   
P2V041   SR    RF,RF                                                            
         ICM   RF,3,SFULL                                                       
         MH    RF,=H'180'          CONVERT TO SECS * 3                          
         SRL   RF,2                DIVIDE BY 4                                  
         STCM  RF,3,WKTIMES                                                     
         ICM   RF,3,SFULL+2                                                     
         MH    RF,=H'180'          CONVERT TO SECS * 3                          
         SRL   RF,2                DIVIDE BY 4                                  
         A     RF,=F'44'           ADD 59 SECS TO T2                            
         STCM  RF,3,WKTIMES+2                                                   
*                                                                               
P2V050   CLI   SBYTE,1                                                          
         BNE   ERR3                                                             
*                                                                               
P2V990   EQU   *                                                                
*                                                                               
P2V991   TM    DDS,DDSTRM                                                       
         BNO   P2VXX               IF A DDS TERMINAL                            
         OC    USERID,USERID                                                    
         BNZ   P2VXX               HAS NOT LOGGED ON                            
         CLI   WKDDSFN,0                                                        
         BNE   P2VXX               AND HAS FORGOTTEN TO INPUT U=?               
         MVI   WKDDSFN,C'U'                                                     
*&&UK*&& MVC   WKUSER,=X'0026'     SET U=DDS1 FOR UK                            
*&&US*&& MVC   WKUSER,=X'002B'     SET U=TCH1 FOR US                            
*                                                                               
P2VXX    B     XIT1                                                             
*                                                                               
DROPFLD  SR    RF,RF               DROP FIELD                                   
         IC    RF,SBYTE                                                         
         SH    RF,=H'1'            IF LAST FIELD EXIT                           
         BZ    P2V990                                                           
         STC   RF,SBYTE            RESET NUMBER OF FIELDS                       
         LA    R6,32(R6)                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* INIT BUFFER FOR WRKFID (ALSO SAVE WRKFID)                                     
***********************************************************************         
SETBUFF  ST    RE,SAVERE                                                        
*                                                                               
         MVC   WRKFIDSV,WRKFID     SAVE FILEID & INIT BUFFER                    
         L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,WKDMCB,(X'00',BUFFER),WRKFID                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ACIREC                                                        
         MVC   BUFFDATA,0(R5)         SAVE V(W_TAB)/V(W_FILE)                   
         MVC   CIDATA(L'CIDATA),12(R5) SAVE FILE DATA FOR THIS WRKF             
         L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHANGE STATUS OF FILE TO STATUS IN STAACT                                     
***********************************************************************         
WKUPDT   NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         L     R8,ASYSFAC                                                       
*                                                                               
         GOTO1 ADATAMGR,WKDMCB,STAACT,WRKFID                                    
         CLI   8(R1),0                                                          
         BE    WKUPDTX             OK EXIT                                      
         DC    H'0'                                                             
*                                                                               
WKUPDTX  LH    R1,STATCH           BUMP NUMBER OF STATUS CHANGES                
         LA    R1,1(R1)                                                         
         STH   R1,STATCH                                                        
         B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOCK WRKF                                                                     
***********************************************************************         
WFLOCK   NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         L     R8,ASYSFAC                                                       
*                                                                               
         BRAS  RE,FIRFLOCK         ENQ WRKF                                     
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOAD SCREEN (TEST FIRST FOR CURRENT)                                          
***********************************************************************         
LOADSCR  NTR1  BASE=ABASE                                                       
*                                                                               
         LM    R9,RB,ABASES                                                     
         L     R8,ASYSFAC                                                       
         CLC   SCREENR,SCREENC     IS REQ'D SCREEN CURRENT                      
         BE    LOADSCX                                                          
         LA    R1,SRVEX2H                                                       
         ST    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9013500'                                           
         MVC   DMCB+7(1),SCREENR                                                
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SCREENC,SCREENR                                                  
LOADSCX  B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET USERID FROM 2 CHR ID NUMBER                                               
***********************************************************************         
GETUSER  NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         L     R8,ASYSFAC                                                       
*                                                                               
         CLC   GIUSER,GIPREV                                                    
         BE    GETUSRX                                                          
         MVC   GIPREV,GIUSER                                                    
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),GIUSER                                               
         NI    CTIKID+8,X'7F'      UNSET GENERIC FLAG                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         BNE   GETUSR12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
GETUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID ELEMENT                              
         BNE   *+14                                                             
         MVC   GIUSERID,2(R7)      GET ID NAME                                  
         B     GETUSR20                                                         
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         BNE   GETUSR10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETUSR12 EDIT  (B2,GIUSER),(6,GIUSERID),FILL=0,DUB=SDUB,WRK=SWORK1              
*                                                                               
GETUSR20 LA    RF,0                                                             
         LA    RE,GIUSERID                                                      
GETUSR21 CLI   0(RE),X'40'                                                      
         BE    GETUSR30                                                         
         CLI   0(RE),0                                                          
         BE    GETUSR30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CH    RF,=H'8'                                                         
         BL    GETUSR21                                                         
GETUSR30 STC   RF,GIULEN                                                        
GETUSRX  B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* RESET USERID & FILENUM IF Q=N OR U=USER INPUT                                 
***********************************************************************         
SETUSR   NTR1                                                                   
*                                                                               
         CLI   WKQUEUE,0           TEST IF SPECIFIC WRKF NAMED BY Q=N           
         BE    SETUSR2             NO                                           
         CLI   WKQUEUE,C'U'                                                     
         BE    SETUSR2                                                          
         MVC   WRKFID,=CL8'WRKF'   SET SPECIFIC QUEUE NAME                      
         MVC   WRKFCHR,WKQUEUE                                                  
         BAS   RE,SETBUFF          INIT BUFFER                                  
*                                                                               
SETUSR2  OC    USERID,USERID       MUST INPUT U=... IF NOT LOGGED ON            
         BNZ   *+12                                                             
         CLI   WKDDSFN,0           WAS U=... INPUT TO OVERRIDE LOGON            
         BE    ERR7                NO                                           
         CLI   WKDDSFN,0                                                        
         BE    SETUSRX                                                          
         TM    WKUSER,X'80'        IGNORE GENERIC IDS                           
         BNO   *+12                                                             
         OI    DDS,DDSGEN          SET GENERIC FLAG                             
         B     SETUSRX                                                          
         MVC   USERID,WKUSER       SET LOGON ID FROM U=ID                       
         OI    DDS,DDSUSR                                                       
         OC    USERID,USERID                                                    
         BZ    SETUSR4             ZERO USER                                    
*                                                                               
SETUSR3  MVC   NXSRCID,USERID      GET WRKF ID FROM U=... VALUE                 
         L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,WKDMCB,(X'00',GFILE),WRKFL                              
         MVC   WRKFID,NXUSRINF                                                  
         MVC   WKPASS,WRKFCHR                                                   
*                                                                               
SETUSR4  BAS   RE,SETBUFF          INIT BUFFER                                  
*                                                                               
SETUSRX  B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* SCAN INPUT FIELDS FOR ? HELP                                                  
***********************************************************************         
HELPSCAN NTR1                                                                   
*                                                                               
         LA    R4,64(R3)           R4=A(FIRST FIELD)                            
         SR    R2,R2               CLEAR FIELD COUNT                            
SCAN1    SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         BZ    SCANX                                                            
         TM    1(R4),X'20'         TEST PROT                                    
         BNO   SCAN3                                                            
         AR    R4,R0               NEXT FIELD                                   
         B     SCAN1                                                            
SCAN2    LTR   R0,R0                                                            
         BZ    SCAN2A                                                           
         TM    HLPFLG,X'80'                                                     
         BNO   *+6                                                              
         BCTR  R1,0                                                             
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
SCAN2A   SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         AR    R4,R0                                                            
         B     SCAN1                                                            
*                                                                               
SCAN3    EQU   *                   UNPROT FOUND                                 
         LA    R2,1(R2)            INC FIELD COUNT                              
         LA    R1,8(R4)                                                         
         SH    R0,=H'8'                                                         
         SR    R5,R5               POS COUNT ZERO                               
SCAN4    CLI   PFKEY,1                                                          
         BNE   SCAN4A                                                           
         C     R4,CURSOR                                                        
         BE    SCAN5                                                            
SCAN4A   CLI   0(R1),C'?'                                                       
         BE    SCAN5               HELP REQUIRED                                
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SCAN4                                                         
         B     SCAN2               NEXT FIELD                                   
*                                                                               
SCAN5    XC    HELP,HELP           CLEAR HELP                                   
         ST    R4,QHDR             SAVE ADDR                                    
         STC   R2,HLPFLD           SET FIELD NUM                                
         STC   R5,HLPPOS           POSITION                                     
         STC   R5,5(R4)            AND NEW FIELD LENGTH                         
         TM    DDS,DDSTRM                                                       
         BNO   SCAN6                                                            
         CLI   1(R1),C'*'          DDS CAN ENTER ?*                             
         BNE   SCAN6                                                            
         OI    HLPFLG,X'80'        AND GET DDS HELP                             
         LA    R1,1(R1)                                                         
SCAN6    SR    R3,R3               CHECK FOR PAGE NO                            
         TM    1(R1),X'F0'                                                      
         BNO   SCAN2                                                            
         LA    R3,1(R3)                                                         
         TM    2(R1),X'F0'                                                      
         BNO   SCAN7                                                            
         LA    R3,1(R3)                                                         
         TM    3(R1),X'F0'                                                      
         BNO   SCAN7                                                            
         LA    R3,1(R3)                                                         
*                                                                               
SCAN7    BCTR  R3,0                CONVERT PAGE NO                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   R3,DUB                                                           
         STC   R3,HLPPAG                                                        
         B     SCAN2                                                            
*                                                                               
SCANX    XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
*  HELP INDEX 2 BYTES ACTION-INDEX 00=NO ACTION FF=ANY ACTION                   
***********************************************************************         
HLPIND1  DC    X'FF010000'                                                      
*                                                                               
HLPIND2  DC    X'00022103220423052406250726082709280A290B'                      
         DC    X'2D0C310D0000'                                                  
*                                                                               
HLPIND3  DC    X'310E290EFF0F0000'                                              
*                                                                               
HLPIND4  DC    X'00102110221023102410251026102710281029102D103111'              
         DC    X'0000'                                                          
*                                                                               
HLPIND5  DC    X'00122112221223122412251226122712281229122D12'                  
         DC    X'31130000'                                                      
*                                                                               
HLPIND6  DC    X'3114FF150000'                                                  
*                                                                               
HLPIND7  DC    X'FF160000'                                                      
         EJECT                                                                  
*  FLAGS       X'80'                   DDS ONLY PANEL                           
*              X'40'                   EXTRA TEXT BLOCK IN CIREC                
*                                                                               
HELPTAB  DS    0CL3                    INDEX/FLAGS/PANEL                        
         DC    X'01',X'00',X'01'                                                
         DC    X'02',X'00',X'02'                                                
         DC    X'03',X'00',X'03'                                                
         DC    X'04',X'00',X'04'                                                
         DC    X'05',X'00',X'05'                                                
         DC    X'06',X'00',X'06'                                                
         DC    X'07',X'00',X'07'                                                
         DC    X'08',X'00',X'08'                                                
         DC    X'09',X'00',X'09'                                                
         DC    X'0A',X'00',X'0A'                                                
         DC    X'0B',X'00',X'0B'                                                
         DC    X'0C',X'00',X'0C'                                                
         DC    X'0D',X'00',X'0D'                                                
         DC    X'0E',X'00',X'0E'                                                
         DC    X'0F',X'00',X'0F'                                                
         DC    X'10',X'00',X'10'                                                
         DC    X'11',X'00',X'11'                                                
         DC    X'12',X'00',X'12'                                                
         DC    X'13',X'00',X'13'                                                
         DC    X'14',X'00',X'14'                                                
         DC    X'15',X'00',X'15'                                                
         DC    X'16',X'00',X'16'                                                
         DC    X'00'                                                            
*                                                                               
HELPID   DC    XL10'0135FF00000000000000'                                       
         EJECT                                                                  
         DS    0H                                                               
ACTNTBL  DS    0CL10               ACTION NAMES AND VALUES                      
*                                                                               
*              (LA RF) A(ACTION),ACTNUM,LEN,FLAG,A(SUB ACT TAB)                 
*                                                                               
*              FLAGS  X'80'=NEW STATUS,X'40'=DDS                                
*                                                                               
         DC    X'41F0',S(SR@DSP),X'21',X'08',X'00',AL3(1)                       
         DC    X'41F0',S(SR@HOLD),X'22',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(SR@PURGE),X'23',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(SR@ACTVT),X'24',X'08',X'C0',AL3(SACTNT24)              
         DC    X'41F0',S(SR@KEEP),X'25',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(SR@UKEEP),X'26',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(SR@CLEAR),X'27',X'08',X'40',AL3(0)                     
         DC    X'41F0',S(SR@SIZE),X'28',X'08',X'40',AL3(SACTNT28)               
         DC    X'41F0',S(SR@RTAIN),X'29',X'08',X'C0',AL3(1)                     
         DC    X'41F0',S(SR@DELD),X'2B',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(SR@SLECT),X'2D',X'08',X'40',AL3(0)                     
*                                                                               
         DC    X'41F0',S(SR4ACTVT),X'24',X'04',X'C0',AL3(SACTNT24)              
         DC    X'41F0',S(SR4RTAIN),X'29',X'04',X'C0',AL3(1)                     
*                                                                               
         DC    X'41F0',S(SR@BROWS),X'31',X'08',X'40',AL3(1)                     
         DC    X'41F0',S(SR@CHNG),X'32',X'08',X'40',AL3(1)                      
*                                                                               
         DC    F'00'                                                            
*                                                                               
SACTLEN  DS    0CL6                                                             
*                                                                               
SACTNT24 DC    X'41F0',S(SR@RTAIN),H'01'   ACTV SUB ACTIONS                     
         DC    H'00'                                                            
SACTNT28 DC    X'41F0',S(DC@PC),H'01'      ACTV SUB ACTIONS                     
         DC    X'41F0',S(DC@FILE),H'02'                                         
         DC    H'00'                                                            
*                                                                               
SACTNT31 DC    X'41F0',S(SR@SLECT),H'03'   LIST,SEL                             
         DC    H'00'                                                            
*                                                                               
         DS    0H                                                               
SELTABL  DS    0CL8                SEL ACTIONS                                  
*                                                                               
*              (LA RF),A(ACTION),ACTNUM,FLAGS,COMMAND-TYPE,ROUTINE              
*                                                                               
*              FLAGS X'80'    DDS ONLY                                          
*              FLAGS X'40'    NUMERIC SUFFIX                                    
*              FLAGS X'20'    NEGATIVE NUMERIC ALLOWED                          
*              FLAGS X'10'    DON'T EXIT AFTER ACTION                           
*              FLAGS X'02-01' MIN INPUT (MACHINE) LEN 0-2                       
*                                                                               
         DC    X'41F0',S(DC3USE),X'02',X'80',X'00',X'01'                        
         DC    X'41F0',S(DC3TOT),X'03',X'80',X'00',X'02'                        
         DC    X'41F0',S(DC1AST),X'04',X'C0',X'00',X'03'                        
         DC    X'41F0',S(DC1PNT),X'05',X'80',X'00',X'04'                        
         DC    X'41F0',S(DC1DIT),X'06',X'80',X'00',X'04'                        
         DC    X'41F0',S(DC1PLS),X'07',X'80',X'00',X'05'                        
         DC    X'41F0',S(SR4DSP),X'21',X'80',X'00',X'06'                        
         DC    X'41F0',S(SR@SLECT),X'2D',X'80',X'00',X'06'                      
         DC    X'41F0',S(SR4HOLD),X'22',X'90',X'00',X'06'                       
         DC    X'41F0',S(SR4ACTVT),X'24',X'90',X'00',X'06'                      
         DC    X'41F0',S(SR@KEEP),X'25',X'90',X'00',X'06'                       
         DC    X'41F0',S(SR@UKEEP),X'26',X'90',X'00',X'06'                      
         DC    X'41F0',S(SR@PURGE),X'23',X'90',X'00',X'06'                      
         DC    X'41F0',S(SR@RTAIN),X'29',X'D0',X'00',X'06'                      
         DC    X'41F0',S(SR@DELD),X'2B',X'90',X'00',X'06'                       
         DC    X'41F0',S(SR@BROWS),X'31',X'80',X'00',X'06'                      
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
GLIST    DC    CL8'GLIST'                                                       
GFILE    DC    CL8'GFILE'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
RANDOM   DC    CL8'RANDOM'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
*                                                                               
TEMPSTR  DC    CL8'TEMPSTR'                                                     
CTFILE   DC    CL8'CTFILE '                                                     
WRKFL    DC    CL8'WRKFIL'                                                      
*                                                                               
DC@PFKEY DC    CL8'PFKEY'                                                       
DC3DQU   DC    CL8'DQU  '                                                       
DC5DQU   DC    CL8'=DQU '                                                       
DC3USE   DC    CL8'U=   '                                                       
DC3TOT   DC    CL8'T=   '                                                       
DC1AST   DC    CL1'*'                                                           
DC1PNT   DC    CL1'.'                                                           
DC1DIT   DC    CL1'"'                                                           
DC1PLS   DC    CL1'+'                                                           
         DC    CL4'*."+'                                                        
DC@PC    DC    CL8'%'                                                           
DC@FILE  DC    CL8'FILE'                                                        
DDSPSWD  DC    CL8'DDSPQ '                                                      
*                                                                               
SPACES   DC    CL40' '                                                          
FFS      DC    18X'FF'                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
ERR0     LA    RE,SREMBC           MUST BE CONNECTED                            
         B     ERRX                                                             
ERR2     LA    RE,SREACT           INVALID ACTION                               
         B     ERRX                                                             
ERR3     LA    RE,282              INVALID FILE ID                              
         B     ERRX                                                             
ERR4     LA    RE,283              MISSING FILE ID                              
         B     ERRX                                                             
ERR5     LA    RE,180              INVALID FILE CLASS                           
         B     ERRX                                                             
ERR6     LA    RE,SREUID           INVALID USER ID                              
         B     ERRX                                                             
ERR7     LA    RE,181              NOT LOGGED ON AND NO U=USER ID               
         B     ERRX                                                             
ERR8     LA    RE,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR9     LA    RE,SREPSW           INVALID PASSWORD                             
         B     ERRX                                                             
ERR10    LA    RE,285              INVALID PRINT QUEUE ID                       
         B     ERRX                                                             
ERR11    LA    RE,271              ONLY ONE TIME IS VALID                       
         B     ERRX                                                             
                                                                                
***********************************************************************         
* ERROR MESSAGES   RE=MSG NUMBER                                                
***********************************************************************         
ERRX     CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
         L     RD,SAVERD           ERRORS CAN BE CALLED FROM ANYWHERE           
         STH   RE,MSGNUM                                                        
         MVI   MSGTYP,C'E'                                                      
         B     EXIT                DON'T SAVE AFTER ERRORS                      
         B     INTSAVE                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* INFO MESSAGES                                                                 
***********************************************************************         
INF0     LA    RE,1                NO MORE HELP                                 
         B     INFX                                                             
INF1     LA    RE,156              ENTER ACTION                                 
         B     INFX                                                             
                                                                                
***********************************************************************         
* OUTPUT INFO MESSAGES   RE=MSG NUMBER                                          
***********************************************************************         
INFX     L     RD,SAVERD                                                        
         CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
         STH   RE,MSGNUM                                                        
         MVI   MSGTYP,C'I'                                                      
         B     INTSAVE                                                          
                                                                                
***********************************************************************         
* WORKER AND SHARED MEMORY ROUTINES                                             
***********************************************************************         
       ++INCLUDE DMWRKFR           WORKER ROUTINES                              
       ++INCLUDE SHFIR             SHARED MEMORY FILE INDEX ROUTINES            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SRNWKDD           DICTIONARY REFS                              
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SRNWKWK                                                        
         EJECT                                                                  
*                                                                               
SRNWKFFD DS    CL64                                                             
       ++INCLUDE SRNWKFFD                                                       
*                                                                               
       ++INCLUDE DDWKSCAND                                                      
         EJECT                                                                  
*                                                                               
*FADSECTS                                                                       
*DDCOMFACS                                                                      
*SRERREQUS                                                                      
*DDFLDHDR                                                                       
*CTGENFILE                                                                      
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
       ++INCLUDE SRNWKTWAB                                                      
         IHAASCB LIST=YES                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SRNWK00   09/27/19'                                      
         END                                                                    
