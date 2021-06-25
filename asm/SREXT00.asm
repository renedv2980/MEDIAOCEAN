*          DATA SET SREXT00    AT LEVEL 002 AS OF 10/06/08                      
*PHASE T10A00A                                                                  
*INCLUDE BUPPER                                                                 
*&&UK                                                                           
*INCLUDE MEWS                                                                   
*INCLUDE MESHER                                                                 
*INCLUDE GETCOMM                                                                
*INCLUDE GEUP                                                                   
*&&                                                                             
         TITLE '$EXT - EXTRACT UPDATE FACILITY'                                 
EXT      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**$EXT**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
         MVC   ATIA,SRPAR2                                                      
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         L     R4,ATIA                                                          
         USING SRSD,R4             R4=A(SPECIAL S/R SAVE AREA)                  
*&&US*&& NI    SSBJFLAG,X'FF'-SSBJFEXT                                          
*&&US*&& B     EXIT                                                             
*                                                                               
         L     R1,SRPAR4                                                        
         USING COMFACSD,R1                                                      
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
*                                                                               
*&&UK*&& MVC   VADDAY,CADDAY                                                    
*&&UK*&& MVC   VHEXOUT,CHEXOUT                                                  
*                                                                               
         DROP  R1                                                               
         L     R1,=V(BUPPER)                                                    
         A     R1,RELO                                                          
         ST    R1,VBUPPER                                                       
*&&UK                                                                           
         L     R1,=V(MEWS)                                                      
         A     R1,RELO                                                          
         ST    R1,VMEWS                                                         
         L     R1,=V(MESHER)                                                    
         A     R1,RELO                                                          
         ST    R1,VMESHER                                                       
         L     R1,=V(GETCOMM)                                                   
         A     R1,RELO                                                          
         ST    R1,VGETCOMM                                                      
         L     R1,=V(GEUP)                                                      
         A     R1,RELO                                                          
         ST    R1,VGEUP                                                         
*&&                                                                             
         B     EXT2                                                             
         EJECT                                                                  
***********************************************************************         
* PASS UTL TO FIND A TERMINAL AWAITING EXTRACT UPDATE                 *         
*                                                                     *         
* THE UTL IS PASSED IN TWO PARTS ON A ROUND-ROBIN BASIS -             *         
* FROM LAST+1 TO END AND FROM START TO LAST                           *         
* ALL TERMINALS MUST BE PROCESSED IN ORDER TO ESTABLISH THE TOTAL     *         
* NUMBER OF TERMINALS PENDING EXTRACT UPDATES                         *         
***********************************************************************         
         SPACE 1                                                                
EXT2     L     R5,VUTL             SET BXLE REGS FOR UTL PASS                   
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING UTLD,R5             R5=A(UTL)                                    
*                                                                               
         XC    AUTL,AUTL                                                        
         XC    EXTERMS,EXTERMS                                                  
         MVI   UTLPASS,2           START SEARCH FROM TERMINAL THAT HAD          
         OC    SSBELAST,SSBELAST   LAST UPDATE (UNLESS FIRST TIME)              
         BZ    EXT4                                                             
         L     R5,SSBELAST         POINT TO TERM THAT HAD LAST UPDATE           
         MVI   UTLPASS,1                                                        
         B     EXT6                EXCLUDE THIS TERM ON FIRST PASS              
*                                                                               
EXT4     TM    TJOBFLAG,TJOBFEXT   TEST ANY UPDT IN TERMINAL QUEUE              
         BZ    EXT6                                                             
         OC    AUTL,AUTL           TEST TERMINAL ADDRESS ALREADY SET            
         BNZ   *+8                                                              
         ST    R5,AUTL                                                          
         LH    R1,EXTERMS          BUMP SYSTEM NUMBER OF UPDATES                
         AH    R1,=H'1'                                                         
         STH   R1,EXTERMS                                                       
*                                                                               
EXT6     BXLE  R5,R6,EXT4                                                       
*                                                                               
         CLI   UTLPASS,2           TEST ALL TERMINALS PROCESSED                 
         BE    EXT8                                                             
         L     R5,VUTL             NO - SET BXLE REGS FOR FIRST HALF            
         LA    R5,6(R5)                                                         
         L     R7,SSBELAST         INCLUDE LAST UPDT TERM ON THIS PASS          
         AR    R7,R6                                                            
         BCTR  R7,0                                                             
         MVI   UTLPASS,2           SET SECOND PASS IN PROGRESS                  
         B     EXT4                                                             
*                                                                               
EXT8     ICM   R5,15,AUTL          POINT TO UTL ENTRY                           
         BZ    EXTX                                                             
         NI    TJOBFLAG,X'FF'-TJOBFEXT                                          
*                                                                               
         LA    RF,SRPAGENO         SET S/R TWA SAVE PAGE & READ/LOCK IT         
         SLL   RF,32-8                                                          
         ICM   RF,3,TNUM                                                        
         ST    RF,TWAPAGE                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),SSBTWAL                                               
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,TWAPAGE,SRSD                
*                                                                               
         LA    R1,SRJOBQ           SEARCH JOB Q FOR UPDATE PENDING              
         USING SRJOBQ,R1           R1=A(JOB Q)                                  
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=N'JOB Q ENTRIES                           
         BZ    EXT20                                                            
         XC    AJOBQ,AJOBQ                                                      
EXT10    TM    SRJOBSTA,SRJOBUPD   TEST EXTRACT UPDATE PENDING                  
         BZ    EXT12                                                            
         OC    AJOBQ,AJOBQ         TEST FIRST/SECOND ENTRY FOUND                
         BZ    *+12                                                             
         OI    TJOBFLAG,TJOBFEXT   YES - SET UPDATE PENDING                     
         B     EXT14                                                            
         ST    R1,AJOBQ            SAVE A(JOB Q ENTRY)                          
         MVC   REPID,SRJOBREP      SAVE REPORT NUMBER                           
EXT12    LA    R1,SRJOBQLN(R1)     BUMP TO NEXT JOB Q ENTRY                     
         BCT   R0,EXT10                                                         
         OC    AJOBQ,AJOBQ         TEST ANY JOBS FOUND                          
         BZ    EXT20               NO                                           
         DROP  R1                                                               
*                                                                               
EXT14    LA    R3,SRUPDQ-SRSD      SEARCH UPDATE Q FOR ENTRY                    
         LA    R3,SRSD(R3)                                                      
         USING SRUPDQ,R3           R3=A(JOB QUEUE)                              
         LA    R0,SRUPDMAX                                                      
*                                                                               
EXT16    CLC   SRUPDREP,REPID      LOCATE UPDATE Q ENTRY FOR REPORT             
         BE    EXT18                                                            
         LA    R3,SRUPDQLN(R3)     BUMP TO NEXT JOB QUEUE ENTRY                 
         BCT   R0,EXT16                                                         
         DC    H'0'                                                             
EXT18    CLI   SRUPDSTA,SRUPDSCH   MUST BE A SCHEDULED ENTRY                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 UPDATE,SRUPDQ       UPDATE FIRST JOB FOUND                       
         BE    EXT22               UPDATE QUEUE IF EXTRACT UPDATED              
         MVI   KILL,0                                                           
         BL    *+8                                                              
         MVI   KILL,1              SET TO KILL TASK FOR RECOVERY                
*                                                                               
         MVI   SRUPDSTA,SRUPDERR   SET ENTRY IN ERROR & WRITE PAGE BACK         
         L     R1,AJOBQ                                                         
         USING SRJOBQ,R1                                                        
         NI    SRJOBSTA,X'FF'-SRJOBUPD                                          
         OI    SRJOBSTA,SRJOBOUT+SRJOBINV+SRJOBUPE                              
         OI    TJOBFLAG,TJOBFOUT                                                
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRSD                         
         LH    R0,EXTERMS                                                       
         TM    TJOBFLAG,TJOBFEXT                                                
         BNZ   *+8                                                              
         SH    R0,=H'1'                                                         
         LTR   R0,R0                                                            
         BP    *+8                                                              
         NI    SSBJFLAG,X'FF'-SSBJFEXT                                          
         ST    R5,SSBELAST                                                      
         CLI   KILL,0              TEST UPDATIVE ERROR                          
         BE    *+6                                                              
         DC    H'0'                FORCE RECOVERY/RESTORE FOR MY SIN            
         TM    TJOBFLAG,TJOBFEXT   TEST MORE UPDATES PENDING                    
         BNZ   EXT8                                                             
*                                                                               
EXT20    LH    R0,EXTERMS          DECREMENT SYSTEM NUMBER OF JOBS              
         SH    R0,=H'1'                                                         
         STH   R0,EXTERMS                                                       
         BP    EXT2                PASS UTL AGAIN IF MORE TO DO                 
         B     EXTX                                                             
*                                                                               
EXT22    MVI   SRUPDSTA,SRUPDAVA   SET THIS UPDATE Q ENTRY AVAILABLE            
         L     R1,AJOBQ                                                         
         USING SRJOBQ,R1                                                        
         NI    SRJOBSTA,X'FF'-SRJOBUPD                                          
         OI    SRJOBSTA,SRJOBOUT   SET JOB READY (PENDING NOTIFY)               
         OI    TJOBFLAG,TJOBFOUT   SET AWAITING NOTIFY FLAG                     
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRSD                         
*                                                                               
EXT24    ST    R5,SSBELAST         SET A(UTL ENTRY) IN SSB                      
         TM    TJOBFLAG,TJOBFEXT                                                
         BNZ   EXTX                                                             
         LH    R0,EXTERMS          DECREMENT SYSTEM NUMBER OF JOBS              
         SH    R0,=H'1'                                                         
         STH   R0,EXTERMS                                                       
*                                                                               
EXTX     OC    EXTERMS,EXTERMS     RESET SSB FLAG IF ALL UPDATED                
         BNZ   *+8                                                              
         NI    SSBJFLAG,X'FF'-SSBJFEXT                                          
*                                                                               
EXIT     XIT1  ,                                                                
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* DO EXTRACT UPDATE                                                   *         
*                                                                     *         
* NTRY - R1=A(SRUPDQ ENTRY)                                           *         
*                                                                     *         
* EXIT - CC=LOW IF A NON-UPDATIVE ERROR OCCURED                       *         
*        CC=EQUAL IF UPDATE SUCCESSFUL                                *         
*        CC=HIGH IF A SERIOUS ERROR OCCURED                           *         
***********************************************************************         
         SPACE 1                                                                
UPDATE   NTR1  WORK=(R7,UPDWORKX-UPDWORKD)                                      
         LR    R2,R1                                                            
         USING SRUPDQ,R2           R2=A(UPDATE Q ENTRY)                         
         USING UPDWORKD,R7         R7=A(LOCAL W/S)                              
         LA    R0,UPDWORKD                                                      
         LH    R1,=Y(UPDWORKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   UPDFLAG,0                                                        
         L     R1,SRPAR3                                                        
         OC    SRUPDKEY+3(2),SRUPDKEY+3                                         
         BZ    UPDATEL                                                          
         PACK  DUB,SRUPDKEY+3(2)                                                
         CVB   R0,DUB                                                           
         STC   R0,UPDSE                                                         
         MVC   DUB(1),UPDSE                                                     
         MVC   DUB+1(3),=X'FFFFFF'                                              
         XC    DUB+4(4),DUB+4                                                   
*                                                                               
         L     R1,VSELIST          LOCATE SELIST ENTRY                          
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          R1=A(SELIST)                                 
         CLC   SESYS,UPDSE                                                      
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     UPDATEL                                                          
         TM    SEIND,SEINOP        TEST SYSTEM IS STARTED                       
         BNZ   UPDATEL                                                          
         TM    SEIND,SEISTRT                                                    
         BZ    UPDATEL                                                          
         CLI   SESYS,1             TEST SERVICE SYSTEM                          
         BE    UPDATEL                                                          
         MVC   UPDOV,SEOVSYS                                                    
         DROP  R1                                                               
*                                                                               
         GOTO1 VSWITCH,DUB         SWITCH TO SYSTEM FOR UPDATE                  
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BNE   UPDATEL                                                          
*                                                                               
         LA    RE,UPDTAB           SEARCH TABLE FOR SEOVSYS                     
UPDATE2  CLI   0(RE),0             TEST E-O-T                                   
         BE    UPDATEL                                                          
         CLC   UPDOV,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'UPDTAB(RE)                                                  
         B     UPDATE2                                                          
         L     R1,SRPAR3                                                        
         USING UTLD,R1                                                          
         MVC   UPDSVSVC,TSVCREQ                                                 
         XC    TSVCREQ,TSVCREQ     CLEAR S/R FOR DATAMGR CALLS                  
         DROP  R1                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)                                                       
         A     RF,RELO             RF=A(OVERLAY SYSTEM ROUTINE)                 
         BR    RF                                                               
*                                                                               
UPDATEL  MVI   WORK,0              CC=LOW   - NOT SERIOUS ERROR                 
         B     UPDATEX                                                          
UPDATEE  MVI   WORK,1              CC=EQUAL - UPDATE COMPLETED OK               
         B     UPDATEX                                                          
UPDATEH  MVI   WORK,2              CC=HIGH  - SERIOUS ERROR - RECOVER           
         B     UPDATEX                                                          
UPDATEX  L     R1,SRPAR3                                                        
         USING UTLD,R1                                                          
         MVC   TSVCREQ,UPDSVSVC    RESTORE S/R                                  
         MVI   TPRGIND,0           RESET PROGRAM INDICATORS                     
         DROP  R1                                                               
         CLI   WORK,1                                                           
         B     EXIT                                                             
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* MEDIA SYSTEM SYSTEM EXTRACT UPDATE ROUTINE                          *         
***********************************************************************         
         SPACE 1                                                                
UPDMED   GOTO1 VCALLOV,DMCB,0,X'D9000AFA'                                       
         MVC   VREPORT,0(R1)       SAVE V(REPORT)                               
         GOTO1 (RF),(R1),0,X'D9000A56'                                          
         MVC   VTMUNPK,0(R1)                                                    
*                                                                               
         L     R1,SRPAR3                                                        
         USING UTLD,R1                                                          
         OI    TPRGIND,X'04'       SET TO CONVERTED PROGRAM                     
         DROP  R1                                                               
*                                                                               
         MVC   MEWSDMGR,VDATAMGR   INITIALISE MEWSBLK                           
         MVC   MEWSHELO,VHELLO                                                  
         LA    R0,BLOCK                                                         
         ST    R0,MEWSABUF                                                      
         LA    R0,IO                                                            
         ST    R0,MEWSAREC                                                      
         MVC   MEWSPKEY(MEWSPKLN),SRUPDKEY                                      
*                                                                               
         MVI   MEWSACTN,MEWSOPN    OPEN WORKER FILE                             
         GOTO1 VMEWS,MEWSBLK                                                    
         BNE   UPDATEL                                                          
*                                                                               
         LA    R6,IO               READ COORDINATOR USER ID RECORD              
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),MEWSWUSR                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                        
         CLI   8(R1),0                                                          
         BNE   UPDATEL                                                          
*                                                                               
         LA    R6,CTIDATA          LOCATE MEDIA SYSTEM ELEMENT                  
         SR    R0,R0                                                            
         USING CTSYSD,R6                                                        
UPDMED2  CLI   CTSYSEL,0           TEST E-O-R                                   
         BE    UPDATEL                                                          
         CLI   CTSYSEL,X'21'       TEST SYSTEM ELEMENT                          
         BNE   *+12                                                             
         CLI   CTSYSNUM,X'04'      TEST MEDIA SYSTEM                            
         BE    *+14                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     UPDMED2                                                          
         MVC   AGYNUM,CTSYSAGB     SET AGENCY NUMBER                            
         NI    AGYNUM,X'F0'                                                     
*                                                                               
         LA    R6,KEY              BUILD TABLE OF MEDIA LETTERS                 
         USING DMED,R6                                                          
         XC    MEDKEY,MEDKEY                                                    
         MVC   MEDKAM,AGYNUM                                                    
         MVI   MEDKTYP,MEDKPASQ                                                 
         MVC   KEYS(L'MEDKEY),MEDKEY                                            
         GOTO1 VDATAMGR,DMCB,DMRDHI,MEDDIR,MEDKEY,MEDKEY                        
         ORG   *-2                                                              
UPDMED4  BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BNE   UPDATEL                                                          
         CLC   MEDKEY(MEDKCODE-MEDKEY),KEYS                                     
         BNE   UPDMED6                                                          
         ZIC   RE,MEDDNUM                                                       
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         LA    RE,MEDLET(RE)                                                    
         MVC   0(1,RE),MEDDCOD     INSERT MEDIA CODE INTO LIST                  
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         B     UPDMED4                                                          
         DROP  R6                                                               
*                                                                               
UPDMED6  MVC   REPACOM,SRPAR4      INITIALISE REPBLK                            
         LA    R0,REPL1                                                         
         ST    R0,REPABUF          A(PRINT LINE BUFFER)                         
         LA    R0,MEDSPEC                                                       
         ST    R0,REPAPHS          A(SPECS)                                     
         LA    R0,WORKD                                                         
         AH    R0,=Y(BLOCK2-WORKD)                                              
         ST    R0,REPAPQB          A(PRINTQ BUFFER)                             
         MVI   REPWIDTH,REPWREGQ                                                
         MVI   REPHEADN,2                                                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSN,2                                                       
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPPRNTN,1                                                       
         MVI   REPPRNTI,REPPCLRA                                                
         MVC   REPSYSID,=C'ME'                                                  
         MVC   REPPRGID,=C'SH'                                                  
         MVC   REPDATE,SSBDATEB                                                 
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPBLK      INITIALISE REPBLK                            
         MVC   REPUSRID,SRUPDUSR                                                
         MVC   REPREPNO,SRUPDREP   PASS REPORT NUMBER TO RE-OPEN                
         XC    REPREPCI,REPREPCI                                                
         MVI   REPREPCF,0                                                       
         MVI   REPACTN,REPAREO                                                  
         GOTO1 VREPORT,REPBLK      RE-OPEN REPORT                               
         CLI   REPERRS,0                                                        
         BNE   UPDATEL                                                          
*                                                                               
         LA    R0,IO+2             INITIALSE MESHBLK                            
         ST    R0,MESHAREC                                                      
         MVC   MESHDMGR,VDATAMGR                                                
         MVC   MESHHELO,VHELLO                                                  
         MVC   MESHGCOM,VGETCOMM                                                
         MVI   MESHORIG,MEWSOREQ                                                
         GOTO1 VDATCON,DMCB,(3,SSBDATEB),(2,MESHDATE)                           
         MVI   MESHACTN,MESHAPUT                                                
         XC    IOCOUNT,IOCOUNT                                                  
*                                                                               
UPDMED8  L     R1,SSBTKADR         INCREMENT I/O COUNT                          
         SR    R0,R0                                                            
         ICM   R0,7,TCBIOCNT-TCBD(R1)                                           
         XC    TCBIOCNT-TCBD(L'TCBIOCNT,R1),TCBIOCNT-TCBD(R1)                   
         AH    R0,IOCOUNT                                                       
         STH   R0,IOCOUNT                                                       
         CH    R0,=H'32000'                                                     
         BNH   *+12                                                             
         BAS   RE,CLOREP           CLOSE THE REPORT                             
         B     UPDATEH                                                          
*                                                                               
         MVI   MEWSACTN,MEWSGET    GET WORKER FILE RECORD                       
         GOTO1 VMEWS,MEWSBLK                                                    
         TM    MEWSACTN,X'FF'-X'80'                                             
         BZ    *+12                                                             
         BAS   RE,CLOREP           CLOSE THE REPORT                             
         B     UPDATEH                                                          
         TM    MEWSACTN,X'80'                                                   
         BNZ   UPDMED14                                                         
*                                                                               
         MVC   MEDLETL,IO+2        SAVE MEDIA LETTER                            
         LA    RE,MEDLET           CONVERT MEDIA LETTER TO NUMBER               
         SR    RF,RF                                                            
         LA    R0,L'MEDLET                                                      
UPDMED10 CLC   0(1,RE),MEDLETL                                                  
         BE    UPDMED12                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,UPDMED10                                                      
         MVI   MESHERRS,MESHEIMD                                                
         BAS   RE,FORMLIN                                                       
         B     UPDMED8                                                          
*                                                                               
UPDMED12 STC   RF,IO+2             SET AGENCY/MEDIA VALUES                      
         OC    IO+2(1),AGYNUM                                                   
         GOTO1 VMESHER,MESHBLK     PUT RECORD TO MEDIA FILE                     
         BAS   RE,FORMLIN                                                       
         B     UPDMED8                                                          
*                                                                               
UPDMED14 MVI   IO+3,X'FF'          SET LAST TIME CALL                           
         GOTO1 VMESHER,MESHBLK                                                  
*                                                                               
         MVI   MEWSACTN,MEWSUSE    SET WORKER FILE USED                         
         GOTO1 VMEWS,MEWSBLK                                                    
         BAS   RE,CLOREP           CLOSE REPORT                                 
         B     UPDATEE                                                          
*                                                                               
CLOREP   NTR1  ,                                                                
         MVI   REPLP,C' '                                                       
         MVC   REPLP+1(L'REPLP-1),REPLP                                         
         MVC   REPLP+1(19),=C'** END OF REPORT **'                              
         MVI   REPACTN,REPAPUT                                                  
         GOTO1 VREPORT,REPBLK                                                   
         MVI   REPACTN,REPACLO     CLOSE ERROR REPORT                           
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT MEDIA ERROR REPORT                                           *         
***********************************************************************         
         SPACE 1                                                                
FORMLIN  NTR1  ,                                                                
         L     R6,MESHAREC                                                      
         USING DBUY,R6                                                          
         CLI   BUYSER,X'FF'        IGNORE DUMMY BUY RECORDS                     
         BE    FORMLINX                                                         
         CLI   BUYKTYP,BUYKTYPQ                                                 
         BNE   FORMLINX                                                         
         CLI   MESHERRS,MESHOK                                                  
         BNE   FORML1                                                           
         CLI   MESHCODE,MESHEIGN                                                
         BE    FORMLINX                                                         
         CLI   MESHCODE,MESHERKD                                                
         BE    FORMLINX                                                         
         CLI   MESHCODE,MESHECHA                                                
         BE    FORMLINX                                                         
*                                                                               
FORML1   MVC   REPLMED,MEDLETL                                                  
         MVC   REPLCLI(L'BUYKCLI),BUYKCLI                                       
         TM    BUYKCLI+1,X'C0'                                                  
         BNZ   FORML2                                                           
         SR    R0,R0                                                            
         ICM   R0,3,BUYKCLI+1                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REPLCLI+1(4),DUB                                                 
*                                                                               
FORML2   EDIT  (B1,BUYKPRO),(3,REPLPRO),ALIGN=LEFT                              
*                                                                               
         EDIT  (B1,BUYKCAM),(3,REPLCAM),ALIGN=LEFT                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BUYKSUP                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REPLSUP(4),DUB                                                   
         ZIC   R0,BUYKSUP+2                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REPLSUP+4(2),DUB                                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BUYSER+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REPLSER,DUB                                                      
         MVC   REPLSER(1),BUYSER                                                
*                                                                               
         MVC   REPLAGY,BUYKCOA                                                  
*                                                                               
         MVC   REPLDAY,BUYDAY                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUYKDAT),(8,REPLDAT)                             
*                                                                               
         LA    R6,BUYEL                                                         
         SR    R0,R0                                                            
FORML4   CLI   0(R6),0                                                          
         BE    FORML8                                                           
         CLI   0(R6),TVELQ                                                      
         BE    FORMLTV                                                          
         CLI   0(R6),GHIELQ                                                     
         BE    FORMLGHI                                                         
         CLI   0(R6),PRSELQ                                                     
         BE    FORMLPRS                                                         
         CLI   0(R6),REMELQ                                                     
         BE    FORMREM                                                          
FORML6   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FORML4                                                           
*                                                                               
         USING DTV,R6                                                           
FORMLTV  EDIT  (B2,TVSECS),(3,REPLSEC)                                          
         GOTO1 VTMUNPK,DMCB,TVTIME,(9,REPLTIM)                                  
         B     FORML6                                                           
*                                                                               
         USING DGHI,R6                                                          
FORMLGHI EDIT  (B2,GHISECS),(3,REPLSEC)                                         
         GOTO1 VTMUNPK,DMCB,GHITIME,(9,REPLTIM)                                 
         B     FORML6                                                           
*                                                                               
         USING DPRS,R6                                                          
FORMLPRS MVC   REPLPOS,PRSPOS                                                   
         MVC   REPLCOL(L'PRSCOL),PRSCOL                                         
         TM    REPLCOL,X'F0'                                                    
         BNO   FORML6                                                           
         MVC   REPLCOL(1),PRSCOL+1                                              
         MVC   REPLCOL+1(3),=C'CLR'                                             
         B     FORML6                                                           
*                                                                               
         USING DREM,R6                                                          
FORMREM  ZIC   RE,REMLEN                                                        
         LA    R0,REMARKS-DREM+1                                                
         SR    RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   REPLREM(0),REMARKS                                               
         B     FORML6                                                           
*                                                                               
FORML8   L     R1,=A(MSHMSGS)                                                   
         A     R1,RELO                                                          
         CLI   MESHERRS,MESHOK                                                  
         BNE   *+10                                                             
         MVC   MESHERRS,MESHCODE                                                
FORML10  CLI   0(R1),0                                                          
         BE    FORML12                                                          
         CLC   MESHERRS,0(R1)                                                   
         BE    FORML12                                                          
         LA    R1,L'MSHMSGS(R1)                                                 
         B     FORML10                                                          
FORML12  MVC   REPLERR,1(R1)                                                    
         MVI   REPACTN,REPAPUT                                                  
         GOTO1 VREPORT,REPBLK                                                   
*                                                                               
FORMLINX XIT1  ,                                                                
         DROP  R6                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CONTROL SYSTEM SYSTEM EXTRACT UPDATE ROUTINE                        *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
UPDCON   GOTO1 VCALLOV,DMCB,0,X'D9000AFA'                                       
         MVC   VREPORT,0(R1)       SAVE V(REPORT)                               
         MVC   GEUPDMGR,VDATAMGR   INITIALISE GEUPBLK                           
         MVC   GEUPHELO,VHELLO                                                  
         LA    R0,BLOCK                                                         
         ST    R0,GEUPABUF                                                      
         LA    R0,IO                                                            
         ST    R0,GEUPAREC                                                      
         MVC   GEUPPKEY(GEUPPKLN),SRUPDKEY                                      
*                                                                               
         L     R1,SRPAR3                                                        
         USING UTLD,R1                                                          
         OI    TPRGIND,X'04'       SET TO CONVERTED PROGRAM                     
         DROP  R1                                                               
*                                                                               
         MVI   GEUPACTN,GEUPOPN    OPEN WORKER FILE                             
         GOTO1 VGEUP,GEUPBLK                                                    
         BNE   UPDATEL                                                          
*                                                                               
         MVC   REPACOM,SRPAR4      INITIALISE REPBLK                            
         LA    R0,REPL2                                                         
         ST    R0,REPABUF          A(PRINT LINE BUFFER)                         
         LA    R0,CONSPEC                                                       
         ST    R0,REPAPHS          A(SPECS)                                     
         LA    R0,WORKD                                                         
         AH    R0,=Y(BLOCK2-WORKD)                                              
         ST    R0,REPAPQB          A(PRINTQ BUFFER)                             
         MVI   REPWIDTH,REPWREGQ                                                
         MVI   REPHEADN,2                                                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSN,2                                                       
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPPRNTN,1                                                       
         MVI   REPPRNTI,REPPCLRA                                                
         MVC   REPSYSID,=C'CT'                                                  
         MVC   REPPRGID,=C'FT'                                                  
         MVC   REPDATE,SSBDATEB                                                 
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPBLK      INITIALISE REPBLK                            
         MVC   REPUSRID,SRUPDUSR                                                
         MVC   REPREPNO,SRUPDREP   PASS REPORT NUMBER TO RE-OPEN                
         XC    REPREPCI,REPREPCI                                                
         MVI   REPREPCF,0                                                       
         MVI   REPACTN,REPAREO                                                  
         GOTO1 VREPORT,REPBLK      RE-OPEN REPORT                               
         CLI   REPERRS,0                                                        
         BNE   UPDATEL                                                          
         XC    IOCOUNT,IOCOUNT                                                  
         XC    FLAG2,FLAG2         CLEAR ERROR FLAG                             
*                                                                               
CON01    L     R1,SSBTKADR         INCREMENT I/O COUNT                          
         SR    R0,R0                                                            
         ICM   R0,7,TCBIOCNT-TCBD(R1)                                           
         XC    TCBIOCNT-TCBD(L'TCBIOCNT,R1),TCBIOCNT-TCBD(R1)                   
         AH    R0,IOCOUNT                                                       
         STH   R0,IOCOUNT                                                       
         CH    R0,=H'32000'        TEST 32000 NOT EXEEDED                       
         BNH   *+12                                                             
         BAS   RE,CLOSE            CLOSE THE REPORT                             
         B     UPDATEH                                                          
         MVI   GEUPACTN,GEUPGET    GET WORKER FILE RECORD                       
         GOTO1 VGEUP,GEUPBLK                                                    
         TM    GEUPACTN,X'FF'-X'80'                                             
         BZ    *+12                                                             
         BAS   RE,CLOSE            ERROR CLOSE THE REPORT                       
         B     UPDATEH                                                          
         TM    GEUPACTN,X'80'      TEST END OF FILE                             
         BNZ   CONX                                                             
*                                                                               
         BAS   RE,FTREPORT         BUILD FT REPORT LIST                         
         BE    *+12                                                             
         BAS   RE,CLOSE                                                         
         B     UPDATEH                                                          
*                                                                               
         BAS   RE,FTUPDT           UPDATE EXCHANGE RECORDS                      
         BE    CON02                                                            
         MVI   REPACTN,REPAPUT                                                  
         GOTO1 VREPORT,REPBLK      WRITE ERROR LINE                             
         BAS   RE,CLOSE                                                         
         B     UPDATEH                                                          
CON02    MVI   REPACTN,REPAPUT     WRITE PRINT LINE                             
         GOTO1 VREPORT,REPBLK                                                   
         B     CON01                                                            
*                                                                               
CONX     MVI   GEUPACTN,GEUPUSE    SET WORKER FILE USED                         
         GOTO1 VGEUP,GEUPBLK                                                    
         BAS   RE,CLOSE            CLOSE REPORT                                 
         CLI   FLAG2,0                                                          
         BNE   UPDATEL             SAFE ERROR                                   
         B     UPDATEE                                                          
*                                                                               
CLOSE    NTR1  ,                                                                
         MVI   REPL2P,C' '                                                      
         MVC   REPL2P+1(L'REPL2P-1),REPL2P                                      
         MVC   REPL2P+1(30),=C'** END OF REPORT, NO  ERRORS**'                  
         SR    R1,R1                                                            
         ICM   R1,1,FLAG2                                                       
         BZ    CLOSE1                                                           
         LA    RF,REPL2P+19                                                     
         EDIT  (R1),(3,0(RF)),ALIGN=LEFT                                        
CLOSE1   MVI   REPACTN,REPAPUT                                                  
         GOTO1 VREPORT,REPBLK                                                   
         MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD "FINSTAT UPDATE" REPORT LINE                                  *         
***********************************************************************         
         SPACE 1                                                                
FTREPORT NTR1  ,                                                                
         LA    R6,IO                                                            
         LA    R6,2(R6)                                                         
         USING GEXCD,R6                                                         
         CLI   GEKREC,GEKRECQ                                                   
         BNE   FTREPERR                                                         
         MVC   REPLCUR+0(3),GEKCURF                                             
         MVC   REPLCUR+3(4),=C' TO '                                            
         MVC   REPLCUR+7(3),GEKCURT                                             
         LA    R1,TYPTABL                                                       
FTREP01  CLC   GEKCTYP,0(R1)                                                    
         BE    FTREP02                                                          
         LA    R1,6(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   FTREP01                                                          
         B     FTREPERR                                                         
FTREP02  MVC   REPLTYP,1(R1)                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(2,GEKPSTA),(8,REPLPER)                             
         OC    REPLPER,REPLPER                                                  
         BZ    FTREPERR                                                         
         OI    REPLPER,X'F0'                                                    
*                                                                               
FTREP03  LA    R1,GEXRATE                                                       
         BAS   RE,EDITNUM                                                       
         MVC   REPLRAT,WORK                                                     
*                                                                               
         MVC   REPLINV,=C'NO '                                                  
         CLI   GESTAT,GEINVRT                                                   
         BNE   *+10                                                             
         MVC   REPLINV,=C'YES'                                                  
*                                                                               
         XC    HALF,HALF           CONVERT GEXSHFT TO HALFWORD                  
         MVC   HALF+1,GEXSHFT                                                   
         TM    HALF+1,X'80'                                                     
         BNO   *+8                                                              
         MVI   HALF,X'FF'                                                       
         EDIT  (B2,HALF),(5,REPLSHFT),FLOAT=-,ALIGN=LEFT                        
*                                                                               
         CR    RB,RB                                                            
         B     FTREPX                                                           
*                                                                               
FTREPERR LTR   RB,RB                                                            
FTREPX   XIT1  ,                                                                
*                                                                               
TYPTABL  DC    C'C',C'CLOSE'       EXCHANGE RATE TYPES                          
         DC    C'1',C'1 MTH'                                                    
         DC    C'3',C'3 MTH'                                                    
         DC    C'6',C'6 MTH'                                                    
         DC    X'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO OUTPUT NUMBERS IN 99999.99999 FORMAT                    *         
***********************************************************************         
         SPACE 1                                                                
EDITNUM  XC    DUB,DUB                                                          
         MVC   DUB+2(6),0(R1)                                                   
         MVC   WORK(12),=X'4020202021204B2020202020'                            
         ED    WORK(12),DUB+2                                                   
         LA    R1,WORK+11                                                       
EDITN1   LA    RF,WORK                                                          
         CR    R1,RF                                                            
         BNH   EDITN2                                                           
         CLI   0(R1),C'.'                                                       
         BE    EDITN2                                                           
         CLI   0(R1),C'0'                                                       
         BNE   EDITN3                                                           
         MVI   0(R1),C' '                                                       
         BCT   R1,EDITN1                                                        
EDITN2   MVI   1(R1),C'0'                                                       
EDITN3   LA    R0,12                                                            
         CLI   WORK,C' '                                                        
         BNE   *+18                                                             
         MVC   WORK(11),WORK+1                                                  
         MVI   WORK+11,C' '                                                     
         BCT   R0,*-18                                                          
EDNMX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE GENDIR/GENFIL EXCHANGE RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
FTUPDT   NTR1  ,                                                                
         SR    R0,R0               CLEAR ACTION POINTER                         
         XC    FLAG1,FLAG1         AND (ADDED/UPDATED) FLAG                     
         LA    R6,IO                                                            
         LA    R6,2(R6)                                                         
         USING GEXCD,R6                                                         
         MVC   WORK,GEKEY          COPY FINSTAT REC INTO WORK                   
         MVC   GEKPEND,GEKPSTA     MOVE START DATE TO END DATE                  
         XC    GEKPSTA,GEKPSTA     CLEAR START DATE                             
         MVC   KEYS(L'GEKEY),GEKEY SAVE KEY                                     
         GOTO1 VDATAMGR,DMCB,(X'80',DMRDHI),GENDIR,GEKEY,GEKEY                  
         CLI   8(R1),0                                                          
         BNE   FTUPX                                                            
*                                                                               
         CLC   KEYS(GEKPEND-GEKEY),GEKEY                                        
         BNE   FTUPADD             NO PREVIOUS FOUND SO ADD                     
*                                                                               
         LA    R0,FTACT03                                                       
         CLC   GEKPEND,=X'FFFF'    END DATE MUST BE UFN                         
         BNE   FTUP1                                                            
         CLC   GEKPSTA,WORK+GEKPSTA-GEKEY                                       
         BNL   FTUP1               START MUST BE LESS THAN OURS                 
         B     FTUP2                                                            
*                                                                               
FTUP1    CLC   GEKEY,WORK          TEST KEYS ARE IDENTICAL                      
         BE    FTUP1A                                                           
         SR    RE,RE                                                            
         IC    RE,FLAG2                                                         
         LA    RE,1(RE)            BUMP NUMBER OF ERRORS                        
         STC   RE,FLAG2                                                         
         B     FTUPX                                                            
FTUP1A   SR    R0,R0                                                            
         MVC   FULL,GEDDA                                                       
         GOTO1 (RF),(R1),(X'88',GETREC),GENFILE,FULL,GEKEY,IOWORK               
         TM    8(R1),X'FD'                                                      
         BNZ   FTUPX                                                            
         MVC   GEKEY(L'WORK),WORK  REPLACE RECORD                               
         GOTO1 (RF),(R1),(X'00',PUTREC),GENFILE,FULL,GEKEY,IOWORK               
         CLI   8(R1),0                                                          
         BNE   FTUPX                                                            
         LA    R0,FTACT05          SET RECORD REPLACED                          
         B     FTUPX                                                            
*                                                                               
FTUP2    SR    R0,R0                                                            
         MVC   KEYS,GEKEY          MOVE IO INTO KEYSAVE                         
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),GENDIR,GEKEY,KEY                    
         TM    8(R1),X'FD'                                                      
         BNZ   FTUPX                                                            
         MVC   FULL,KEY+GEDDA-GEKEY                                             
         GOTO1 (RF),(R1),(X'88',GETREC),GENFILE,FULL,GEKEY,IOWORK               
         TM    8(R1),X'FD'                                                      
         BNZ   FTUPX                                                            
         OI    GESTAT,X'80'                                                     
         GOTO1 (RF),(R1),(X'00',PUTREC),GENFILE,FULL,GEKEY,IOWORK               
         CLI   8(R1),0                                                          
         BNE   FTUPX                                                            
         OI    KEY+GEDSTAT-GEKEY,X'80'                                          
         GOTO1 (RF),(R1),(X'00',DMWRT),GENDIR,KEYS,KEY                          
         CLI   8(R1),0                                                          
         BNE   FTUPX                                                            
         MVC   GEKEY,KEYS          RESTORE KEY INTO IO                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,WORK+GEKPSTA-GEKEY),(0,DUB)                      
         GOTO1 VADDAY,DMCB,DUB,DUB1,X'FFFFFFFF'                                 
         GOTO1 VDATCON,DMCB,(0,DUB1),(2,GEKPEND)                                
*                                                                               
         NI    GESTAT,X'FF'-X'80'  CLEAR DELETED FLAG                           
         MVC   KEY,GEKEY                                                        
         GOTO1 VDATAMGR,DMCB,ADDREC,GENFILE,KEY,GEKEY,IOWORK                    
         CLI   8(R1),0                                                          
         BNE   FTUPX                                                            
         OI    FLAG1,X'80'         FLAG UPDATE                                  
*                                                                               
FTUPADD  MVC   KEY,WORK                                                         
         GOTO1 VDATAMGR,DMCB,ADDREC,GENFILE,KEY,WORK,IOWORK                     
         CLI   8(R1),0                                                          
         BNE   FTUPX                                                            
         LA    R0,FTACT01          SET RECORD ADDED                             
         TM    FLAG1,X'80'                                                      
         BNO   FTUPX                                                            
         LA    R0,FTACT02          SET RECORD UPDATED                           
*                                                                               
FTUPX    LTR   R0,R0                                                            
         BNZ   FTUPX1                                                           
         MVC   REPLACTN,FTACT04    FATAL DISK ERROR                             
         LA    RF,EXT                                                           
         SR    RE,RF               WHERE WAS IT                                 
         ST    RE,FULL                                                          
         MVC   HALF,DMCB+8         WHAT WAS IT                                  
         GOTO1 VHEXOUT,DMCB,FULL+1,REPLACTN+23,3                                
         GOTO1 (RF),(R1),HALF,REPLACTN+17,1                                     
         LTR   RB,RB                                                            
         B     FTUPXX              EXIT FATAL ERROR                             
*                                                                               
FTUPX1   LR    R1,R0                                                            
         MVC   REPLACTN,0(R1)      PUT OUT MESSAGE                              
         CR    RB,RB               NON FATAL EXIT                               
FTUPXX   XIT1  ,                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* BUDGET SYSTEM SYSTEM EXTRACT UPDATE ROUTINE                         *         
***********************************************************************         
         SPACE 1                                                                
UPDBUD   LA    R3,UPDBBLK          BUILD BUPPER BLOCK                           
         USING BUPBLKD,R3          R3=A(BUPPER BLOCK)                           
         XC    BUPBLKD(BUPBLKL),BUPBLKD                                         
         MVC   BUPADMGR,VDATAMGR                                                
         MVC   BUPAHELO,VHELLO                                                  
         LA    R0,BLOCK                                                         
         ST    R0,BUPABUFF                                                      
         LA    R0,IO                                                            
         ST    R0,BUPAREC                                                       
         GOTO1 VGETFACT,DUB,0                                                   
         L     R1,0(R1)                                                         
         MVC   BUPBDATE,FADATEB-FACTSD(R1)                                      
         MVI   BUPORIG,BUACTEXT                                                 
         MVC   BUIKUSER,SRUPDUSR                                                
         MVC   BUPKEY(BUPKEYL),SRUPDKEY                                         
         LA    R0,BUPBLKD                                                       
         ST    R0,DMCB                                                          
         LA    R1,DMCB                                                          
         L     RF,VBUPPER                                                       
*                                                                               
         MVI   DMCB,BUPOPEN        OPEN WORKER FILE                             
         BASR  RE,RF                                                            
         CLI   0(R1),0             TEST ERRORS                                  
         BNE   UPDATEL                                                          
*                                                                               
UPDBUD2  MVI   DMCB,BUPREAD        READ WORKER FILE RECORD                      
         BASR  RE,RF                                                            
         TM    0(R1),X'FF'-X'80'   TEST ERRORS                                  
         BNZ   UPDATEH                                                          
         TM    0(R1),X'80'         TEST E-O-F                                   
         BNZ   UPDBUD4                                                          
*                                                                               
         MVI   0(R1),BUPPUT        PUT RECORD TO BUDFIL                         
         BASR  RE,RF                                                            
         CLI   0(R1),0             TEST ERRORS                                  
         BNE   UPDATEH                                                          
         B     UPDBUD2                                                          
*                                                                               
UPDBUD4  MVI   DMCB,BUPUSED        PURGE WORKER FILE                            
         BASR  RE,RF                                                            
         CLI   0(R1),0             TEST ERRORS                                  
         BNE   UPDATEL                                                          
         B     UPDATEE                                                          
         DROP  R7                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMWRT    DC    C'DMWRT  '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
CTFILE   DC    C'CTFILE '                                                       
*&&UK                                                                           
MEDDIR   DC    C'MEDDIR '                                                       
GENDIR   DC    C'GENDIR '                                                       
GENFILE  DC    C'GENFILE'                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*&&                                                                             
         SPACE 1                                                                
* TABLE OF SYSTEM OVERLAY NUMBERS AND ROUTINE ADDRESSES                         
*                                                                               
UPDTAB   DS    0XL4                                                             
         DC    X'05',AL3(UPDBUD)   BUDGET SYSTEM EXTRACT UPDATE                 
*&&UK*&& DC    X'04',AL3(UPDMED)   MEDIA SYSTEM EXTRACT UPDATE                  
*&&UK*&& DC    X'0A',AL3(UPDCON)   CONTROL SYSTEM EXTRACT UPDATE                
         DC    AL1(0)                                                           
         EJECT                                                                  
*&&UK                                                                           
MEDSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,49,C'COORDINATION UPDATE ERROR REPORT'                        
         SPEC  H2,49,C'--------------------------------'                        
         SPEC  H1,100,PAGE                                                      
         SPEC  H2,100,REPORT                                                    
         SPEC  M1,1,C' MED CLI   PRO CAM AG PUBSTA  SERIAL  DAY'                
         SPEC  M2,1,C' --- ---   --- --- -- ------  NUMBER  ---'                
         SPEC  M1,45,C'DATE  SEC/ TIME/     SPACE/ '                            
         SPEC  M2,45,C'----  POS  COLOUR    REMARKS'                            
         SPEC  M1,82,C'ERROR DESCRIPTION'                                       
         SPEC  M2,82,C'-----------------'                                       
         SPEC  END                                                              
         SPACE 1                                                                
MSHMSGS  DS    0CL51               ** MESHER MESSAGES **                        
         DC    AL1(MESHEIAG)                                                    
         DC    CL50'COORDINATING AGENCY NOT ON FILE'                            
         DC    AL1(MESHEIMD)                                                    
         DC    CL50'COORDINATING MEDIA NOT ON FILE'                             
         DC    AL1(MESHEICL)                                                    
         DC    CL50'COORDINATING CLIENT NOT ON FILE'                            
         DC    AL1(MESHEIPR)                                                    
         DC    CL50'COORDINATING PRODUCT NOT ON FILE'                           
         DC    AL1(MESHEICA)                                                    
         DC    CL50'COORDINATING CAMPAIGN NOT ON FILE'                          
         DC    AL1(MESHEIMI)                                                    
         DC    CL50'INCOMPATIBLE FROM/TO MEDIA INDICATORS'                      
         DC    AL1(MESHEDOC)                                                    
         DC    CL50'BUY DATE OUTSIDE CAMPAIGN PERIOD'                           
         DC    AL1(MESHEDOB)                                                    
         DC    CL50'BUY DATE NOT COVERED BY BURST'                              
         DC    AL1(MESHEISU)                                                    
         DC    CL50'COORDINATING SUPPLIER NOT ON FILE'                          
         DC    AL1(MESHESNO)                                                    
         DC    CL50'STATION NOT OPEN IN CAMPAIGN'                               
         DC    AL1(MESHENCE)                                                    
         DC    CL50'MISSING COORDINATION ELEMENT ON BUY RECORD'                 
         DC    AL1(MESHETMA)                                                    
         DC    CL50'TOO MANY COORDINATED AGENCIES'                              
         DC    AL1(MESHENOB)                                                    
         DC    CL50'NO BURSTS ON FILE'                                          
         DC    AL1(MESHEBLE)                                                    
         DC    CL50'BUY REPLE NUMBER EXCEEDED'                                  
         DC    AL1(MESHETMO)                                                    
         DC    CL50'TOO MANY CAMPAIGN ORIGINS'                                  
         DC    AL1(MESHECOA)                                                    
         DC    CL50'COORDINATED AGENCY NOT ON FILE'                             
         DC    AL1(MESHEDIE)                                                    
         DC    CL50'DIRECTORY I/O ERROR - CONTACT DDS'                          
         DC    AL1(MESHEFIE)                                                    
         DC    CL50'FILE I/O ERROR - CONTACT DDS'                               
         DC    AL1(MESHEIGN)                                                    
         DC    CL50'RECORD UNCHANGED OR RERUN IN PROGRESS'                      
         DC    AL1(MESHEADD)                                                    
         DC    CL50'BUY RECORD ADDED'                                           
         DC    AL1(MESHECHA)                                                    
         DC    CL50'BUY RECORD CHANGED'                                         
         DC    AL1(MESHEDEL)                                                    
         DC    CL50'BUY RECORD DELETED'                                         
         DC    AL1(MESHERKD)                                                    
         DC    CL50'DUPLICATE BUY RECORD KEY'                                   
         DC    AL1(0)                                                           
         DC    CL50'UNKNOWN ERROR'                                              
*                                                                               
CONSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'FINSTAT UPDATE'                                          
         SPEC  H2,57,C'--------------'                                          
         SPEC  M1,1,C'CURRENCYS   TYPE   DATE  '                                
         SPEC  M2,1,C'----------  ----- -------'                                
         SPEC  M1,26,C'     RATE      INV  SHIFT'                               
         SPEC  M2,26,C'  -----------  ---  -----'                               
         SPEC  M1,51,C'              ACTION            '                        
         SPEC  M2,51,C'  ------------------------------'                        
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         SPACE 1                                                                
FTACT01  DC    CL30'EXCHANGE RECORD ADDED         '                             
FTACT02  DC    CL30'EXCHANGE RECORD UPDATED       '                             
FTACT03  DC    CL30'*ERROR* RATE ALREADY EXISTS   '                             
FTACT04  DC    CL30'FATAL DISK ERROR XX AT XXXXXX '                             
FTACT05  DC    CL30'EXCHANGE RECORD REPLACED      '                             
*&&                                                                             
         EJECT                                                                  
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
FLAG1    DS    C                                                                
FLAG2    DS    C                                                                
DMCB     DS    6F                                                               
WORK     DS    XL64                                                             
RELO     DS    A                                                                
VSWITCH  DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VBUPPER  DS    V                                                                
VDATCON  DS    V                                                                
*&&UK                                                                           
VMEWS    DS    V                                                                
VMESHER  DS    V                                                                
VGETCOMM DS    V                                                                
VGEUP    DS    V                                                                
VREPORT  DS    V                                                                
VTMUNPK  DS    V                                                                
VADDAY   DS    V                                                                
VHEXOUT  DS    V                                                                
*&&                                                                             
ATIA     DS    A                                                                
AUTL     DS    A                                                                
TWAPAGE  DS    A                                                                
AJOBQ    DS    A                                                                
EXTERMS  DS    H                                                                
REPID    DS    XL2                                                              
KILL     DS    X                                                                
UTLPASS  DS    X                                                                
KEY      DS    XL64                                                             
KEYS     DS    XL64                                                             
IOWORK   DS    12D                                                              
IO       DS    2048C                                                            
BLOCK    DS    4096C                                                            
BLOCK2   DS    14336X                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
* DDCOMFACS                                                                     
* FAFACTS                                                                       
* FASELIST                                                                      
* FASYSFAC                                                                      
* FASRS                                                                         
* FASSB                                                                         
* FATCB                                                                         
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASRS                                                          
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATCB                                                          
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* BUPPERD                                                                       
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* BUGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE BUGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*&&UK                                                                           
* GEGENEXC                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENEXC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* MEFILMEDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE MEFILMEDD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* MEFILBUYD                                                                     
         PRINT OFF                                                              
       ++INCLUDE MEFILBUYD                                                      
         PRINT ON                                                               
*&&                                                                             
         EJECT                                                                  
UPDWORKD DSECT                     ** UPDATE S/R LOCAL W/S **                   
UPDSVSVC DS    XL2                                                              
UPDFLAG  DS    X                                                                
UPDSE    DS    X                   SYSTEM NUMBER                                
UPDOV    DS    X                   OVERLAY SYSTEM NUMBER                        
UPDBBLK  DS    XL(BUPBLKL)         BUPPER BLOCK                                 
*&&UK                                                                           
         ORG   UPDBBLK                                                          
IOCOUNT  DS    H                   NUMBER OF PHYSICAL I/O'S                     
AGYNUM   DS    XL1                 AGENCY BINARY                                
MEDLETL  DS    CL1                 LAST MEDIA LETTER                            
MEDLET   DS    CL16                MEDIA LETTERS                                
         SPACE 1                                                                
* MEWSBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE MEWSBLK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* MESHBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE MESHBLK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* GEUPBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE GEUPBLK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FAREPBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAREPBLK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
REPL1    DS    4CL(REPWREGQ)       HEADLINED/MIDLINES                           
REPLP    DS    0CL(REPWREGQ)                                                    
         SPACE 1                                                                
REPLBXL  DS    CL1                 BOX LEFT                                     
         DS    CL1                                                              
REPLMED  DS    CL1                 MEDIA LETTER                                 
         DS    CL2                                                              
REPLCLI  DS    CL5                 CLIENT CODE                                  
         DS    CL1                                                              
REPLPRO  DS    CL3                 PRODUCT NUMBER                               
         DS    CL1                                                              
REPLCAM  DS    CL3                 CAMPAIGN NUMBER                              
         DS    CL1                                                              
REPLAGY  DS    CL2                 AGENCY-ID                                    
         DS    CL1                                                              
REPLSUP  DS    CL6                 SUPPLIER NUMBER                              
         DS    CL1                                                              
REPLSER  DS    CL8                 BUY SERIAL NUMBER                            
         DS    CL1                                                              
REPLDAY  DS    CL3                 BUY DAY EXPRESION                            
         DS    CL1                                                              
REPLDAT  DS    CL7                 BUY DATE                                     
         DS    CL1                                                              
REPLSEC  DS    CL3                 SECONDS LENGTH                               
REPLPOS  EQU   REPLSEC             POSITION CODE                                
         DS    CL2                                                              
REPLTIM  DS    CL9                 TIME EXPRESSION                              
REPLCOL  EQU   REPLTIM             COLOUR CODE                                  
         DS    CL1                                                              
REPLREM  DS    CL15                BUY REMARKS                                  
REPLBXC1 DS    CL1                                                              
REPLERR  DS    CL50                ERROR MESSAGE                                
REPLBXR  DS    CL1                                                              
         ORG   REPLP+L'REPLP                                                    
         EJECT                                                                  
         ORG   REPL1                                                            
REPL2    DS    4CL(REPWREGQ)       HEADLINED/MIDLINES                           
REPL2P   DS    0CL(REPWREGQ)                                                    
         SPACE 1                                                                
REPLCUR  DS    CL10                                                             
         DS    CL2                                                              
REPLTYP  DS    CL5                                                              
         DS    CL1                                                              
REPLPER  DS    CL7                                                              
         DS    CL2                                                              
REPLRAT  DS    CL11                                                             
         DS    CL2                                                              
REPLINV  DS    CL3                                                              
         DS    CL2                                                              
REPLSHFT DS    CL5                                                              
         DS    CL2                                                              
REPLACTN DS    CL30                                                             
         ORG   REPL2P+L'REPL2P                                                  
*&&                                                                             
         SPACE 1                                                                
UPDWORKX EQU   *                                                                
UPDWORKL EQU   *-UPDWORKD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SREXT00   10/06/08'                                      
         END                                                                    
