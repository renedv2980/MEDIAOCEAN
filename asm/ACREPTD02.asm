*          DATA SET ACREPTD02  AT LEVEL 007 AS OF 11/07/14                      
*PHASE ACTD02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE TIMEOUT                                                                
***********************************************************************         
* GENERAL PROGRAM DESCRIPTION                                         *         
* ===========================                                         *         
*                                                                     *         
* REQUESTABLE TIMESHEET AUDIT REPORT                                  *         
* TO SHOW BY PERSON AND TIMESHEET PERIOD END WHAT A TIMELINE WENT FROM*         
* AND/OR WHAT IT WENT TO.                                             *         
* MATERIALS CAN BE INCLUDED AS AN OPTION, BUT, WILL ONLY SHOW WHAT THE*         
* CHANGE IS TO, AND ONLY DISPLAYS IN THE DOWNLOAD                     *         
*                                                                     *         
* PROFILES                                                            *         
* --------                                                            *         
* NO PROFILES FOR THIS REPORT                                         *         
*                                                                     *         
* OPTIONS                                                             *         
* -------                                                             *         
* QSTART   DS    CL6       37        START DATE                       *         
* QEND     DS    CL6       43        END DATE                         *         
* START END DATES - DATES TIMESHEETS FALL BETWEEN (COMPULSORY)        *         
*                                                                     *         
* QACTSTRT DS    CL6       53   (2)  ACTIVITY START DATE (YYMMDD)     *         
* QACTEND  DS    CL6       59   (2)  ACTIVITY END DATE (YYMMDD)       *         
* ACTIVITY DATES - DATES OF ACTIVITY TO TIMESHEETS                    *         
*                                                                     *         
* QOPT1    DS    CL1       60        OPTION#1                         *         
* OPTION 1 =P  REPORT ON ALL ACTIVITY AFTER IN PROGRESS               *         
* OPTION 1 =A  REPORT ON ALL ACTIVITY AFTER FULLY APPROVED ONLY       *         
* OPTION 1 =S  REPORT ON ALL ACTIVITY AFTER SUBMITTED                 *         
* OPTION 1 =P  REPORT ON ALL ACTIVITY AFTER PART APPROVED             *         
* OPTION 1 =R  REPORT ON ALL ACTIVITY AFTER REJECTED                  *         
*                                                                     *         
* QOPT2    DS    CL1       61        OPTION#2                         *         
* OPTION 2 =M  INCLUDE MATERIAL INFORMATION (ONLY VALID WITH D/LOAD)  *         
*                                                                     *         
* QOPT3    DS    CL1       62        OPTION#3                         *         
* OPTION 3 =Y  INCLUDE DISK ADDRESS FOR DEBUGGING                     *         
*                                                                     *         
* QOPT7    DS    CL1       66        OPTION#7                         *         
* OPTION 7 =   DOWNLOADABLE?                                          *         
*                                                                     *         
*  CHANGE DOCUMENTATION                                               *         
*  ====================                                               *         
*  USER DATE     LVL  CHANGE DESCRIPTION                              *         
*  ---- -------- ---  ------------------------------------------------*         
*  TFRY 06JUL09  001  LO01-7843 FIRST EDITION                         *         
*  TFRY 24SEP09  002  LO01-9016 INCLUDE TIME OF ACTIVITY ON DOWNLOAD  *         
*                               INCLUDE MOA ON REPORT                 *         
*  JSHA 05AUG13  007  POP-285   INCREASED TSTABLE MAX TO 7500 FOR BIG *         
*                               REQUESTS AND ADDED DEBUGGING CODE     *         
*       07Nov14  007  SUP-4055  INCREASED STTABLE MAX TO 500 FOR BIG  *         
*                               AUDIT REQUESTS                        *         
***********************************************************************         
         SPACE 1                                                                
         TITLE 'TIME AUDIT REPORT'                                              
ACTD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTD**,R8,R9                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACTAD,RC                                                         
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS THE MODES                                                   *         
***********************************************************************         
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LEDF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
                                                                                
EXIT     XMOD1 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
RUNF     L     RF,VEXTRAS          SET UP BOX ROUTINE                           
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
REQF     L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         GOTO1 ADDICTAT,DMCB,C'L   ',DDIN,DDOUT                                 
         MVC   CMPY,QCOMPANY                                                    
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         GOTO1 DATCON,DMCB,(0,QACTSTRT),(1,ACTSTRT)                             
         GOTO1 DATCON,DMCB,(0,QACTEND),(1,ACTEND)                               
         XR    R1,R1               GET 2'S COMPLEMENT OF START DATE             
         ICM   R1,7,START                                                       
         LNR   R1,R1                                                            
         STCM  R1,7,STARTCC                                                     
         XR    R1,R1                GET 2'S COMPLEMENT OF END DATE              
         ICM   R1,7,END                                                         
         LNR   R1,R1                                                            
         STCM  R1,7,ENDCC                                                       
         MVC   ROFF(L'ROFF),SPACES                                              
         MVC   RDEP(L'RDEP),SPACES                                              
         MVC   RSUB(L'RSUB),SPACES                                              
         MVC   RPERS(L'RPERS),SPACES                                            
         CLI   QOPT7,C'Y'                                                       
         BE    REQF01                                                           
                                                                                
         MVC   DATE3(L'DATE3),ZEROS                                             
         MVC   PERSON3(L'PERSON3),ZEROS                                         
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,1          SET UP PAGE FOR NON DOWNLOAD                 
         USING BOXD,R2                                                          
         L     R2,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'198'                                                 
         B     REQF02                                                           
                                                                                
REQF01   BAS   RE,INTREF           GET TIMELINE REFERENCE FOR DOWNLOAD          
                                                                                
REQF02   L     R4,=A(TSTABLE)                                                   
         ST    R4,TSTABAD          STORE TABLE ADDRESS                          
         L     R5,=A(STTABLE)                                                   
         ST    R5,STTABAD                                                       
                                                                                
         L     R2,ADCMPEL          GET SECURITY ALPHA ID                        
         USING CPYELD,R2                                                        
         MVC   SCMPY,CPYALPHA                                                   
         DROP  R2                                                               
         USING CT5REC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SCMPY                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,IOKEY,ADIO                            
         L     R2,ADIO                                                          
         CLC   IOKEY(L'CT5KEY),0(R2)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,CT5DATA                                                       
         USING CTSEAD,R3                                                        
         XR    R0,R0                                                            
                                                                                
REQF03   CLI   CTSEAEL,0                                                        
         BE    EXIT                                                             
         CLI   CTSEAEL,CTSEAELQ                                                 
         BNE   REQF04                                                           
         MVC   SCMPY,CTSEAAID                                                   
                                                                                
REQF04   IC    R0,CTSEALEN                                                      
         AR    R3,R0                                                            
         B     REQF03                                                           
         DROP  R2,R3                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
         USING ACLELD,R2                                                        
LEDF     L     R2,ADLDGHIR                                                      
         MVC   LALEN,ACLELLVA      SAVE LENGTHS OF OFFICE,DEPT,SUB DEPT         
         MVC   LBLEN,ACLELLVB                                                   
         MVC   LCLEN,ACLELLVC                                                   
         MVC   LDLEN,ACLELLVD                                                   
                                                                                
LEDF01   OC    QACCOUNT,QACCOUNT   IS THERE AN ACCOUNT ENTERED?                 
         BZ    EXIT                                                             
                                                                                
         SR    RF,RF               RF = LENGTH                                  
         SR    RE,RE               RE =DISPLACEMENT                             
         LA    R6,QACCOUNT                                                      
         IC    RF,LDLEN                                                         
         IC    RE,LCLEN                                                         
         SR    RF,RE                                                            
         LA    R6,0(RE,R6)                                                      
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   RPERS(0),0(R6)      SAVE PERSON                                  
         EX    RF,*-6                                                           
                                                                                
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         LA    R6,QACCOUNT                                                      
         IC    RF,LCLEN                                                         
         IC    RE,LBLEN                                                         
         SR    RF,RE                                                            
         LA    R6,0(RE,R6)                                                      
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   RSUB(0),0(R6)       SAVE SUB DEPARTMENT                          
         EX    RF,*-6                                                           
                                                                                
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         LA    R6,QACCOUNT                                                      
         IC    RF,LBLEN                                                         
         IC    RE,LALEN                                                         
         SR    RF,RE                                                            
         LA    R6,0(RE,R6)                                                      
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   RDEP(0),0(R6)       SAVE DEPARTMENT                              
         EX    RF,*-6                                                           
                                                                                
         SR    RF,RF                                                            
         LA    R6,QACCOUNT                                                      
         IC    RF,LALEN                                                         
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   ROFF(0),0(R6)       SAVE OFFICE                                  
         EX    RF,*-6                                                           
                                                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
         USING PERRECD,R2                                                       
REQL     LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   PERKEY(L'PERKEY),SPACES                                          
         MVI   PERKTYP,PERKTYPQ    X'0F' PERSON RECORD                          
         MVC   PERKCPY,QCOMPANY                                                 
         MVC   PERKCODE,RPERS      (BLANKS IF NOT SPECIFIED)                    
         MVC   MYKEY,IOKEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,IOKEY                           
         BNE   EXIT                                                             
                                                                                
REQL00   LA    R2,IOKEY                                                         
         CLC   PERKEY(2),MYKEY                                                  
         BNE   EXIT                                                             
         CLC   RPERS,SPACES                                                     
         BE    *+14                                                             
         CLC   PERKEY(10),MYKEY                                                 
         BNE   EXIT                                                             
         LA    R3,LOC2C          R3=A(2'S COMP OF LOC ST & END DATES)           
         XC    LOC2C,LOC2C                                                      
         MVC   PERSON,PERKCODE                                                  
         MVC   KEYDA,PERKDA                                                     
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,KEYDA,ADIO,DMWORK                      
         XR    R0,R0                                                            
         L     R2,ADIO                                                          
         LA    R6,PERRFST                                                       
                                                                                
         USING EMPELD,R6                                                        
REQL02   CLI   EMPEL,PIDELQ                                                     
         BE    REQL06                                                           
         CLI   EMPEL,LOCELQ                                                     
         BE    REQL08                                                           
         CLI   EMPEL,GPNELQ        X'5A' - GENERAL PURPOSE NAME ELM             
         BE    REQL12                                                           
         CLI   EMPEL,0                                                          
         BE    REQL16                                                           
REQL04   IC    R0,EMPLN                                                         
         AR    R6,R0                                                            
         B     REQL02                                                           
                                                                                
         USING PIDELD,R6                                                        
REQL06   MVC   BINPID,PIDNO                                                     
         OI    BYTE2,X'F0'         SET STATUS BYTE  - FOUND BINARY PID          
         B     REQL04                                                           
                                                                                
         USING LOCELD,R6                                                        
REQL08   CLC   LOCSTART,END        DOES LOCATION FALL WITHIN                    
         BH    REQL04              START AND END DATES                          
         CLC   LOCEND,ZEROS                                                     
         BE    *+14                                                             
         CLC   LOCEND,START                                                     
         BL    REQL04                                                           
         CLC   ROFF,SPACES                                                      
         BE    REQL10                                                           
         CLC   LOCOFF,ROFF                                                      
         BNE   REQL04                                                           
         CLC   RDEP,SPACES                                                      
         BE    REQL10                                                           
         CLC   LOCDEPT,RDEP                                                     
         BNE   REQL04                                                           
         CLC   RSUB,SPACES                                                      
         BE    REQL10                                                           
         CLC   LOCSUB,RSUB                                                      
         BNE   REQL04                                                           
                                                                                
         USING OFFALD,R1                                                        
REQL10   L     R1,ADOFFALD         IS OFFICE IN LIMIT ACCESS?                   
         MVC   OFFAOFFC,LOCOFF                                                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 ADOFFAL                                                          
         BNE   REQL04                                                           
         DROP  R1                                                               
                                                                                
         XR    R1,R1               GET 2'S COMPLEMENT OF START DATE             
         ICM   R1,7,LOCSTART                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,0(R3)                                                       
         LA    R3,3(R3)                                                         
         XR    R1,R1                GET 2'S COMPLEMENT OF END DATE              
         ICM   R1,7,LOCEND                                                      
         LNR   R1,R1                                                            
         STCM  R1,7,0(R3)                                                       
         LA    R3,3(R3)                                                         
         OI    BYTE2,X'0F'         SET STATUS BYTE - FOUND CORRECT              
         B     REQL04              LOCATION                                     
                                                                                
         USING GPNELD,R6                                                        
REQL12   SR    R4,R4                                                            
         IC    R4,GPNLN                                                         
         SHI   R4,GPNLNQ+1                                                      
                                                                                
         CLI   GPNTYP,GPNTLST      ARE WE AT LAST NAME ELEMENT?                 
         BNE   REQL14              NO                                           
         CHI   R4,L'PERLSTNM       DONT GO HIGHER THAN FIELD LENGTH             
         BNH   *+8                                                              
         LA    R4,L'PERLSTNM                                                    
         EXMVC R4,PERLSTNM,GPNNME                                               
         B     REQL04                                                           
                                                                                
REQL14   CLI   GPNTYP,GPNTFST      ARE WE AT FIRST NAME ELEMENT?                
         BNE   REQL04              NO                                           
         CHI   R4,L'PERFSTNM       DONT GO HIGHER THAN FIELD LENGTH             
         BNH   *+8                                                              
         LA    R4,L'PERFSTNM                                                    
         EXMVC R4,PERFSTNM,GPNNME  SAVE FIRST NAME                              
         B     REQL04                                                           
                                                                                
REQL16   TM    BYTE2,X'FF'         HAVE WE FOUND A PERSON IN CORRECT            
         BNO   REQL18              LOCATION WITHIN DATES?                       
         MVC   SAVEKEY,PERKEY      SAVE KEY                                     
         BAS   RE,PROC             AND PROCESS                                  
                                                                                
         MVC   IOKEY,SAVEKEY                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,IOKEY,IOKEY                           
                                                                                
REQL18   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,IOKEY                           
         MVC   PERSON,SPACES                                                    
         MVC   PERFSTNM,SPACES                                                  
         MVC   PERLSTNM,SPACES                                                  
         MVC   BINPID,SPACES                                                    
         MVI   BYTE2,X'00'                                                      
         B     REQL00                                                           
                                                                                
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TAURECD & FILL TABLE                                        *         
***********************************************************************         
PROC     NTR1  ,                                                                
         XC    MYKEY,MYKEY                                                      
         XC    IOKEY,IOKEY                                                      
         XC    PEND,PEND                                                        
         XC    TSTABCNT,TSTABCNT   CLEAR TABLE COUNTS                           
         XC    STTABCNT,STTABCNT                                                
         USING TSTABD,R4                                                        
         L     R4,TSTABAD          TIMESHEET TABLE                              
         USING STTABD,R5                                                        
         L     R5,STTABAD          STATUS CHANGES TABLE                         
         USING AUDRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   AUDKEY,SPACES2                                                   
         MVI   AUDKTYP,AUDKTYPQ    X'37' TIME AUDIT                             
         MVI   AUDKSUB,AUDKSUBQ    X'25'                                        
         MVC   AUDKCPY,CMPY        COMPANY                                      
         MVI   AUDKAUDT,AUDKTIME                                                
         MVC   AUDKPIDB,BINPID     BINARY PID                                   
         MVC   MYKEY,IOKEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,IOKEY                           
         BE    PROC02                                                           
         DC    H'0'                                                             
                                                                                
PROC01   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,IOKEY                           
                                                                                
PROC02   LA    R6,IOKEY                                                         
         CLC   AUDKEY(AUDKPEDT-AUDRECD),MYKEY  CHECK KEY UP TO PID              
         BNE   PROC20              END OF TAURECS FOR THIS PERSON               
         CLC   AUDKPEDT,ENDCC      DOES IT FALL WITHIN DATE RANGE?              
         BL    PROC01              (2'S COMPLEMENT)                             
         CLC   AUDKPEDT,STARTCC                                                 
         BH    PROC20                                                           
         LA    R3,LOC2C                                                         
*                                                                               
PROC03   CLC   AUDKPEDT,3(R3)      DOES IT FALL WITHIN THE LOCATION             
         BL    PROC04              START AND END DATES                          
         CLC   AUDKPEDT,0(R3)                                                   
         BH    PROC04                                                           
         B     PROC05                                                           
*                                                                               
PROC04   LA    R3,6(R3)                                                         
         CLC   0(3,R3),ZEROS                                                    
         BNE   PROC03                                                           
         CLC   3(3,R3),ZEROS                                                    
         BNE   PROC03                                                           
         B     PROC01                                                           
*                                                                               
PROC05   MVC   KEYDA,AUDKDA                                                     
         MVC   PEDT,AUDKPEDT       SAVE 2'S C. PERIOD END DATE                  
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,KEYDA,ADIO,DMWORK                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADIO                                                          
         AHI   R6,AUDRFST-AUDRECD                                               
                                                                                
         USING STCELD,R6           GET STCEL                                    
PROC06   CLI   STCEL,0                                                          
         BE    PROC01                                                           
         CLI   STCEL,STCELQ                                                     
         BNE   *+12                                                             
         CLI   STCIND,STCITIME     IS IT BRANDOCEAN TIME?                       
         BE    PROC10                                                           
                                                                                
PROC08   XR    R0,R0                                                            
         IC    R0,STCLN            GET NEXT ELEMENT                             
         AR    R6,R0                                                            
         B     PROC06                                                           
                                                                                
PROC10   CLI   STCDTFR,0           IS IT A STATUS CHANGE?                       
         BNE   PROC18                                                           
         CLI   STCDTTO,0                                                        
         BNE   PROC18                                                           
         CLI   STCTTYP,STCTTSAD    TEST ADD                                     
         BE    PROC18                                                           
         CLI   STCTTYP,STCTTSDL    TEST DELETE                                  
         BE    PROC18                                                           
*                                  BUILD TIMESHEET TABLE ENTRY                  
*&&US                                                                           
         LA    RE,TSDA             FIND NEXT OPEN SLOT                          
         LA    R0,5                LEAVE 1 FOR FINAL MOVE                       
PROC05A  OC    0(L'KEYDA,RE),0(RE) OPEN SLOT?                                   
         BZ    PROC05B                                                          
         LA    RE,L'KEYDA(RE)                                                   
         BCT   R0,PROC05A                                                       
PROC05B  MVC   0(L'KEYDA,RE),KEYDA SAVE FOR DEBUGGING                           
*&&                                                                             
         MVC   TSDATE(3),STCTDTE   SAVE DATE OF CHANGE                          
         MVC   TSPERS,PERSON       SAVE TIMESHEET FOR PERSON                    
         MVC   TSPERFNM,PERFSTNM   SAVE PERSON FIRST NAME                       
         MVC   TSPERLNM,PERLSTNM   SAVE PERSON LAST  NAME                       
         SR    R0,R0                                                            
         ICM   R0,7,PEDT                                                        
         LNR   R0,R0                                                            
         STCM  R0,7,TSPEND         SAVE TRUE PERIOD END DATE                    
         MVC   TSPID(2),STCTPID    PERSON WHO MADE CHANGE                       
         MVC   TSTIME,STCTTIM      TIME OF CHANGE                               
         MVC   TSUSER,STCTUSR      SAVE USER ID                                 
         MVC   TSMOA,STCTMOA       SAVE MOA                                     
         MVC   TSROW,STCTROW       ROW NUMBER                                   
         MVC   TSMROW,ZEROS                                                     
         MVC   TSCHG,STCTTYP       WHAT HAS CHANGED                             
         MVC   TSCHG2,STCTTYP                                                   
         TR    TSCHG2,TRANS        TRANSLATE STCTTYP                            
         MVC   TSROWCHG(L'TSROWCHG),ZEROS                                       
         MVC   TSROWCH2(L'TSROWCH2),ZEROS                                       
         CLI   STCLN,STCLN3Q       TEST SHORT ELEMENT                           
         BE    PROC16              YES - FINISHED                               
         CLI   STCTTYP,STCTRWDL    IS IT A LINE DELETE                          
         BE    PROC16              YES - FINISHED                               
         TM    STCTTYP,STCTMRAM    TEST MATERIAL AMEND/ADD/DEL(X'08')           
         BO    PROC14                                                           
         MVC   TSACC,STCTCULA      ACCOUNT SJ/1N                                
         MVC   TSHRS,STCTCHRS      HOURS                                        
         MVC   TSTYPE,STCTCTTY     TIME TYPE                                    
         MVC   TSWC,STCTCTSK       WORKCODE                                     
         MVC   TSORD,STCTCORD      ORDER                                        
         MVC   TSREF,STCTCINT      INTERNAL REFERENCE                           
         MVC   TSROWCHG,STCTSTAT   TYPE OF ROW CHANGE                           
         MVC   TSROWCH2,STCTSTA3   TYPE OF ROW CHANGE                           
         XR    R2,R2                                                            
         IC    R2,STCLN                                                         
         AHI   R2,-(STCLNTQ+1)                                                  
         BM    PROC16              NO NARRATIVE                                 
         CHI   R2,L'TSNARR-1       CHECK NOT > OUR MAX                          
         JNH   *+8                                                              
         LHI   R2,L'TSNARR-1                                                    
         MVC   TSNARR(0),STCTCNAR  NARRATIVE                                    
         EX    R2,*-6                                                           
         B     PROC16                                                           
                                                                                
PROC14   MVC   TSMROW,STCTMROW     FOR MATERIALS                                
         CLI   STCTTYP,STCTMRDL    MATERIAL DELETE?                             
         BE    PROC16                                                           
         CLI   STCLN,STCLN4Q       OLDER SHORT ELEMENT?                         
         BE    PROC16                                                           
         MVC   TSMSTAT,STCTSTA2                                                 
         MVC   TSMTOT,STCMCTOT                                                  
         MVC   TSMPRI,STCMCPRC                                                  
         MVC   TSMMULT,STCMCMUL                                                 
         MVC   TSMCOD,STCMCCOD                                                  
         XR    R2,R2                                                            
         IC    R2,STCLN                                                         
         AHI   R2,-(STCLNMQ+1)                                                  
         BM    PROC16               NO NARRATIVE                                
         CHI   R2,L'TSMNARR-1       CHECK NOT > OUR MAX                         
         JNH   *+8                                                              
         LHI   R2,L'TSMNARR-1                                                   
         MVC   TSMNARR(0),STCMCTXT   NARRATIVE                                  
         EX    R2,*-6                                                           
         B     PROC16                                                           
                                                                                
PROC16   LA    R4,TSTABL(R4)       BUMP TABLE                                   
         CLC   TSPERS(3),EFFFS      TEST EOT                                    
         BNE   *+6                                                              
         DC    H'0'                DIE IF TABLE IS FULL                         
         L     R0,TSTABCNT         BUMP TABLE COUNT                             
         AHI   R0,1                                                             
         ST    R0,TSTABCNT                                                      
         B     PROC08                                                           
*                                  BUILD STATUS TABLE ENTRY                     
PROC18   MVC   STPERS,PERSON                                                    
         MVC   STPERFNM,PERFSTNM                                                
         MVC   STPERLNM,PERLSTNM                                                
         SR    R0,R0               UPDATE STATUS CHANGE TABLE                   
         ICM   R0,7,PEDT                                                        
         LNR   R0,R0                                                            
         STCM  R0,7,STPEND         SAVE PERIOD END DATE                         
         MVC   STDATE(3),STCTDTE   DATE OF CHANGE                               
         MVC   STTIME,STCTTIM       TIME OF CHANGE                              
         MVC   STPID(2),STCTPID     PERSON WHO MADE CHANGE                      
         MVC   STUSER,STCTUSR       SAVE USER ID                                
         MVC   STSTAT,STCDTTO       STATUS TO                                   
         LA    R5,STTABL(R5)        BUMP TABLE                                  
         CLC   STPERS(3),EFFFS                                                  
         BNE   *+6                                                              
         DC    H'0'                 END OF TABLE                                
         L     R0,STTABCNT          BUMP TABLE COUNT                            
         AHI   R0,1                                                             
         ST    R0,STTABCNT                                                      
         B     PROC08                                                           
                                                                                
PROC20   MVC   STPERS(3),EFFFS      MARK END OF TABLE                           
         MVC   TSPERS(3),EFFFS      MARK END OF TABLE                           
         B     SORT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SORT TABLE AND CALL PRINT/DOWNLOAD AFTER EACH PERSON                *         
***********************************************************************         
SORT     L     R4,TSTABAD          (TIMESHEET TABLE)                            
         L     R0,TSTABCNT                                                      
         GOTO1 XSORT,DMCB,(0,TSTABAD),(R0),TSTABL,TSTABL,0                      
         L     R5,STTABAD          (STATUS TABLE)                               
         L     R0,STTABCNT                                                      
         GOTO1 XSORT,DMCB,(0,STTABAD),(R0),STTABL,STTABL,0                      
*                                                                               
         L     R4,TSTABAD                                                       
SORT02   CLC   TSPERS(3),ZEROS      POPULATE NARRATIVE FOR DOWNLOAD             
         BE    SORT04               AS CURRENT NARRATIVE IS NOT                 
         CLC   TSPERS(3),EFFFS      HELD ON STCELD                              
         BE    SORT04                                                           
         MVC   COMPROW,TSROW                                                    
         MVC   PEND,TSPEND                                                      
         MVC   NARR,TSNARR                                                      
         MVC   NARR2,TSMNARR                                                    
         MVC   COMPMAT,TSMROW                                                   
         LA    R4,TSTABL(R4)                                                    
         CLC   PEND,TSPEND                                                      
         BNE   SORT02                                                           
         CLC   TSROW,COMPROW                                                    
         BNE   SORT02                                                           
         TM    TSROWCHG,X'08'      (STCTTEXT)                                   
         BO    SORT02                                                           
         TM    TSCHG,X'08'         MAT CHG/DEL/ADD?                             
         BE    *+20                                                             
         MVC   TSNARR,NARR                                                      
         MVC   NARR,SPACES                                                      
         B     SORT02                                                           
         CLC   COMPMAT,TSMROW                                                   
         BNE   SORT02                                                           
         MVC   TSMNARR,NARR2                                                    
         MVC   NARR2,SPACES                                                     
                                                                                
SORT04   L     R4,TSTABAD                                                       
         MVC   PEND,SPACES                                                      
*                                                                               
                                                                                
SORT10   CLC   STPERS(3),ZEROS     BUMP STATUS TABLE ALONG TO GET FIRST         
         BNE   *+12                ENTRY                                        
SORT14   LA    R5,STTABL(R5)                                                    
         B     SORT10                                                           
         CLC   STPERS(3),EFFFS     END OF TABLE?                                
         BE    SORT42                                                           
                                                                                
         MVC   COMPARE(L'COMPROW+L'COMPMAT),ZEROS                               
         CLC   PEND,STPEND         DO WE HAVE A DIFFERENT PERIOD END?           
         BE    SORT16                                                           
         MVI   BYTE1,X'00'                                                      
         MVC   PEND,STPEND                                                      
         L     R4,TSTABAD                                                       
                                                                                
SORT16   CLI   BYTE1,X'00'         HAS THE CORRECT STATUS ALREADY BEEN          
         BNE   SORT20              FOUND?                                       
         MVC   STATUS,SPACES                                                    
         CLI   QOPT1,C'I'          READ STATUS TABLE TO FIND A MATCH ON         
         BE    SORT18              OPTION 1                                     
                                                                                
         CLI   QOPT1,C'A'          APPROVED?                                    
         BNE   *+12                                                             
         TM    STSTAT,X'40'                                                     
         BNO   SORT14                                                           
                                                                                
         CLI   QOPT1,C'S'          SUBMITTED?                                   
         BNE   SORT17                                                           
         CLI   STSTAT,X'00'        ANY CHANGE IN STATUS SHOULD REPORTED         
         BE    SORT14              ON WHEN OPT1=S                               
                                                                                
SORT17   CLI   QOPT1,C'R'          REJECTED                                     
         BNE   *+12                                                             
         TM    STSTAT,X'02'                                                     
         BNO   SORT14                                                           
                                                                                
         CLI   QOPT1,C'P'          PART APPROVED?                               
         BNE   *+12                                                             
         TM    STSTAT,X'10'                                                     
         BNO   SORT14                                                           
                                                                                
         MVI   BYTE1,X'01'         SET BYTE                                     
         B     SORT46                                                           
                                                                                
SORT18   MVC   COMPACT1(L'COMPACT1),ZEROS    SET UP ACTIVITY DATES FOR          
         MVC   COMPACT2(3),STDATE            COMPARE IF FIRST STATUS            
         MVC   COMPACT2+3(4),STTIME          CHANGE AND OPT1 = IN PROG          
         MVI   BYTE1,X'02'                                                      
         B     SORT22                                                           
                                                                                
SORT20   MVC   COMPACT2,EFFFS      SET UP ACTIVITY DATES FOR COMPARE            
         MVC   COMPACT1(3),STDATE                                               
         MVC   COMPACT1+3(4),STTIME CHECK NEXT STATUS TABLE ENTRY IS            
         CLC   0(L'STPERS+L'STPEND,R5),STTABL(R5)                               
         BNE   SORT22              FOR THE SAME PERSON AND TIMESHEET            
         ST    R5,STTABAD2                                                      
         LA    R5,STTABL(R5)                                                    
         MVC   COMPACT2(3),STDATE                                               
         MVC   COMPACT2+3(4),STTIME                                             
         L     R5,STTABAD2                                                      
                                                                                
SORT22   BAS   RE,CLEAR            CLEAR OUTPUT FIELDS                          
         CLC   TSPERS,STPERS       ARE STATUS TABLE AND TIMESHEET TABLE         
         BNE   SORT38              LOOKING AT THE SAME PERSON?                  
         CLC   TSPEND,STPEND       AND PERIOD END DATE?                         
         BNE   SORT38                                                           
         CLC   TSROWCOM(L'TSROW+L'TSMROW),COMPARE NEW ROW NUMBER?               
         BE    SORT23                                                           
         B     SORT24                                                           
                                                                                
SORT23   ST    R4,TSTABAD2                                                      
         LA    R4,TSTABL(R4)                                                    
         CLC   TSDATE(7),COMPACT1  DOES TIMESHEET ACTIVITY FALL WITHIN          
         BNH   SORT30              STATUS TABLE ACTIVITY DATES?                 
         CLC   TSDATE(7),COMPACT2                                               
         BH    SORT30                                                           
         CLC   TSPERS,STPERS       CHECK TS+1 IS FOR THE SAME PERSON            
         BNE   SORT32              AND PERIOD END                               
         CLC   TSPEND,STPEND                                                    
         BNE   SORT32                                                           
                                                                                
         OC    ACTSTRT,ACTSTRT     WERE ANY ACTIVITY DATES SPECIFIED?           
         BZ    *+14                IF SO CHECK TIMESHEET ACTIVITY FALLS         
         CLC   TSDATE(3),ACTSTRT   IN THESE DATES                               
         BL    SORT40                                                           
         OC    ACTEND,ACTEND                                                    
         BZ    *+14                                                             
         CLC   TSDATE(3),ACTEND                                                 
         BH    SORT40                                                           
                                                                                
         L     R4,TSTABAD2                                                      
         CLC   TSROWCOM(L'TSROW+L'TSMROW),TSTABL+TSROWCOM-TSFST(R4)             
         BE    SORT50              OUTPUT DATA AS "FROM"                        
         LA    R4,TSTABL(R4)                                                    
         B     SORT72              OUTPUT DATA AS "TO"                          
                                                                                
SORT24   CLC   TSDATE(7),COMPACT1  DOES TIMESHEET ACTIVITY FALL WITHIN          
         BNH   SORT36              STATUS TABLE ACTIVITY DATES?                 
         CLC   TSDATE(7),COMPACT2                                               
         BH    SORT36                                                           
                                                                                
         OC    ACTSTRT,ACTSTRT     WERE ACTIVITY START/END DATES                
         BZ    *+14                SPECIFIED IN  REQUEST?                       
         CLC   TSDATE(3),ACTSTRT   IF SO CHECK TIMESHEET ACTIVITY FALLS         
         BL    SORT34              IN THESE DATES                               
         OC    ACTEND,ACTEND                                                    
         BZ    SORT26                                                           
         CLC   TSDATE(3),ACTEND                                                 
         BH    SORT34                                                           
                                                                                
SORT26   B     SORT72              OUTPUT LINE AS "TO" CHANGES                  
                                                                                
SORT28   MVC   COMPARE(L'COMPROW+L'COMPMAT),TSROWCOM                            
         B     SORT22                                                           
                                                                                
SORT30   MVC   COMPARE(L'COMPROW+L'COMPMAT),TSROWCOM                            
         B     SORT40              SAVE ROW FOR NEXT COMPARISON                 
                                                                                
SORT32   MVC   COMPARE(L'COMPROW+L'COMPMAT),ZEROS                               
         B     SORT40                                                           
                                                                                
SORT34   CLI   BYTE1,X'01'                                                      
         BNE   SORT36                                                           
         ST    R4,TSTABAD2                                                      
         LA    R4,TSTABL(R4)                                                    
         MVC   COMPARE(L'COMPROW+L'COMPMAT),TSROWCOM                            
         B     SORT40                                                           
                                                                                
SORT36   MVC   COMPARE(L'COMPROW+L'COMPMAT),TSROWCOM                            
                                                                                
SORT38   LA    R4,TSTABL(R4)       BUMP TABLE                                   
                                                                                
SORT40   CLC   0(3,R4),ZEROS       GET NEXT TIMESHEET TABLE ENTRY               
         BNE   *+12                                                             
         LA    R4,TSTABL(R4)                                                    
         B     SORT40                                                           
         CLC   0(3,R4),EFFFS       END OF TABLE?                                
         BNE   SORT22                                                           
         CLI   BYTE1,X'02'         FIRST TIME THROUGH AND QOPT1=I               
         BE    SORT46                                                           
         B     SORT44                                                           
                                                                                
SORT42   L     R0,TSTABAD         CLEAR TIMESHEET TABLE READY FOR NEXT          
         L     R1,=A(TSTABNUM*TSTABL)                                           
         LA    R2,0                                                             
         LA    R3,X'00'                                                         
         SLL   R3,24                                                            
         MVCL  R0,R2                                                            
                                                                                
         L     R0,STTABAD          CLEAR STATUS TABLE READY FOR NEXT            
         L     R1,=A(STTABNUM*STTABL)                                           
         LA    R2,0                                                             
         LA    R3,X'00'                                                         
         SLL   R3,24                                                            
         MVCL  R0,R2                                                            
                                                                                
         MVC   MYKEY,SAVEKEY                                                    
         B     XIT                                                              
                                                                                
***********************************************************************         
* OUTPUT STATUS CHANGE LINE                                           *         
***********************************************************************         
SORT44   CLC   0(11,R5),STTABL(R5) OUTPUT STATUS CHANGE IF THERE IS ONE         
         BNE   SORT14                                                           
         LA    R5,STTABL(R5)                                                    
                                                                                
SORT46   OC    ACTSTRT,ACTSTRT     WERE THERE ACTIVITY DATES SPECIFIED?         
         BZ    *+14                IF SO DO STATUS CHANGES FALL IN              
         CLC   STDATE(3),ACTSTRT   BETWEEN THESE DATES?                         
         BL    SORT48                                                           
         OC    ACTEND,ACTEND                                                    
         BZ    *+14                                                             
         CLC   STDATE(3),ACTEND                                                 
         BH    SORT48              OUTPUT STATUS LINE INFORMATION               
         BAS   RE,CLEAR                                                         
         GOTO1 DATCON,DMCB,(1,STPEND),(17,DATE2)                                
         GOTO1 DATCON,DMCB,(1,STDATE),(17,DATE)                                 
         GOTO1 TIMEOUT,DMCB,(3,STTIME),(3,DUB)                                  
         MVC   TIMCHG(L'TIMCHG),DUB+7                                           
         MVC   USER,STUSER                                                      
         BAS   RE,GETUSR                                                        
         MVC   BINPID,STPID                                                     
         BAS   RE,GETPID                                                        
         MVC   PERSON2,PERCOD                                                   
         MVC   PERSON,STPERS                                                    
         MVC   PERFSTNM,STPERFNM                                                
         MVC   PERLSTNM,STPERLNM                                                
         MVC   SWHAT,AC@TSCHG                                                   
         TM    STSTAT,X'80'        DELETED?                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@DELD                                                   
         TM    STSTAT,X'40'        FULLY APPROVED                               
         BZ    *+10                                                             
         MVC   STATUS,AC@FUAPR                                                  
         TM    STSTAT,X'20'        SUBMITTED?                                   
         BZ    *+10                                                             
         MVC   STATUS,AC@ESSUB                                                  
         TM    STSTAT,X'10'        PART APPROVED?                               
         BZ    *+10                                                             
         MVC   STATUS,AC@ESPAP                                                  
         TM    STSTAT,X'08'        AWAITING LINE MANAGER APPROVAL?              
         BZ    *+10                                                             
         MVC   STATUS,AC@AWLMA                                                  
         TM    STSTAT,X'02'        REJECTED                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@ESREJ                                                  
         TM    STSTAT,X'01'        LINE MANAGER APPROVED                        
         BZ    *+10                                                             
         MVC   STATUS,AC@LIMA                                                   
         CLI   STSTAT,X'00'        IN PROGRESS                                  
         BNE   *+10                                                             
         MVC   STATUS,AC@INPRO                                                  
         CLI   QOPT7,C'Y'          DOWNLOADING?                                 
         BE    *+12                                                             
         BAS   RE,MYREPORT                                                      
         B     *+8                                                              
         BAS   RE,MYDOWN                                                        
SORT48   MVI   BYTE1,X'01'                                                      
         L     R4,TSTABAD                                                       
         MVC   COMPARE(L'COMPROW+L'COMPMAT),ZEROS                               
         MVC   SWHAT,SPACES                                                     
         B     SORT20                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT "FROM" INFORMATION                                           *         
***********************************************************************         
SORT50   CLI   TSTYPE,1            B TIME?                                      
         BNE   *+12                                                             
         MVI   TIMTYP2,C'B'                                                     
         B     SORT52                                                           
         CLI   TSTYPE,2            R TIME?                                      
         BNE   *+12                                                             
         MVI   TIMTYP2,C'R'                                                     
         B     SORT52                                                           
         CLI   TSTYPE,3            N TIME?                                      
         BNE   *+12                                                             
         MVI   TIMTYP2,C'N'                                                     
         B     SORT52                                                           
         CLI   TSTYPE,X'10'                                                     
         BNE   SORT52                                                           
         MVI   TIMTYP2,C'N'                                                     
                                                                                
SORT52   CLC   TSMOA,SPACES        MOA                                          
         BE    SORT54                                                           
         CLC   TSMOA,ZEROS                                                      
         BE    SORT54                                                           
         MVC   WORK(L'TSMOA),TSMOA                                              
         MVI   WORK+L'TSMOA,X'01'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(9,MOA2)                                    
                                                                                
SORT54   CLC   TSHRS,ZEROS                                                      
         BE    SORT56                                                           
         CURED (P3,TSHRS),(6,HOURS2),2                                          
SORT56   MVC   TIMACC2,TSACC                                                    
         MVC   SWORK2,TSWC                                                      
         TM    TSCHG,X'08'         MAT ADD/CHG/DEL OR TMS DEL                   
         BO    SORT58                                                           
         MVC   NARR2,TSNARR                                                     
         MVC   ORD2,TSORD                                                       
         MVC   REF2,TSREF                                                       
         B     SORT70                                                           
                                                                                
SORT58   CLI   QOPT2,C'M'          ARE WE REPORTING ON MATERIALS?               
         BNE   SORT70                                                           
                                                                                
         MVC   MATNAR2,TSMNARR                                                  
                                                                                
SORT60   CLC   TSMTOT,ZEROS        MATERIAL TOTAL                               
         BE    SORT62                                                           
         CURED (P6,TSMTOT),(12,TOTAL2),2                                        
                                                                                
SORT62   MVC   CODE2,TSMCOD                                                     
         CLC   CODE2,ZEROS                                                      
         BNE   SORT64                                                           
         MVC   PRICE2,SPACES                                                    
         MVC   MULT2,SPACES                                                     
         MVC   ITMDESC,SPACES                                                   
         B     SORT70                                                           
SORT64   BAS   RE,ITMDES                                                        
         MVC   ITMDESC2,ITMDESC                                                 
                                                                                
SORT66   CLC   TSMPRI,ZEROS        PRICE OF ITEM                                
         BE    SORT68                                                           
         CURED (P6,TSMPRI),(12,PRICE2),2                                        
                                                                                
SORT68   CLC   TSMMULT,ZEROS       NUMBER OF ITEMS                              
         BE    SORT70                                                           
         CURED (P6,TSMMULT),(12,MULT2),2                                        
                                                                                
SORT70   LA    R4,TSTABL(R4)                                                    
         EJECT                                                                  
***********************************************************************         
* OUTPUT "TO" INFORMATION                                             *         
***********************************************************************         
SORT72   CLI   TSTYPE,1                                                         
         BNE   *+12                                                             
         MVI   TIMTYPE,C'B'                                                     
         B     SORT73                                                           
         CLI   TSTYPE,2                                                         
         BNE   *+12                                                             
         MVI   TIMTYPE,C'R'                                                     
         B     SORT73                                                           
         CLI   TSTYPE,3                                                         
         BNE   *+12                                                             
         MVI   TIMTYPE,C'N'                                                     
         B     SORT73                                                           
         CLI   TSTYPE,X'10'                                                     
         BNE   SORT73                                                           
         MVI   TIMTYPE,C'N'                                                     
                                                                                
SORT73   CLC   TSMOA,SPACES        MOA                                          
         BE    SORT74                                                           
         CLC   TSMOA,ZEROS                                                      
         BE    SORT74                                                           
         MVC   WORK(L'TSMOA),TSMOA                                              
         MVI   WORK+L'TSMOA,X'01'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(9,MOA)                                     
                                                                                
SORT74   CLC   TSHRS,ZEROS                                                      
         BE    SORT76                                                           
         CURED (P3,TSHRS),(6,HOURS),2,MINUS=YES                                 
*                                                                               
SORT76   MVC   BINPID,TSPID        GET PERSONID                                 
         BAS   RE,GETPID                                                        
         MVC   PERSON2,PERCOD                                                   
         MVC   USER,TSUSER         GET USERID                                   
         BAS   RE,GETUSR                                                        
         MVC   PERSON,TSPERS                                                    
         MVC   PERFSTNM,TSPERFNM                                                
         MVC   PERLSTNM,TSPERLNM                                                
         MVC   TIMACC1,TSACC                                                    
         MVC   SWORK,TSWC                                                       
         TM    TSCHG,X'08'         MAT ADD/CHG/DEL OR TMS DEL                   
         BO    *+10                                                             
         MVC   NARR,TSNARR         NARRATIVE                                    
         MVC   ORD,TSORD                                                        
         MVC   REF,TSREF                                                        
         CLC   COMPACT1,ZEROS                                                   
         BNE   *+14                                                             
         MVC   STATUS,SPACES                                                    
         B     SORT82                                                           
                                                                                
SORT78   CLI   STSTAT,X'00'        IS THERE A STATUS                            
         BNE   SORT80                                                           
         MVC   STATUS,SPACES                                                    
         B     SORT82                                                           
                                                                                
SORT80   TM    STSTAT,X'80'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@DELD      DELETED?                                     
         TM    STSTAT,X'40'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@FUAPR     FULLY APPROVED?                              
         TM    STSTAT,X'20'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@ESSUB     SUBMITTED?                                   
         TM    STSTAT,X'10'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@ESPAP     PART APPROVED?                               
         TM    STSTAT,X'08'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@AWLMA     AWAITING LINE MANAGER APPROVAL?              
         TM    STSTAT,X'02'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@ESREJ     REJECTED?                                    
         TM    STSTAT,X'01'                                                     
         BZ    *+10                                                             
         MVC   STATUS,AC@LIMA      LINE MANAGER APPROVED?                       
                                                                                
SORT82   CLI   TSCHG,X'01'         WHAT IS THE TYPE OF CHANGE?                  
         BNE   *+14                                                             
         MVC   SWHAT,AC@RWCHG      ROW CHANGED                                  
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'02'                                                      
         BNE   *+14                                                             
         MVC   SWHAT,AC@RWDEL      ROW DELETED                                  
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'03'                                                      
         BNE   *+14                                                             
         MVC   SWHAT,AC@RWADD      ROW ADDED                                    
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'04'                                                      
         BNE   *+14                                                             
         MVC   SWHAT,AC@TSCHG      TIMESHEET CHANGED                            
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'05'         TIMESHEET ADDED                              
         BNE   *+14                                                             
         MVC   SWHAT,AC@TSADD                                                   
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'08'         MATERIAL CHANGED                             
         BNE   *+14                                                             
         MVC   SWHAT,AC@MTCHG                                                   
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'09'         MATERIAL DELETED                             
         BNE   *+14                                                             
         MVC   SWHAT,AC@MTDEL                                                   
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'0A'         MATERIAL ADDED                               
         BNE   *+14                                                             
         MVC   SWHAT,AC@MTADD                                                   
         B     SORT84                                                           
                                                                                
         CLI   TSCHG,X'0B'         TIMESHEET DELETED                            
         BNE   SORT84                                                           
         MVC   SWHAT,AC@TSDEL                                                   
                                                                                
SORT84   GOTO1 DATCON,DMCB,(1,TSPEND),(17,DATE2)                                
         GOTO1 DATCON,DMCB,(1,TSDATE),(17,DATE)                                 
         GOTO1 TIMEOUT,DMCB,(3,TSTIME),(3,DUB)                                  
         MVC   TIMCHG(L'TIMCHG),DUB+7                                           
         CLC   TSROW,ZEROS                                                      
         BE    SORT86                                                           
         CURED (B2,TSROW),(4,ROW),0,ZERO=NO,ALIGN=LEFT                          
                                                                                
SORT86   MVC   STATUS4,SPACES                                                   
         CLI   TSROWCHG,0          EXTRACT ROW CHANGE INFORMATION               
         BNE   SORT87                                                           
         CLI   TSROWCH2,0                                                       
         BE    SORT100                                                          
                                                                                
SORT87   LA    R2,STATUS4                                                       
         MVI   BYTE3,X'00'                                                      
                                                                                
         TM    TSROWCHG,X'80'      ACCOUNT                                      
         BNO   SORT88                                                           
         MVC   0(L'AC@RSACC,R2),AC@RSACC                                        
         LA    R2,L'AC@RSACC(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT88   TM    TSROWCHG,X'40'      TIME TYPE                                    
         BNO   SORT90                                                           
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@TYPE,R2),AC@TYPE                                          
         LA    R2,L'AC@TYPE(R2)                                                 
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT90   TM    TSROWCHG,X'20'      HOURS                                        
         BNO   SORT92                                                           
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@RSHRS,R2),AC@RSHRS                                        
         LA    R2,L'AC@RSHRS(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT92   TM    TSROWCHG,X'10'      WC                                           
         BNO   SORT94                                                           
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@WC,R2),AC@WC                                              
         LA    R2,L'AC@WC(R2)                                                   
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT94   TM    TSROWCHG,X'08'      NARRATIVE                                    
         BNO   SORT96                                                           
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@NART,R2),AC@NART                                          
         LA    R2,L'AC@NART(R2)                                                 
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT96   TM    TSROWCHG,X'04'      ORDER                                        
         BNO   SORT98                                                           
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@ORDER,R2),AC@ORDER                                        
         LA    R2,L'AC@ORDER(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT98   TM    TSROWCHG,X'02'      INTERNAL REFERENCE                           
         BNO   SORT99                                                           
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@RSREF,R2),AC@RSREF                                        
         LA    R2,L'AC@RSREF(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT99   TM    TSROWCH2,X'80'      MOA CHANGE                                   
         BNO   SORT100                                                          
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C','                                                    
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@MOA,R2),AC@MOA                                            
                                                                                
SORT100  CLI   QOPT2,C'M'          ARE WE REPORTING ON MATERIALS?               
         BNE   SORT120                                                          
         TM    TSCHG,X'08'         MATERIAL CHANGE?                             
         BNO   SORT120                                                          
         MVC   ITMDESC,SPACES                                                   
                                                                                
         MVC   MATNARR,TSMNARR                                                  
                                                                                
         MVC   STATUS3,SPACES                                                   
         CLI   TSMSTAT,0                                                        
         BNE   *+8                                                              
         B     SORT110                                                          
                                                                                
         LA    R2,STATUS3          TYPE OF MATERIAL CHANGE                      
         MVI   BYTE3,X'00'                                                      
                                                                                
         TM    TSMSTAT,X'80'       TOTAL                                        
         BNO   SORT102                                                          
         MVC   0(L'AC@TOTAL,R2),AC@TOTAL                                        
         LA    R2,L'AC@TOTAL(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT102  TM    TSMSTAT,X'40'       CODE                                         
         BNO   SORT104                                                          
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@CODE,R2),AC@CODE                                          
         LA    R2,L'AC@CODE(R2)                                                 
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT104  TM    TSMSTAT,X'20'       PRICE                                        
         BNO   SORT106                                                          
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@PRICE,R2),AC@PRICE                                        
         LA    R2,L'AC@PRICE(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT106  TM    TSMSTAT,X'10'       UNITS                                        
         BNO   SORT108                                                          
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@UNITS,R2),AC@UNITS                                        
         LA    R2,L'AC@UNITS(R2)                                                
         MVI   BYTE3,X'01'                                                      
                                                                                
SORT108  TM    TSMSTAT,X'08'       NARRATIVE                                    
         BNO   SORT110                                                          
         CLI   BYTE3,X'01'                                                      
         BNE   *+14                                                             
         MVC   0(1,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@NART,R2),AC@NART                                          
         LA    R2,L'AC@NART(R2)                                                 
                                                                                
SORT110  CLC   TSMTOT,ZEROS        MATERIAL TOTAL                               
         BE    SORT116                                                          
         CURED (P6,TSMTOT),(12,TOTAL),2                                         
                                                                                
SORT112  MVC   CODE,TSMCOD                                                      
         CLC   CODE,ZEROS                                                       
         BNE   SORT114                                                          
         MVC   TSMPRI,SPACES                                                    
         MVC   TSMMULT,SPACES                                                   
         B     SORT120                                                          
SORT114  BAS   RE,ITMDES                                                        
*                                                                               
SORT116  CLC   TSMPRI,ZEROS        PRICE OF ITEM                                
         BE    SORT118                                                          
         CURED (P6,TSMPRI),(12,PRICE),2                                         
                                                                                
SORT118  CLC   TSMMULT,ZEROS       NUMBER OF ITEMS                              
         BE    SORT120                                                          
         CURED (P6,TSMMULT),(12,MULT),2                                         
                                                                                
SORT120  CLI   QOPT7,C'Y'          DOWNLOADABLE?                                
         BE    *+12                                                             
         BAS   RE,MYREPORT                                                      
         B     *+8                                                              
         BAS   RE,MYDOWN                                                        
         B     SORT28                                                           
                                                                                
         EJECT                                                                  
***********************************************************************         
*  LAST FOR RUN/REQUEST                                               *         
***********************************************************************         
RUNL     CLI   QOPT7,C'Y'                                                       
         BNE   RUNL2                                                            
         TM    RUNIND,RUNIINI                                                   
         BZ    XIT                                                              
         LA    RF,DLBUFF                                                        
         USING DLCBD,RF                                                         
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         B     EXIT                                                             
*                                                                               
RUNL2    MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT PAGE                                                          *         
***********************************************************************         
MYREPORT NTR1  ,                                                                
         USING PLINED,R2                                                        
         USING BOXD,R6                                                          
*        MVC   BOXWIDTH,=F'198'                                                 
         CLI   QOPT7,C'Y'                                                       
         BE    XIT                                                              
         CLC   DATE3,ZEROS         IS IT THE FIRST LINE                         
         BE    MYREP05                                                          
         CLC   DATE2,DATE3         IS THE DATE DIFFERENT TO PREV                
         BNE   MYREP05                                                          
         CLC   PERSON,PERSON3      IS THE PERSON DIFFERENT TO PREVIOUS          
         BE    MYREP10                                                          
                                                                                
MYREP05  MVI   FORCEHED,C'Y'                                                    
         L     R6,ADBOX                                                         
         MVC   DATE3,DATE2                                                      
         MVC   PERSON3,PERSON                                                   
         B     MYREP10                                                          
                                                                                
MYREP10  LA    R2,XP                                                            
         MVC   XHEAD5+12(8),PERSON                                              
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'PERFSTNM),PERFSTNM                                        
         MVC   WORK+(L'PERFSTNM+1),PERLSTNM                                     
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         MVC   XHEAD5+22(L'WORK),WORK                                           
         MVC   XHEAD6+12(8),DATE2                                               
*&&US                                                                           
         CLI   QOPT3,C'Y'          SHOW DISK ADDRESS FOR DEBUGGING              
         BNE   *+10                                                             
         MVC   XHEAD7+1(3),=C'D/A'                                              
*&&US                                                                           
         LA    RE,TSDA             FIND NEXT OPEN SLOT                          
         LA    RF,XHEAD7+12                                                     
         LA    R0,6                MAX                                          
MYREP10A OC    0(L'KEYDA,RE),0(RE) OPEN SLOT?                                   
         BZ    MYREP10B                                                         
         MVC   WORK(L'KEYDA),0(RE)                                              
         CHI   R0,6                FIRST TIME?                                  
         BE    *+12                YES - SKIP COMMA                             
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         STM   RE,RF,SVREG                                                      
         GOTO1 HEXOUT,DMCB,WORK,(RF),L'KEYDA                                    
         LM    RE,RF,SVREG                                                      
         LA    RE,L'KEYDA(RE)                                                   
         LA    RF,8(RF)                                                         
         BCT   R0,MYREP10A                                                      
MYREP10B DS    0H                                                               
*&&                                                                             
         MVC   P1ROW,ROW                                                        
         MVC   P1TYPE2,TIMTYP2                                                  
         MVC   P1ACCN2,TIMACC2                                                  
         MVC   P1WC2,SWORK2                                                     
         MVC   P1HOURS2,HOURS2                                                  
         MVC   P1MOA2,MOA2                                                      
         MVC   P1TYPE,TIMTYPE                                                   
         MVC   P1ACCN,TIMACC1                                                   
         MVC   P1WC,SWORK                                                       
         MVC   P1HOURS,HOURS                                                    
         MVC   P1MOA,MOA                                                        
         MVC   P1STAT,STATUS                                                    
         MVC   P1ACT,SWHAT                                                      
         MVC   P1ACT2,STATUS4                                                   
         MVC   P1PID,PERSON2                                                    
         MVC   P1CPY,USER                                                       
         MVC   P1DATE,DATE                                                      
                                                                                
MYREPX   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* BOX ROUTINES (HOOK)                                                 *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1  ,                                                                
         CLI   RCSUBPRG,0                                                       
         BE    XIT                                                              
         L     RF,ADBOX                                                         
         USING BOXD,RF                                                          
                                                                                
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         USING PLINED,RE                                                        
         LA    RE,BOXCOLS                                                       
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         MVI   P1HOOK1,C'L'                                                     
         MVI   P1HOOK2,C'C'                                                     
         MVI   P1HOOK6,C'C'                                                     
         MVI   P1HOOK10,C'C'                                                    
         MVI   P1HOOK11,C'C'                                                    
         MVI   P1HOOK12,C'C'                                                    
         MVI   P1HOOK13,C'C'                                                    
         MVI   P1HOOK14,C'C'                                                    
         MVI   P1HOOK15,C'C'                                                    
         MVI   P1HOOK16,C'R'                                                    
                                                                                
HOOK5    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
                                                                                
         DROP  RE,RF                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* MY DOWN - PUT DATA TO DOWNLOAD REPORT (INCLUDING INITIALISATION)    *         
***********************************************************************         
         USING DLCBD,R2                                                         
MYDOWN   NTR1                                                                   
                                                                                
         L     RF,=V(DLFLD)                                                     
         ST    RF,DOWNLOAD                                                      
         TM    RUNIND,RUNIINI      CHECK IF INITIALISED                         
         BNZ   MYDOWN14                                                         
         OI    RUNIND,RUNIINI                                                   
         LA    R2,DLBUFF                                                        
         LA    RE,DLPLINE                                                       
         ST    RE,DLCBAPL                                                       
         LA    RE,DLPRINT                                                       
         ST    RE,DLCBAPR                                                       
         MVI   DLCBACT,DLCBSOR                                                  
         OI    DLCBFLG1,DLCBFXTN   USE EXTENDED CONTROL BLOCK                   
         GOTO1 DOWNLOAD,DLCBD                                                   
         MVC   DLPLINE,SPACES                                                   
                                                                                
*********************** DO HEADLINES FIRST*****************************         
         CLI   QOPT2,C'M'                                                       
         BNE   MYDOWN03                                                         
         LA    R0,17                                                            
MYDOWN01 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD,SPACES                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN01                                                      
                                                                                
         LA    R0,13                                                            
MYDOWN02 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@MAT),AC@MAT                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN02                                                      
                                                                                
         MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
MYDOWN03 LA    R0,3                                                             
MYDOWN04 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD,SPACES                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN04                                                      
                                                                                
         LA    R0,8                                                             
MYDOWN05 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@FROM),AC@FROM                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN05                                                      
                                                                                
         LA    R0,8                                                             
MYDOWN06 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@TO),AC@TO                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN06                                                      
                                                                                
         CLI   QOPT2,C'M'                                                       
         BNE   MYDOWN09                                                         
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD,SPACES                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R0,6                                                             
MYDOWN07 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@FROM),AC@FROM                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN07                                                      
                                                                                
         LA    R0,6                                                             
MYDOWN08 LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@TO),AC@TO                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         BCT   R0,MYDOWN08                                                      
                                                                                
MYDOWN09 MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TIMESHEET FOR PERSON                         
         MVC   DLCBFLD(L'AC@PRSN),AC@PRSN                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PERSON FIRST NAME                            
         MVC   DLCBFLD(L'AC@CFNAM),AC@CFNAM                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PERSON LAST  NAME                            
         MVC   DLCBFLD(L'AC@CLNAM),AC@CLNAM                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PERIOD END OF TIMESHEET                      
         MVC   DLCBFLD(L'AC@PERD),AC@PERD                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ROW NUMBER                                   
         MVC   DLCBFLD(L'AC@ROW),AC@ROW                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R0,2                                                             
MYDOWN10 LA    R2,DLBUFF           TIME TYPE                                    
         MVC   DLCBFLD(L'AC@TYPE),AC@TYPE                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ACCOUNT                                      
         MVC   DLCBFLD(L'AC@ACC),AC@ACC                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WORKCODE                                     
         MVC   DLCBFLD(L'AC@WC),AC@WC                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           HOURS                                        
         MVC   DLCBFLD(L'AC@HOURS),AC@HOURS                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           MOA                                          
         MVC   DLCBFLD(L'AC@MOA),AC@MOA                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           NARRATIVE                                    
         MVC   DLCBFLD(L'AC@NRTV),AC@NRTV                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ORDER                                        
         MVC   DLCBFLD(L'AC@ORDC),AC@ORDC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           REFERENCE                                    
         MVC   DLCBFLD(L'REFNAME),REFNAME                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         BCT   R0,MYDOWN10                                                      
                                                                                
         CLI   QOPT2,C'M'          ARE THEY REPORTING ON MATERIALS?             
         BNE   MYDOWN12                                                         
                                                                                
         LA    R2,DLBUFF           MATERIAL CHANGE - (ITEM ACTION)              
         MVC   DLCBFLD(L'AC@ITACT),AC@ITACT                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R0,2                                                             
MYDOWN11 LA    R2,DLBUFF           CODE                                         
         MVC   DLCBFLD(L'AC@ARTCD),AC@ARTCD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           DESCRIPTION                                  
         MVC   DLCBFLD(L'AC@DESC),AC@DESC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PRICE                                        
         MVC   DLCBFLD(L'AC@UNITP),AC@UNITP                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           MULT                                         
         MVC   DLCBFLD(L'AC@UNITS),AC@UNITS                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TOTAL                                        
         MVC   DLCBFLD(L'AC@TOTAL),AC@TOTAL                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           NARRATIVE                                    
         MVC   DLCBFLD(L'AC@NRTV),AC@NRTV                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         BCT   R0,MYDOWN11                                                      
                                                                                
MYDOWN12 LA    R2,DLBUFF           TMS STATUS TO                                
         MVC   DLCBFLD(L'AC@STT),AC@STT                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           CHANGE - (TIMESHEET ACTION)                  
         MVC   DLCBFLD(L'AC@TSACT),AC@TSACT                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           DESCRIPTION OF CHANGE                        
         MVC   DLCBFLD(L'AC@TLACT),AC@TLACT                                     
         MVI   DLCBACT,DLCBPUT     (TIMELINE ACTION)                            
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WHO MADE CHANGE                              
         MVC   DLCBFLD(L'AC@CPID),AC@CPID                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           USERID OF CHANGE                             
         MVC   DLCBFLD(L'AC@CPY),AC@CPY                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           DATE OF CHANGE                               
         MVC   DLCBFLD(L'AC@ACTYD),AC@ACTYD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TIME OF CHANGE                               
         MVC   DLCBFLD(L'AC@ACTTM),AC@ACTTM                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
MYDOWN13 MVI   DLCBACT,DLCBEOL     SET END OF LINE                              
         GOTO1 DOWNLOAD,DLCBD                                                   
*************************OUTPUT DATA***********************************         
MYDOWN14 DS    0H                                                               
         LA    R2,DLBUFF           TIMESHEET FOR PERSON                         
         MVC   DLCBFLD(L'PERSON),PERSON                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PERSON FIRST NAME                            
         MVC   DLCBFLD(L'PERFSTNM),PERFSTNM                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PERSON LAST  NAME                            
         MVC   DLCBFLD(L'PERLSTNM),PERLSTNM                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PERIOD END OF TIMESHEET                      
         MVC   DLCBFLD(L'DATE2),DATE2                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ROW NUMBER                                   
         MVC   DLCBFLD(L'ROW),ROW                                               
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TIME TYPE (FROM)                             
         MVC   DLCBFLD(L'TIMTYP2),TIMTYP2                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ACCOUNT (FROM)                               
         MVC   DLCBFLD(L'TIMACC2),TIMACC2                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WORKCODE (FROM)                              
         MVC   DLCBFLD(L'SWORK2),SWORK2                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           HOURS (FROM)                                 
         MVC   DLCBFLD(L'HOURS2),HOURS2                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           MOA (FROM)                                   
         MVC   DLCBFLD(L'MOA2),MOA2                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           NARRATIVE (FROM)                             
         MVC   DLCBFLD(L'NARR2),NARR2                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ORDER (FROM)                                 
         MVC   DLCBFLD(L'ORD2),ORD2                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           REFERENCE (FROM)                             
         MVC   DLCBFLD(L'REF2),REF2                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TIME TYPE (TO)                               
         MVC   DLCBFLD(L'TIMTYPE),TIMTYPE                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ACCOUNT (TO)                                 
         MVC   DLCBFLD(L'TIMACC1),TIMACC1                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WORKCODE (TO)                                
         MVC   DLCBFLD(L'SWORK),SWORK                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           HOURS (TO)                                   
         MVC   DLCBFLD(L'HOURS),HOURS                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           MOA (TO)                                     
         MVC   DLCBFLD(L'MOA),MOA                                               
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           NARRATIVE (TO)                               
         MVC   DLCBFLD(L'NARR),NARR                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ORDER (TO)                                   
         MVC   DLCBFLD(L'ORD),ORD                                               
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           REFERENCE (TO)                               
         MVC   DLCBFLD(L'REF),REF                                               
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
                                                                                
         CLI   QOPT2,C'M'          REPORTING ON MATERIALS?                      
         BNE   MYDOWN15                                                         
                                                                                
         LA    R2,DLBUFF           STATUS                                       
         MVC   DLCBFLD(L'STATUS3),STATUS3                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           CODE (FROM)                                  
         MVC   DLCBFLD(L'CODE2),CODE2                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ITEM DESCRIPTION (FROM)                      
         MVC   DLCBFLD(L'ITMDESC2),ITMDESC2                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PRICE (FROM)                                 
         MVC   DLCBFLD(L'PRICE2),PRICE2                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           MULTIPLIER (FROM)                            
         MVC   DLCBFLD(L'MULT2),MULT2                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TOTAL (FROM)                                 
         MVC   DLCBFLD(L'TOTAL2),TOTAL2                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           NARRATIVE (FROM)                             
         MVC   DLCBFLD(L'MATNAR2),MATNAR2                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           CODE (TO)                                    
         MVC   DLCBFLD(L'TSMCOD),TSMCOD                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           ITEM DESCRIPTION (TO)                        
         MVC   DLCBFLD(L'ITMDESC),ITMDESC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           PRICE (TO)                                   
         MVC   DLCBFLD(L'PRICE),PRICE                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           MULTIPLIER (TO)                              
         MVC   DLCBFLD(L'MULT),MULT                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TOTAL  (TO)                                  
         MVC   DLCBFLD(L'TOTAL),TOTAL                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           NARRATIVE  (TO)                              
         MVC   DLCBFLD(L'MATNARR),MATNARR                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
MYDOWN15 LA    R2,DLBUFF           STATUS                                       
         MVC   DLCBFLD(L'STATUS),STATUS                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WHAT WAS CHANGED                             
         MVC   DLCBFLD(L'SWHAT),SWHAT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WHAT WAS CHANGED                             
         MVC   DLCBFLD(L'STATUS4),STATUS4                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           WHO MADE CHANGE                              
         MVC   DLCBFLD(L'PERSON2),PERSON2                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           USERID OF CHANGE                             
         MVC   DLCBFLD(L'USER),USER                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           DATE OF CHANGE                               
         MVC   DLCBFLD(L'DATE),DATE                                             
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
         LA    R2,DLBUFF           TIME OF CHANGE                               
         MVC   DLCBFLD(L'TIMCHG),TIMCHG                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
MYDOWN16 MVI   DLCBACT,DLCBEOL     SET END OF LINE                              
         NI    DLCBFLG1,FFQ-DLCBFXFL                                            
         GOTO1 DOWNLOAD,DLCBD                                                   
                                                                                
MYDOWNX  B     EXIT                                                             
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD REPORT                                                     *         
***********************************************************************         
DLPRINT  NTR1                                                                   
         MVC   P,DLPLINE                                                        
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,1                                                        
         GOTO1 ACREPORT                                                         
         MVC   DLPLINE,SPACES                                                   
         MVI   LINE,1                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PERSON ID (FROM BINARY PID)                          *         
***********************************************************************         
GETPID   NTR1                                                                   
         XC    PERCOD,PERCOD                                                    
         USING SA0REC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SCMPY       SECURITY ALPHA ID                            
         MVC   SA0KNUM,BINPID                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,ADIO                            
         BNE   XIT                                                              
         L     R2,ADIO                                                          
         LA    R6,SA0DATA                                                       
         USING SAPALD,R6                                                        
         XR    R0,R0                                                            
                                                                                
GTPD02   CLI   SAPALEL,0                                                        
         BE    XIT                                                              
         CLI   SAPALEL,SAPALELQ                                                 
         BNE   GTPD04                                                           
         MVC   PERCOD,SAPALPID                                                  
         B     XIT                                                              
                                                                                
GTPD04   IC    R0,SAPALLN                                                       
         AR    R6,R0                                                            
         B     GTPD02                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET USERID                                               *         
***********************************************************************         
GETUSR   NTR1                                                                   
         USING CTIREC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,USER                                                     
         MVC   USER(10),SPACES                                                  
         GOTOR DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,ADIO                            
         BNE   GETUSR8                                                          
         L     R2,ADIO                                                          
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
         XR    R0,R0                                                            
                                                                                
GETUSR2  CLI   CTDSCEL,CTDSCELQ                                                 
         BE    GETUSR4                                                          
         CLI   CTDSCEL,0                                                        
         BE    GETUSR8                                                          
         IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         B     GETUSR2                                                          
                                                                                
                                                                                
GETUSR4  MVC   USER(10),CTDSC                                                   
                                                                                
                                                                                
GETUSR8  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ITEM DESCRIPTION                                     *         
***********************************************************************         
ITMDES   NTR1                                                                   
         USING ARTRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         MVC   ARTKTYP,SPACES                                                   
         MVI   ARTKTYP,ARTKTYPQ                                                 
         MVI   ARTKSUB,ARTKAQ                                                   
         MVC   ARTKCPY,CMPY                                                     
         MVC   ARTKART,TSMCOD                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,IOKEY                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEYDA,ARTKDA                                                     
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,KEYDA,ADIO,DMWORK                      
         L     R2,ADIO                                                          
         LA    R6,ARTRFST                                                       
         USING NAMELD,R6                                                        
         XR    R0,R0                                                            
         MVC   ITMDESC,SPACES                                                   
                                                                                
ID01     CLI   NAMEL,0                                                          
         BE    IDX                                                              
         CLI   NAMEL,NAMELQ                                                     
         BNE   ID02                                                             
         XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         AHI   R1,-2                                                            
         MVC   ITMDESC(0),NAMEREC                                               
         EX    R1,*-6                                                           
         B     IDX                                                              
                                                                                
ID02     IC    R0,NAMEL                                                         
         AR    R6,R0                                                            
         B     ID01                                                             
                                                                                
IDX      B     XIT                                                              
         DROP  R2,R6                                                            
***********************************************************************         
* ROUTINE TO GET INTERNAL TIMELINE REFERENCE                          *         
***********************************************************************         
INTREF   NTR1                                                                   
         USING CPYRECD,R2       GET TIMELINE REF NAME FROM COMPANY              
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            RECORD                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,RCCOMPFL                                                 
         MVC   SAVEKEY,CPYKCPY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,IOKEY                           
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
                                                                                
         MVC   KEYDA,CPYKDA                                                     
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,KEYDA,ADIO,DMWORK                      
                                                                                
         L     R2,ADIO                                                          
         LA    R6,CPYRFST                                                       
         USING FFTELD,R6                                                        
         XR    R0,R0                                                            
         MVC   REFNAME,SPACES                                                   
                                                                                
IR01     CLI   FFTEL,0                                                          
         BE    IR04                                                             
         CLI   FFTEL,FFTELQ                                                     
         BNE   IR02                                                             
         CLI   FFTTYPE,FFTTTINR                                                 
         BE    IR03                                                             
                                                                                
IR02     IC    R0,FFTLN                                                         
         AR    R6,R0                                                            
         B     IR01                                                             
                                                                                
IR03     IC    RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   REFNAME(0),FFTDATA                                               
         EX    RF,*-6                                                           
         OC    REFNAME,SPACES      CONVERT TO UPPERCASE                         
         B     XIT                                                              
                                                                                
IR04     MVC   REFNAME,TIMLREF     MOVE IN DEFAULT "TIMELINE REFERENCE"         
         B     XIT                 IF NOT FOUND                                 
                                                                                
         EJECT                                                                  
         DROP  R2,R6                                                            
***********************************************************************         
* ROUTINE TO CLEAR OUTPUT FIELDS                                      *         
***********************************************************************         
CLEAR    NTR1                                                                   
         MVC   DATE,SPACES                                                      
         MVC   DATE2,SPACES                                                     
         MVC   TIMCHG,SPACES                                                    
         MVC   NARR,SPACES                                                      
         MVC   NARR2,SPACES                                                     
         MVC   MATNARR,SPACES                                                   
         MVC   MATNAR2,SPACES                                                   
         MVC   SWHAT,SPACES                                                     
         MVC   STATUS4,SPACES                                                   
         MVC   PERSON,SPACES                                                    
         MVC   ITMDESC,SPACES                                                   
         MVC   ITMDESC2,SPACES                                                  
         MVC   HOURS,SPACES                                                     
         MVC   HOURS2,SPACES                                                    
         MVC   TIMTYPE,SPACES                                                   
         MVC   TIMTYP2,SPACES                                                   
         MVC   TIMACC2,SPACES                                                   
         MVC   TIMACC1,SPACES                                                   
         MVC   SWORK2,SPACES                                                    
         MVC   SWORK,SPACES                                                     
         MVC   ROW,SPACES                                                       
         MVC   REF2,SPACES                                                      
         MVC   REF,SPACES                                                       
         MVC   ORD2,SPACES                                                      
         MVC   ORD,SPACES                                                       
         MVC   STATUS3,SPACES                                                   
         MVC   PRICE,SPACES                                                     
         MVC   PRICE2,SPACES                                                    
         MVC   MULT2,SPACES                                                     
         MVC   MULT,SPACES                                                      
         MVC   TOTAL2,SPACES                                                    
         MVC   TOTAL,SPACES                                                     
         MVC   CODE2,SPACES                                                     
         MVC   CODE,SPACES                                                      
         MVC   MOA,SPACES                                                       
         MVC   MOA2,SPACES                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DICTIONARY ENTRIES                                                  *         
***********************************************************************         
DDIN     EQU   *                                                                
         DCDDL AC#ACC,7,L          ACCOUNT                                      
         DCDDL AC#TYPE,4,L         TYPE                                         
         DCDDL AC#PERD,8,L         PERIOD                                       
         DCDDL AC#PRSN,8,L         PERSON                                       
         DCDDL AC#CPID,11,L        PERSONAL ID                                  
         DCDDL AC#ACTYD,13,L       ACTIVITY DATE                                
         DCDDL AC#ACTTM,13,L       ACTIVITY TIME                                
         DCDDL AC#TOTAL,5,L        TOTAL (MATERIAL)                             
         DCDDL AC#FROM,4,L         FROM                                         
         DCDDL AC#TO,4,L           TO                                           
         DCDDL AC#STT,6,L          STATUS                                       
         DCDDL AC#WC,2,C           WORKCODE                                     
         DCDDL AC#ITACT,11,C       ITEM ACTION                                  
         DCDDL AC#TSACT,16,C       TIMESHEET ACTION                             
         DCDDL AC#TLACT,16,C       TIME LINE ACTION                             
         DCDDL AC#HOURS,7,L        HOURS                                        
         DCDDL AC#DATE,4,L         DATE                                         
         DCDDL AC#CHGD,7,L         CHANGED                                      
         DCDDL AC#ROW,3,L          ROW                                          
         DCDDL AC#ORDC,5,L         ORDER                                        
         DCDDL AC#NRTV,9,L         NARRATIVE                                    
         DCDDL AC#FUAPR,16,L       FULLY APPROVED                               
         DCDDL AC#ESSUB,16,L       SUBMITTED                                    
         DCDDL AC#ESPAP,16,L       PART APPROVED                                
         DCDDL AC#AWLMA,16,L       AWAITING LINE MANAGER APPROVAL               
         DCDDL AC#ESREJ,16,L       REJECTED                                     
         DCDDL AC#LIMA,16,L        LINE MANAGER APPROVED                        
         DCDDL AC#INPRO,16,L       IN PROGRESS                                  
         DCDDL AC#DELD,16,L        DELETED                                      
         DCDDL AC#CPY,7,L          COMPANY                                      
         DCDDL AC#UNITP,10,L       UNIT PRICE                                   
         DCDDL AC#UNITS,5,L        UNITS                                        
         DCDDL AC#ARTCD,9,L        ITEM CODE                                    
         DCDDL AC#RSHRS,3,L        HOURS (HRS)                                  
         DCDDL AC#RSACC,3,L        ACCOUNT (ACC)                                
         DCDDL AC#NART,4,L         NARRATIVE (NARR)                             
         DCDDL AC#ORDER,3,L        ORDER (ORD)                                  
         DCDDL AC#RSREF,3,L        REFERENCE (REF)                              
         DCDDL AC#CODE,4,L         CODE                                         
         DCDDL AC#PRICE,5,L        PRICE                                        
         DCDDL AC#RWCHG,17,L       ROW CHANGED                                  
         DCDDL AC#RWDEL,17,L       ROW DELETED                                  
         DCDDL AC#RWADD,17,L       ROW ADDED                                    
         DCDDL AC#TSCHG,17,L       TIMESHEET CHANGED                            
         DCDDL AC#TSADD,17,L       TIMESHEET ADDED                              
         DCDDL AC#MTCHG,17,L       MATERIAL CHANGED                             
         DCDDL AC#MTDEL,17,L       MATERIAL DELETED                             
         DCDDL AC#MTADD,17,L       MATERIAL ADDED                               
         DCDDL AC#TSDEL,17,L       TIMESHEET DELETED                            
         DCDDL AC#DESC,11,L        DESCRIPTION                                  
         DCDDL AC#MAT,9,L          MATERIALS                                    
         DCDDL AC#MOA,3,L          MOA                                          
         DCDDL AC#CFNAM,10,L       FIRST NAME                                   
         DCDDL AC#CLNAM,9,L        LAST NAME                                    
         EJECT                                                                  
***********************************************************************         
* MISCELLANEOUS                                                       *         
***********************************************************************         
TIMLREF  DC    C'TIMELINE REFERENCE'                                            
ZEROS    DC    X'00000000000000'                                                
EFFFS    DC    X'FFFFFFFFFFFFFF'                                                
TIMEOUT  DC    V(TIMEOUT)                                                       
         LTORG                                                                  
                                                                                
TRANS    DC    X'00020300040100000203000400000000'                              
* TRANSLATE WORD TO SORT STCTTYP IN THE FOLLOWING ORDER                         
*                                  ROW ADD (00) (&MAT ADD)                      
*                                  TMS ADD (01)                                 
*                                  ROW CHG (02) (&MAT CHG)                      
*                                  ROW DEL (03) (&MAT DEL)                      
*                                  TMS CHG (04) (&TMS DEL)                      
                                                                                
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
CTFILE   DC    C'CTFILE '                                                       
                                                                                
STTABLE  DC    (STTABNUM)XL(STTABL)'00'                                         
STTABEND DC    3X'FF'              END OF TABLE MARKER                          
STTABNUM EQU   500                 MAXIMUM NUMBER OF TABLE ENTRIES              
                                                                                
TSTABLE  DC    (TSTABNUM)XL(TSTABL)'00'                                         
TSTABEND DC    3X'FF'              END OF TABLE MARKER                          
TSTABNUM EQU   7500                MAXIMUM NUMBER OF TABLE ENTRIES              
                                                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACTAD    DSECT                                                                  
TSTABAD  DS    A                   ADDRESS OF TIMESHEET TABLE                   
TSTABAD2 DS    A                   SAVE ADDRESS WHEN READING TS+1               
STTABAD  DS    A                   ADDRESS OF STATUS TABLE                      
STTABAD2 DS    A                   SAVE ADDRESS WHEN READING ST+1               
ADBOX    DS    A                   ADDRESS OF BOX                               
TSTABCNT DS    F                   TIMESHEET TABLE COUNT                        
STTABCNT DS    F                   STATUS CHANGE TABLE COUNT                    
                                                                                
MATNARR  DS    CL42                MATERIAL NARRATIVE                           
MATNAR2  DS    CL42                MATERIAL NARRATIVE                           
NARR2    DS    CL42                NARRATIVE FROM                               
NARR     DS    CL42                NARRATIVE TO                                 
SWHAT    DS    CL17                WHAT HAS CHANGED                             
STATUS4  DS    CL36                ROW CHANGE                                   
STATUS3  DS    CL32                MATERIAL STATUS                              
ITMDESC  DS    CL36                ITEM DESCRIPTION                             
ITMDESC2 DS    CL36                ITEM DESCRIPTION                             
TIMACC1  DS    CL14                ACCOUNT TO                                   
TIMACC2  DS    CL14                ACCOUNT FROM                                 
TOTAL    DS    CL12                MATERIALS TOTAL                              
TOTAL2   DS    CL12                MATERIALS TOTAL                              
PRICE    DS    CL12                MATERIALS PRICE                              
PRICE2   DS    CL12                MATERIALS PRICE                              
MULT     DS    CL12                MATERIALS MULTIPLIER                         
MULT2    DS    CL12                MATERIALS MULTIPLIER                         
REF      DS    CL12                REFERENCE TO                                 
REF2     DS    CL12                REFERENCE FROM                               
PERSON   DS    XL8                 PERSON (TIMESHEET FOR)                       
PERFSTNM DS    CL15                       PERSON FIRST NAME                     
PERLSTNM DS    CL36                       PERSON LAST NAME                      
ORD      DS    CL6                 ORDER TO                                     
ORD2     DS    CL6                 ORDER FROM                                   
HOURS    DS    CL6                 HOUTS TO                                     
HOURS2   DS    CL6                 HOURS FROM                                   
ROW      DS    CL4                 ROW NUMBER                                   
CODE     DS    CL4                 TIMESHEET MATERIAL CODE                      
CODE2    DS    CL4                 TIMESHEET MATERIAL CODE                      
SWORK2   DS    CL2                 WORKCODE FROM                                
SWORK    DS    CL2                 WORKCODE TO                                  
DATE     DS    CL8                 DATE OF CHANGE                               
DATE2    DS    CL8                 TIMESHEET PERIOD END DATE                    
TIMCHG   DS    CL5                 TIME OF CHANGE                               
TIMTYPE  DS    XL1                 TYPE OF TIME TO                              
TIMTYP2  DS    XL1                 TYPE OF TIME FROM                            
*&&UK                                                                           
MOA      DS    CL5                 MONTH OF ACTIVITY TO                         
MOA2     DS    CL5                 MONTH OF ACTIVITY FROM                       
*&&                                                                             
*&&US                                                                           
MOA      DS    CL6                 MONTH OF ACTIVITY TO                         
MOA2     DS    CL6                 MONTH OF ACTIVITY FROM                       
*&&                                                                             
                                                                                
REFNAME  DS    CL36                REFERENCE READ FROM COMPANY RECORD           
STATUS   DS    CL16                STATUS OF THE TIMESHEET                      
USER     DS    CL10                USERID                                       
PERSON2  DS    XL8                 PERSON (WHO MADE CHANGE)                     
PERSON3  DS    XL8                 PERSON USED TO OUTPUT NEW PAGE               
PERCOD   DS    XL8                                                              
DATE3    DS    CL8                 TMS PERIOD USED TO OUTPUT NEW PAGE           
COMPACT1 DS    PL7                 STATUS ACTIVITY DATE FOR COMPARISON          
COMPACT2 DS    PL7                 STATUS ACTIVITY DATE FOR COMPARISON          
ROFF     DS    CL2                 REQUESTED OFFICE                             
RDEP     DS    CL6                 REQUESTED DEPARTMENT                         
RSUB     DS    CL6                 REQUESTED SUBDEPARTMENT                      
RPERS    DS    CL8                 REQUESTED PERSON                             
KEYDA    DS    XL4                 DISK ADDRESS                                 
SVREG    DS    4F                  SAVED AREA FOR UPTO 4 REGISTERS              
STARTCC  DS    PL3                 2'S COMPLEMENT START DATE                    
ENDCC    DS    PL3                 2'S COMPLEMENT END DATE                      
PEDT     DS    PL3                 PERIOD END DATE TIMESHEET                    
PEND     DS    PL3                 USED TO RESET TMS STATUS BYTE                
START    DS    XL3                 START DATE                                   
END      DS    XL3                 END DATE                                     
LOC2C    DS    XL24                2'S COMP OF LOCATION START & END             
ACTSTRT  DS    XL3                 ACTIVITY END                                 
ACTEND   DS    XL3                 ACTIVITY END                                 
BINPID   DS    XL2                 BINARY PID                                   
SCMPY    DS    CL2                 SECURITY COMPANY ID                          
COMPARE  DS    0CL4                                                             
COMPROW  DS    CL2                 ROW FOR COMPARE                              
COMPMAT  DS    CL2                 MATERIAL ROW FOR COMPARE                     
CMPY     DS    CL1                 COMPANY                                      
BYTE1    DS    XL1                 BYTE FOR OPTION 1                            
BYTE2    DS    XL1                 TEST BYTE FOR REQL                           
BYTE3    DS    XL1                 TEST BYTE FOR SPACING IN ROW/MAT CHG         
LALEN    DS    XL1                 LENGTH OF LEVEL 1                            
LBLEN    DS    XL1                 LENGTH OF LEVEL 2                            
LCLEN    DS    XL1                 LENGTH OF LEVEL 3                            
LDLEN    DS    XL1                 LENGTH OF LEVEL 4                            
FFQ      EQU   X'FF'                                                            
RUNIND   DS    XL1                 RUN INDICATOR                                
RUNIINI  EQU   X'01'               DOWNLOAD INITIALISED                         
RUNIEND  EQU   X'10'               RUN ENDED                                    
SPACES2  DC    XL52'40'                                                         
DOWNLOAD DS    V                                                                
IOKEY    DS    CL64                KEY FOR ACCDIR IO                            
MYKEY    DS    CL42                MY KEY                                       
SAVEKEY  DS    CL42                SAVED KEY                                    
DDOUT    EQU   *                                                                
         DSDDL PRINT=YES                                                        
DLSTAT   DS    C                                                                
DLBUFF   DS    CL(DLCBXLX)         DOWNLOAD BUFFERS                             
         DS    CL120                                                            
DLPLINE  DS    CL(L'P)                                                          
         DS    CL(2*L'P)                                                        
         EJECT                                                                  
***********************************************************************         
* STATUS TABLE DSECT                                                  *         
***********************************************************************         
STTABD   DSECT                                                                  
STTAST   DS    0H                                                               
STPERS   DS    CL8                 TIMESHEET FOR PERSON                         
STPEND   DS    PL3                 PERIOD END DATE TIMESHEET                    
STDATE   DS    PL3                 DATE OF CHANGE                               
STTIME   DS    PL4                 TIME OF CHANGE (USED FOR SORTING)            
STSTAT   DS    XL1                 STATUS OF TIMESHEET                          
STPID    DS    XL2                 PERSON WHO MADE CHANGE                       
STUSER   DS    XL2                 USERID OF CHANGE                             
STPERFNM DS    CL(L'PERFSTNM)      PERSON - FIRST NAME                          
STPERLNM DS    CL(L'PERLSTNM)      PERSON - LAST  NAME                          
STTABL   EQU   *-STTAST                                                         
***********************************************************************         
* TIMESHEET TABLE DSECT                                               *         
***********************************************************************         
TSTABD   DSECT                                                                  
TSFST    DS    0H                                                               
TSPERS   DS    CL8                 TIMESHEET FOR PERSON                         
TSPEND   DS    PL3                 PERIOD END DATE TIMESHEET                    
TSPERFNM DS    CL(L'PERFSTNM)      PERSON - FIRST NAME                          
TSPERLNM DS    CL(L'PERLSTNM)      PERSON - LAST  NAME                          
TSROWCOM DS    0XL4                                                             
TSROW    DS    XL2                 ROW NUMBER                                   
TSMROW   DS    XL2                 MATERIAL ROW NUMBER                          
TSDATE   DS    PL3                 DATE OF CHANGE                               
TSTIME   DS    PL4                 TIME OF CHANGE (USED FOR SORTING)            
TSCHG2   DS    XL1                 TRANSLATED VERSION OF CHANGES                
TSPID    DS    XL2                 PERSON WHO MADE CHANGE                       
TSUSER   DS    XL2                 USERID                                       
TSCHG    DS    XL1                 WHAT HAS CHANGED                             
TSROWCHG DS    XL1                 ROW CHANGE                                   
TSTYPE   DS    XL1                 TIME TYPE                                    
TSACC    DS    XL14                ACCOUNT                                      
TSWC     DS    CL2                 WORKCODE                                     
TSHRS    DS    PL3                 HOURS                                        
TSMOA    DS    PL2                 TIMELINE MOA                                 
TSORD    DS    CL6                 ORDER                                        
TSREF    DS    CL12                REFERENCE                                    
TSNARR   DS    CL42                NARRATIVE                                    
TSROWCH2 DS    XL1                 ADDITIONAL ROW CHANGE BYTE                   
TSMSTAT  DS    XL1                 MATERIAL STATUS                              
TSMTOT   DS    PL6                 MATERIAL TOTAL                               
TSMPRI   DS    PL6                 MATERIAL PRICE (PER UNIT)                    
TSMMULT  DS    PL6                 MATERIAL MULTIPLIER ( # UNITS)               
TSMCOD   DS    CL4                 ITEM CODE                                    
TSMNARR  DS    CL42                MATERIAL NARRATIVE                           
TSDA     DS    XL24                FOR REPORTING ALL DISK ADDRESSES             
TSTABL   EQU   *-TSFST             TABLE LENGTH                                 
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
PLINED   DSECT                                                                  
P1HOOK1  DS    CL1                                                              
P1ROW    DS    CL3                 ROW                                          
         DS    CL1                                                              
P1HOOK2  DS    CL1                                                              
P1TYPE2  DS    CL1                 TIME TYPE                                    
         DS    CL2                                                              
P1HOOK3  DS    CL1                                                              
P1ACCN2  DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
P1HOOK4  DS    CL1                                                              
P1WC2    DS    CL2                 WORKCODE                                     
*        DS    CL1                                                              
P1HOOK5  DS    CL1                                                              
P1HOURS2 DS    CL6                 HOURS                                        
         DS    CL1                                                              
P1HOOK17 DS    CL1                                                              
*&&UK                                                                           
P1MOA2   DS    CL5                                                              
         DS    CL2                                                              
*&&                                                                             
*&&US                                                                           
P1MOA2   DS    CL6                                                              
         DS    CL1                                                              
*&&                                                                             
P1HOOK6  DS    CL1                                                              
P1TYPE   DS    CL1                 TIME TYPE                                    
         DS    CL3                                                              
P1HOOK7  DS    CL1                                                              
P1ACCN   DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
P1HOOK8  DS    CL1                                                              
P1WC     DS    CL2                 WORKCODE                                     
         DS    CL1                                                              
P1HOOK9  DS    CL1                                                              
P1HOURS  DS    CL5                 HOURS                                        
         DS    CL1                                                              
P1HOOK18 DS    CL1                                                              
*&&UK                                                                           
P1MOA    DS    CL5                                                              
         DS    CL2                                                              
*&&                                                                             
*&&US                                                                           
P1MOA    DS    CL6                                                              
         DS    CL1                                                              
*&&                                                                             
P1HOOK10 DS    CL1                                                              
P1STAT   DS    CL16                STATUS FROM                                  
         DS    CL1                                                              
P1HOOK11 DS    CL1                                                              
P1ACT    DS    CL17                ACTION                                       
         DS    CL1                                                              
P1HOOK12 DS    CL1                                                              
P1ACT2   DS    CL34                ROW CHANGE                                   
         DS    CL1                                                              
P1HOOK13 DS    CL1                                                              
P1PID    DS    CL8                 PERSON ID (WHO MADE CHANGE)                  
         DS    CL1                                                              
P1HOOK14 DS    CL1                                                              
P1CPY    DS    CL8                 USERID                                       
         DS    CL1                                                              
P1HOOK15 DS    CL1                                                              
P1DATE   DS    CL8                 ACTIVITY DATE                                
         DS    CL1                                                              
P1HOOK16 DS    CL1                                                              
         SPACE 1                                                                
         ORG   PLINED                                                           
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
*        ACREPWORKD                                                             
*        ACGENFILE                                                              
*        ACGENMODES                                                             
*        ACOFFALD                                                               
*        DDREPXTRAD                                                             
*        ACDDEQUS                                                               
*        DDCTRYEQUS                                                             
*        DDDLCB                                                                 
*        SEACSFILE                                                              
*        CTGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACOFFALD                                                       
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPTD02 11/07/14'                                      
         END                                                                    
