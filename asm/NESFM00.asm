*          DATA SET NESFM00    AT LEVEL 245 AS OF 05/13/20                      
*PHASE T31C00B                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T31C00 - NETWORK SUPER FILE MAINT CONTROLLER'                   
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-38512  02/26/20 FCONTROL SUPPORT                          *         
***********************************************************************         
T31C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T31C00,R7,RR=R2,CLEAR=Y                                  
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
**NOP    LR    RE,R8                                                            
**NOP    LA    RF,2000             CLEAR SELECTED STORAGE                       
**NOP    SLL   RF,3                                                             
**NOP    XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
*                                  EXTRACT TIOB VALUES                          
         L     RA,4(R1)                                                         
         ST    RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         LA    R9,IO                                                            
***      AH    R9,=H'6000'         GRABBING 3 2000 BYTE I/O AREAS               
***      LA    R9,24(R9)           NEED SPACE FOR 3 8BYTE LABELS                
         AHI   R9,LENIOAS                                                       
         USING SYSD,R9                                                          
         ST    R9,ASUBSYSD                                                      
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         BNZ   PRO1                THEN SKIP DDLINK STUFF                       
         L     R2,SYSPARMS         TEST IF CALLED FROM DDLINK UPLOAD            
         L     R2,16(R2)           R2=A(COMFACS)                                
         L     RF,CGLOBBER-COMFACSD(R2)                                         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,10,GLVDLUWF                             
         CLI   8(R1),0             WORKER FILE GLOBAL NOT PRESENT               
         BNE   PRO1                                                             
         L     RF,CGENNEW-COMFACSD(R2)                                          
         MVI   GENSTAT6,GES$UPLD   ENABLE DDLINK AUTO UPLOAD                    
         MVI   MODE,TESTGLOB                                                    
         GOTO1 (RF),DMCB,SPOOLD                                                 
         DC    H'0'                GENNEW SHOULD RETURN TO MONITOR              
*                                                                               
PRO1     DS    0H                                                               
         MVI   RACHANGE,C'N'                                                    
         TM    CONRECH+4,X'20'     TEST IF RECORD CHANGED                       
         BZ    *+12                YES                                          
         TM    CONACTH+4,X'20'     TEST IF ACTION CHANGED                       
         BO    *+8                 NO                                           
         MVI   RACHANGE,C'Y'       NOTE CHANGE IN EITHER                        
         SPACE 1                                                                
         EJECT                                                                  
         GOTO1 VTACC               TEST FOR TERMINAL ACCESS                     
INT01    BAS   RE,CALLGENC         OFF TO GENCON                                
         CLI   GOAGAIN,C'Y'        TEST FOR FLAG TO CALL GENCON AGAIN           
         BNE   INT02               NO                                           
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT                      
         B     INT01                                                            
         SPACE 1                                                                
INT02    OI    CONRECH+4,X'20'     SET PREV VALIDATED BITS                      
         OI    CONACTH+4,X'20'                                                  
         MVC   LASTOV,OVERLAY      SAVE THIS TIME OVERLAY                       
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         MVC   TERM,TWATRM         SAVE TERMINAL NUMBER                         
         OI    GENSTAT1,USKYMRG    SAVE ACTION FOR SWAPS                        
*                                                                               
         L     R4,SYSPARMS                                                      
         L     R2,0(R4)            GET A(TIOB)                                  
         LA    R2,0(R2)            CLEAR HIGH ORDER BYTE                        
         USING TIOBD,R2            TRANSLATOR I/O BLOCK                         
*                                  EXTRACT TIOB VALUES                          
         ZIC   RE,TIOBAID          GET PF KEY INPUT THIS TIME                   
         LA    RF,12                                                            
         CR    RE,RF                                                            
         BNH   *+6                 ADJUST FOR PF13-PF24                         
         SR    RE,RF                                                            
         STC   RE,PFAID                                                         
         CLI   PFAID,0                                                          
         BE    *+8                                                              
         OI    GENSTAT2,X'08'      SAVE ACTION FOR SWAPS                        
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
         DROP  R2                                                               
*                                                                               
         L     R2,8(R4)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RE,DMCB             PICK UP A(GENERAL CONTROLLER)                
         ST    RE,VGENCON1                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RE,DMCB             PICK UP A(DEMOCON)                           
         ST    RE,VDEMCON1                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A7A'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RE,DMCB             PICK UP A(STAPACK)                           
         ST    RE,VSTAPACK                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A1C'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RE,DMCB             PICK UP A(MSUNPK)                            
         ST    RE,VMSUNPK                                                       
*                                                                               
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
* SET SYSTEM DEPENDENT VALUES *                                                 
         SPACE 1                                                                
         MVI   SYSTEM,C'F'         FILE MAINT                                   
         MVI   MAXIOS,3            USES 3 I/O AREAS                             
         MVI   NTWA,NTWA06K+1                                                   
*        MVC   LSVTWA0,=X'0900'                                                 
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'20'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'     USUALLY NETFILE                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   SIZEIO,=AL4(LIOA)   EACH I/O IS 4000 BYTES                       
         MVC   LWORK,=AL4(LENWORK) WE TOOK 16000 BYTES IN NMOD                  
**       MVC   LWORK,=F'18400'     WE TOOK 18400 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9031C00'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R0,SVSTART          SET SAVED STORAGE START                      
         ST    R0,ASTARTSV                                                      
*                                                                               
         LHI   R1,(SVAREA-T31CFFD)+BASETWA2                                     
         A     R1,ATWA                                                          
         USING SVAREA,R1                                                        
*                                                                               
******   OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
******   BZ    *+12                                                             
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    *+12                                                             
         LA    RF,SVSECRET                                                      
         ST    RF,ASECBLK                                                       
         DROP  R1                                                               
*                                                                               
         L     R1,SYSPARMS         RF = A(TIOB)                                 
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SHI   R0,12                                                            
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
*                                                                               
         CLC   =C'NUN',CONREC      FOR NUN RECS                                 
         BE    SETSPOT                                                          
         CLC   =C'SGD',CONREC      FOR SGDEF RECS                               
         BE    SETSPOT                                                          
         CLC   =C'SGR',CONREC      FOR SGROUP RECS                              
         BE    SETSPOT                                                          
         CLC   =C'SGA',CONREC      FOR SGASSIGN RECORDS                         
         BE    SETSPOT                                                          
         CLC   =C'DUN',CONREC      FOR DUNIV RECS (ALTRNATE NAME)               
         BE    SETSPOT                                                          
         CLC   =C'PGE',CONREC      FOR PGEST RECS                               
         BE    SETSPOT                                                          
         CLC   =C'GFE',CONREC      FOR GFEST RECS                               
         BE    SETSPOT                                                          
         CLC   =C'PGR',CONREC      FOR PRODUCT GROUP RECORDS                    
         BE    SETSPOT                                                          
         CLC   =C'CGR',CONREC      FOR CLIENT GROUP RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'AOR',CONREC      FOR AOR RECS                                 
         BE    SETSPOT                                                          
         CLC   =C'SPB',CONREC      FOR SPB RECS                                 
         BE    SETSPOT                                                          
         CLC   =C'PGS',CONREC      FOR PGSPB RECS                               
         BE    SETSPOT                                                          
         CLC   =C'PRO',CONREC      FOR PROG, PROD RECS                          
         BE    SETSPOT                                                          
         CLC   =C'UNI',CONREC      FOR UNI RECS                                 
         BE    SETSPOT                                                          
         CLC   =C'NAD',CONREC      FOR NAD RECS                                 
         BE    SETSPOT                                                          
         CLC   =C'DEM',CONREC      FOR NAD RECS (ALTERNATE NAME)                
         BE    SETSPOT                                                          
         CLC   =C'NPR',CONREC      FOR NPROG RECS                               
         BE    SETSPOT                                                          
         CLC   =C'DPR',CONREC      FOR NPROG RECS (ALTRNATE NAME)               
         BE    SETSPOT                                                          
         CLC   =C'CLR',CONREC      FOR CLEARENCE RECS                           
         BE    SETSPOT                                                          
         CLC   =C'UDE',CONREC      FOR USER DEFINITION RECORDS                  
         BE    SETSPOT                                                          
         CLC   =C'CLI',CONREC      FOR CLI RECS                                 
         BE    SETSPOT                                                          
         CLC   =C'REA',CONREC      FOR REASON CODE RECORDS                      
         BE    SETSPOT                                                          
         CLC   =C'PRT',CONREC      FOR PROGRAM TYPE RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'DIS',CONREC      FOR DISTRIBUTION RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'BIL',CONREC      FOR BILL FORUMLA RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'EST',CONREC      FOR BILL FORUMLA RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'UCO',CONREC      FOR UCOMM RECORDS                            
         BE    SETSPOT                                                          
         CLC   =C'BUYG',CONREC      FOR BUYGRP RECORDS                          
         BE    SETSPOT                                                          
         CLC   =C'SUP',CONREC      FOR SUPV RECORDS                             
         BE    SETSPOT                                                          
         CLC   =C'BUYER',CONREC      FOR BUYER RECORDS                          
         BE    SETSPOT                                                          
         CLC   =C'SMAP',CONREC      FOR SECTIONAL MAPS                          
         BE    SETSPOT                                                          
         CLC   =C'CLCOPY',CONREC      FOR CLIENT COPY OV REPORT                 
         BE    SETSPOT                                                          
         CLC   =C'NNL',CONREC        DUNNING LETTERS                            
         BE    SETSPOT                                                          
         CLC   =C'NNT',CONREC        DUNNING LETTERS                            
         BE    SETSPOT                                                          
****     CLC   =C'BILLF',CONREC                                                 
****     BE    SETSPOT                                                          
         CLC   =C'BFORM',CONREC                                                 
         BE    SETSPOT                                                          
         CLC   =C'PXCREC',CONREC                                                
         BE    SETSPOT                                                          
         CLC   =C'LIMIT ',CONREC                                                
         BE    SETSPOT                                                          
         CLC   =C'I2COM',CONREC      I2 COMMENT RECORDS                         
         BE    SETXFIL                                                          
         CLC   =C'FLIGHT',CONREC     WB FLIGHT                                  
         BE    SETXFIL                                                          
         CLC   =C'AUTOPAY',CONREC    AUTOPAY                                    
         BE    SETXFIL                                                          
         CLC   =C'AGENCY',CONREC     AGENCY HEADER RECORD                       
         BE    SETSPOT                                                          
         CLC   =C'BHOLD',CONREC      BHOLD                                      
         BE    SETXFIL                                                          
         CLC   =C'BILLADR',CONREC    BILL ADDRESS RECORD                        
         BE    SETSPOT                                                          
         CLC   =C'CLFRZ',CONREC      CLIENT FREEZE                              
         BE    SETSPOT                                                          
*                                                                               
* comScore RECORDS                                                              
*                                                                               
         CLC   =C'COMDEF',CONREC    FOR COMDEF                                  
         BE    SETSPOT                                                          
         CLC   =C'CUN',CONREC       FOR CUNIV RECS                              
         BE    SETSPOT                                                          
         CLC   =C'CPR',CONREC       CPROG RECS                                  
         BE    SETSPOT                                                          
*                                                                               
*  THIS HAS TO BE LAST COMPARE HERE  SO IT CAN SET GENSTAT                      
*  ADDRESSIBLITY ISSUE CAUSED FORCED ME TO DO THIS   BPOO  11/2/99              
***      CLC   =C'PRG',CONREC      FOR PROGRAM GROUPS RECORDS                   
***      BNE   SYS6                                                             
***      OI    GENSTAT4,USEAFRCH                                                
*                                                                               
SETSPOT  MVC   LKEY,=H'13'         SET VALUES FOR SPTFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
*                                                                               
SYS6     CLC   =C'REP',CONREC      FOR REP RECS                                 
         BE    SYS7                                                             
         CLC   =C'MAS',CONREC      FOR MAS RECS                                 
         BE    SYS7                                                             
         CLC   =C'ADD',CONREC      FOR MAS RECS                                 
         BE    SYS7                                                             
         CLC   =C'MAR',CONREC      FOR MAR RECS                                 
         BNE   SYS8                                                             
SYS7     MVC   LKEY,=H'15'         SET VALUES FOR STAFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
*                                                                               
SYS8     CLC   =C'TSU',CONREC      TRAFFIC SUPPLIER RECORDS                     
         BNE   SYS11                                                            
*                                                                               
         MVC   LKEY,=H'20'         SET VALUES FOR UNTFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
*                                                                               
         MVI   USEIO,0                                                          
*        CLC   =C'DAYP',CONREC     DAYPART RECORDS?                             
*        BNE   *+8                                                              
*        MVI   USEIO,C'Y'                                                       
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
SYS11    LA    R1,SVSTART                                                       
         ST    R1,ASTARTSV                                                      
*                                                                               
SYS15    DS    0H                                                               
         B     XIT                                                              
*                                                                               
SETXFIL  MVC   LKEY,=H'32'         SET VALUES FOR XSPFILE                       
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
*                                                                               
         LA    R1,SVSTART                                                       
         ST    R1,ASTARTSV                                                      
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
* THIS ROUTINE CALLS GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON*            
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.   *            
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW RECORD*            
* AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION AND A  *            
* SELECTION WAS MADE.                                              *            
********************************************************************            
         SPACE 1                                                                
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
*                                  WE GET CONTROL BACK                          
         MVI   GOAGAIN,C'N'        DON'T GO AGAIN UNLESS OVERLAY WANTS          
         GOTO1 VGENCON1,DMCB,(R8)  TO CALL ANOTHER OVERLAY                      
         B     XIT                                                              
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE 1                                                                
         DS    0H                                                               
         USING *,RF                                                             
VCOMMON  NTR1  BASE=SYSRB                                                       
         DROP  RF                                                               
         LR    R7,RB                                                            
         AHI   R7,4096                                                          
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SRL   RF,24                                                            
         BAS   RE,VBRANCH(RF)                                                   
* NOTE NOT TO INSERT ANY RTNS HERE THAT CHANGE CONDITION CODE                   
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
VBRANCH  B     VUSER                                                            
         B     VFLD                                                             
         B     VMED                                                             
         B     VCLI                                                             
         B     VPROD                                                            
         B     VEST                                                             
         B     VNTWK                                                            
         B     VDPT                                                             
         B     VPKG                                                             
         B     VDTE                                                             
         B     VPRO                                                             
         B     VSPT                                                             
         B     VSTA                                                             
         B     VBFM                                                             
         B     GCLI                                                             
         B     VREP                                                             
         B     VMKT                                                             
         B     TRANSF                                                           
         B     CALL                                                             
         B     RETURN                                                           
         B     VGETTWA                                                          
         B     VTACC                                                            
         B     VGMSPACK                                                         
         B     VINITPFK                                                         
         B     VLACC                                                            
         B     VAUTH                                                            
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         LTORG                                                                  
         EJECT                                                                  
* READ AGENCY HEADER FOR USER NAME AND ADDRESS *                                
         SPACE 1                                                                
         PRINT GEN                                                              
VUSER    NTR1                                                                   
         PRINT NOGEN                                                            
         MVI   NFILE,C'S'                                                       
         MVI   USRIDFLG,0                                                       
*                                                                               
         L     R3,ATWA                                                          
         CLI   29(R3),0            TEST FIRST TIME                              
         BE    VUSER2              YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER10                                                          
*                                                                               
VUSER2   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
*                                                                               
         MVI   ERROR,INVMED                                                     
         BAS   RE,NREAD                                                         
         BNZ   TRAPERR                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BAS   RE,NGETREC                                                       
*                                                                               
         DROP  R4                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   VMED4                                                            
         USING AGYEL,R6                                                         
         MVC   USERNAME,AGYNAME                                                 
         MVC   USERADDR,AGYADDR                                                 
         MVC   USERPROF,AGYPROF                                                 
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         MVC   CTAGYSV,AGYCTAGY     COORDINATING AGENCY                         
         MVC   SVAGOF2,AGYOFC2      TWO CHARACTER OFFICE CODE REQUIRED          
         MVC   SVAGYFL1,AGYFLAG1    AGENCY FLAG                                 
         MVC   SVAGYFL2,AGYFLAG2    AGENCY FLAG                                 
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,3            ACC AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         USING AGYACCEL,R6                                                      
         BNE   VUSER10                                                          
         MVC   SVACCAGY,AGYACCAG   SAVE ACC AGENCY LIST                         
         DROP  R6                                                               
*                                                                               
*--CHECK SWAP CONDITIONS                                                        
VUSER10  DS    0H                                                               
         CLI   RACHANGE,C'Y'       TEST IF RECORD/ACTION CHANGED                
         BNE   VUSER12                                                          
         MVI   CALLSP,0            YES-CLEAR STACK POINTER                      
         SPACE 1                                                                
VUSER12  CLI   PFAID,PF12          TEST FOR PF12 (RETURN)                       
         BNE   VUSX                NO                                           
         CLI   CALLSP,0            TEST ANY THING ON STACK                      
         BE    VUSX                NO                                           
         SPACE 1                                                                
         LA    R0,RETURNS          R0=COUNTER                                   
         LA    RE,RETTAB                                                        
         CLC   TWASCR,0(RE)        TEST SCREEN AGAINST RETURN LIST              
         BE    VUSER15             RETURN IS SUPPORTED                          
         LA    RE,L'RETTAB(RE)                                                  
         BCT   R0,*-14                                                          
         B     VUSX                CANNOT RETURN FROM THIS SCREEN               
         SPACE 1                                                                
VUSER15  MVI   PFAID,0             CLEAR OUT PF12 SETTING                       
         OI    GENSTAT2,X'08'      SET GENCON SAVE SCREEN                       
         GOTO1 VRETURN                                                          
         SPACE 1                                                                
VUSX     DS    0H                                                               
         BAS   RE,RESET                                                         
         BAS   RE,INITSEC                                                       
         BRAS  RE,GETTOKEN         GET TOKEN RECORD FOR comScore                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
INITSEC  NTR1                                                                   
         LHI   RE,(SVAREA-T31CFFD)+BASETWA2                                     
         A     RE,ATWA                                                          
         USING SVAREA,RE                                                        
*                                                                               
*!!!     OC    ASECBLK,ASECBLK                                                  
*!!!     BZ    INITSCEX                                                         
*                                                                               
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    INITSCEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
*!!!     GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         GOTO1 (RF),DMCB,('SECPINIT',SVSECRET),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INITSCEX B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
************************************************************                    
* VALIFLD EXTRACTS DATA  FROM SCREEN FIELD                 *                    
*  NOPTFLG SET TO 1 IF INPUT IS OPTIONAL                   *                    
*                                                                               
*  OUTPUT                                                  *                    
*      NFLDH  CONTAINS FIELD HEADER                        *                    
*      NFLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED   *                    
*      R0     CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                 
*      R1     CONTAINS FIELD LENGTH                        *                    
*                                                          *                    
*      CONDITION CODE = 0 IF R1=0                          *                    
************************************************************                    
VFLD     DS    0H                                                               
         NTR1                                                                   
         MVC   NFLDH,0(R2)                                                      
         MVI   NFLD,C' '                                                        
         MVC   NFLD+1(L'NFLD-1),NFLD  FILL WITH SPACES                          
         SR    R0,R0              PRE-CLEAR REGISTER FOR NUMERIC VALUE          
         ZIC   R1,NFLDH                                                         
         AHI   R1,-9                                                            
*                                                                               
         TM    1(R2),X'02'        EXTENDED FIELD HEADER?                        
         BZ    *+8                                                              
         AHI   R1,-8                                                            
*                                                                               
         EX    R1,FLDMOVE                                                       
         LA    RE,NFLD(R1)          POINT RE AT LAST EXTRACTED BYTE             
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,NFLDH+5           SET ACTUAL DATA LENGTH                      
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
GETFLD4  LR    R3,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  R3,0                                                             
         MVC   WORK(6),=6X'F0'     TEST FOR NUMERIC FIELD                       
         EX    R3,MOVEZONE                                                      
         CLC   WORK(6),=6X'F0'                                                  
         BNE   GETFLDX                                                          
         EX    R3,FLDPACK                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         B     GETFLDX                                                          
*                                                                               
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     GETFLDX                                                          
*                                                                               
GTFERRCK CLI   NOPTFLG,1          IS THIS OK?                                   
         BE    GETFLDX                                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
GETFLDX  LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         SPACE 1                                                                
FLDMOVE  MVC   NFLD(0),8(R2)                                                    
FLDPACK  PACK  DUB,NFLD(0)                                                      
MOVEZONE MVZ   WORK(0),8(R2)                                                    
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
***************************************                                         
* VALIDATE MEDIA CODE                 *                                         
*    OUTPUT - BAGYMD,MEDNM,MEDCAPT    *                                         
***************************************                                         
         SPACE 1                                                                
VMED     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         BAS   RE,NREAD                                                         
         BNZ   VMED4                                                            
         L     R6,AIO                                                           
         CLI   BYTE,C'A'           USER SET THE IO AREA                         
         BE    *+12                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BAS   RE,NGETREC                                                       
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BNE   VMED4                                                            
         CLC   2(1,R6),8(R2)                                                    
         BNE   VMED2                                                            
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
         BAS   RE,RESET                                                         
         B     XIT                                                              
         SPACE 1                                                                
VMED4    MVI   ERROR,INVMED                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*********************************                                               
* GET CLIENT,                   *                                               
* VALIDATE CLIENT               *                                               
*   OUTPUT - QCLT,BCLT,CLTNM    *                                               
*            SVCLIST            *                                               
*********************************                                               
         SPACE 1                                                                
GCLI     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
*                                                                               
         L     RF,0(R1)                                                         
         MVC   QCLT(3),0(RF)                                                    
         B     CLT3                                                             
*                                                                               
VCLI     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
         L     R3,0(R1)            PRODUCT TABLE SIZE                           
*                                                                               
         MVI   ERROR,INVCLI                                                     
         CLI   5(R2),3                                                          
         BH    CLIERR                                                           
         CLI   5(R2),2                                                          
         BL    CLIERR                                                           
         BAS   RE,VFLD                                                          
         MVC   QCLT(3),NFLD                                                     
*                                                                               
CLT3     OI    QCLT+2,X'40'                                                     
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   CLIERR                                                           
         SPACE 1                                                                
* READ CLIENT HEADER *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         BAS   RE,NREAD                                                         
         BNZ   CLIERR                                                           
         L     R6,AIO                                                           
         CLI   BYTE,C'A'                                                        
         BE    *+12                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         BAS   RE,NGETREC                                                       
         SPACE 1                                                                
* SAVE CLIENT PRODUCT LIST *                                                    
         SPACE 1                                                                
         LA    RE,SVCLIST                                                       
         LA    RF,MAXPRDS                                                       
         C     R3,=F'500'                                                       
         BNE   *+8                                                              
         LA    RF,500                                                           
         SLL   RF,2                                                             
         XCEF                                                                   
*                                                                               
         LA    R4,SVCLIST                                                       
         LA    R5,MAXPRDS                                                       
         C     R3,=F'500'                                                       
         BNE   *+8                                                              
         LA    R5,500                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0DF1'                                                
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         BAS   RE,NHIGH             READ PASSIVE PROD POINTERS                  
         B     CLT6                                                             
CLT5     BAS   RE,NSEQ                                                          
CLT6     CLC   KEY(5),KEYSAVE                                                   
         BNE   CLT10                                                            
         C     R3,=F'500'           PASS MAXIMUM PRODUCTS                       
         BE    *+12                                                             
         CLI   KEY+10,0             CHECK OVERFLOW PRODUCT                      
         BE    CLT5                 BYPASS                                      
         MVC   0(3,R4),KEY+6        MOVE ALPHA PRODUCT                          
         MVC   3(1,R4),KEY+10        IF NOT OVERFLOW MOVE PROD NUMBER           
         LA    R4,4(R4)                                                         
         BCT   R5,CLT5                                                          
* REPOSITION THE POINTER                                                        
CLT10    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         BAS   RE,NREAD                                                         
         B     CLT12                                                            
***      LA    R4,CLIST                                                         
***      LA    R5,880                                                           
***      LA    RE,SVCLIST                                                       
***      LA    RF,880                                                           
***      MVCL  RE,R4                                                            
*                                                                               
***      LA    R4,CLIST2                                                        
***      LA    R5,140                                                           
***      LA    RE,SVCLIST2                                                      
***      LA    RF,140                                                           
***      MVCL  RE,R4                                                            
*                                                                               
CLT12    MVC   BOFFICE,COFFICE     OFFICE NUMBER                                
*                                                                               
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
*                                                                               
         MVC   BCLIAAN,CPROF+6     AAN PROFILE                                  
         SPACE 1                                                                
* SAVE USER DEFINITION LIST                                                     
         XC    SVP1USER(SVULNQ),SVP1USER                                        
         MVC   SVP1USER(SVULNQ),CPU1  MOVE ALL INFO TO SAVED STORAGE            
*                                                                               
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
*                                                                               
*!!!     BAS   RE,VLACC                                                         
         GOTO1 VLACC,DMCB,BAGYMD,BCLT                                           
         CLI   DMCB+4,X'FF'                                                     
         BE    CLIERR                                                           
*                                                                               
*&&DO                                                                           
         MVI   ERROR,SECLOCK                                                    
         OC    T31CFFD+6(2),T31CFFD+6  TEST ANY SECURITY LIMIT                  
         BZ    CLTX                                                             
         CLI   T31CFFD+6,C'+'          TEST FOR MARKET LOCKOUT                  
         BE    CLTX                                                             
         CLI   T31CFFD+6,C'$'          TEST OFFICE LIMITS                       
         BE    CLT15                                                            
         CLI   T31CFFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    CLT20                                                            
         CLC   T31CFFD+6(2),BCLT       ELSE SINGLE CLIENT ACCESS                
         BNE   CLIERR                                                           
         B     CLTX                                                             
*                                                                               
CLT15    DS    0H               * TEST OFFICE LIST SECURITY *                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                         
         CLI   0(R1),0                                                          
         BNE   CLIERR                                                           
         B     CLTX                                                             
*                                                                               
CLT20    CLC   T31CFFD+7(1),COFFICE   MATCH OFFICE CODE                         
         BNE   CLIERR                                                           
*&&                                                                             
CLTX     BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R6                                                               
         SPACE                                                                  
*                                                                               
CLIERR   B     TRAPERR                                                          
MAXPRDS  EQU   252                                                              
         EJECT                                                                  
***************************                                                     
* VALIDATE PRD            *                                                     
*    OUTPUT - QPRD,BPRD   *                                                     
***************************                                                     
         SPACE                                                                  
VPROD    NTR1                                                                   
         MVI   NFILE,C'S'                                                       
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
*                                                                               
         MVI   ERROR,INVPROD                                                    
         CLI   ACTNUM,ACTCHA                     CHECK FOR CHANGE               
         BE    *+14                                                             
         CLC   =C'AAA',12(R4)                                                   
         BE    TRAPERR                                                          
         CLI   0(R4),2                                                          
         BL    TRAPERR                                                          
         CLI   0(R4),3                                                          
         BH    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),12(R4)                                                  
         BAS   RE,NREAD                                                         
         BNZ   TRAPERR                                                          
         CLI   BYTE,C'A'                                                        
         BE    *+10                                                             
         MVC   AIO,AIO1                                                         
         BAS   RE,NGETREC                                                       
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   QPRD,12(R4)      RETURN EBCDIC PRD CODE                          
         MVC   BPRD,PCODE+1      AND BINARY PRD CODE                            
         BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*******************************                                                 
* VALIDATE ESTIMATE           *                                                 
*   OUTPUT QEST,BEST,ESTNAME  *                                                 
*******************************                                                 
         SPACE                                                                  
VEST     NTR1                                                                   
         MVI   ERROR,INVEST                                                     
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD,DMCB,255                                                 
         LTR   R0,R0               R0=0 IF NOT NUMERIC                          
         BZ    TRAPERR                                                          
         STC   R0,BEST                                                          
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         BAS   RE,NREAD                                                         
         BNZ   TRAPERR                                                          
         CLI   BYTE,C'A'                                                        
         BE    *+10                                                             
         MVC   AIO,AIO1                                                         
         BAS   RE,NGETREC                                                       
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         MVC   ESTNAME,EDESC                                                    
         MVC   QEST,NFLD                                                        
         BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
************************************                                            
* VALIDATE NETWORK                 *                                            
*   OUTPUT - QNET(CL4),QNETMKT(BL2)*                                            
*   OUTPUT - BMKTSTA(5)            *                                            
***********************************                                             
         SPACE                                                                  
VNTWK    NTR1                                                                   
         BAS   RE,VFLD                                                          
         MVI   NFILE,C'T'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NFLD    ASSUMES AFTER VFLD                           
         OC    STAKCALL(4),=XL4'40404040'                                       
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,=CL3'000'   FILL WITH CHAR ZERO                          
         MVC   STAKFILL,=CL5'00000'                                             
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                                                             
         BE    VNTWKOK                                                          
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VNTWKOK  DS    0H                                                               
         MVC   QNET,NFLD                                                        
         LA    R4,IO              GET STATION MARKET NUMBER                     
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,QNETMKT                                                       
         MVC   QPTYPE,SPTYPE       SAVE POSTING TYPE                            
         MVC   QSTYPE,STYPE        SAVE NETWORK TYPE                            
         MVC   QNTISTA,SNTISTA     SAVE NTI STATION                             
*                                                                               
         CLI   SPTYPE,C'C'                                                      
         BE    *+12                                                             
         CLI   SPTYPE,C'O'                                                      
         BNE   *+14                                                             
         MVC   PRECTAB(14),COPREC                                               
         B     *+10                                                             
         MVC   PRECTAB(14),NSPREC                                               
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         BAS   RE,RESET                                                         
         B     XIT                                                              
*                                                                               
*--DEMO OVERRIDE PRECISION TABLES                                               
COPREC   DC    C'R',X'82',C'S',X'81',C'P',X'82',C'T',X'42',C'H',X'42'           
         DC    C'U',X'43',C'V',X'40'                                            
NSPREC   DC    C'R',X'81',C'S',X'81',C'P',X'81',C'T',X'43',C'H',X'43'           
         DC    C'U',X'43',C'V',X'40'                                            
         DROP  R4                                                               
         EJECT                                                                  
**********************************                                              
* VALIDATE A DAYPART             *                                              
*    OUTPUT - DPTNAME,QDPT,QDPT2 *                                              
**********************************                                              
VDPT     NTR1                                                                   
         GOTO1 =A(VALDPT),RR=RELO                                               
         B     XIT                                                              
*&&DO                                                                           
         DS    0H                                                               
         LA    R3,DAYPARTS                                                      
VDPT5    CLC   8(1,R2),0(R3)                                                    
         BE    DPTOK                                                            
         LA    R3,9(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   VDPT5                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
DPTOK    MVC   DPTNAME,1(R3)                                                    
         MVC   QDPT,8(R2)                                                       
         B     XIT                                                              
*&&                                                                             
         SPACE                                                                  
         EJECT                                                                  
****************************                                                    
* VALIDATE PACKAGE         *                                                    
*   OUTPUT - PAKGNAM,BPAKG *                                                    
****************************                                                    
         SPACE                                                                  
VPKG     NTR1                                                                   
         MVI   ERROR,NOTFOUND                                                   
         BAS   RE,VFLD                                                          
         STC   R0,BPAKG                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPKEY,R4                                                         
         MVI   KEY,X'02'                                                        
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,QNET                                                      
         MVC   NPKEST,BEST                                                      
         MVC   NPKPACK,BPAKG                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING NPAKEL,R6                                                        
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   PAKGNAM,NPAKNAME                                                 
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***************************************************                             
* DATE VALIDATION                                                               
*      OUTPUT: YYMMDD IN QDATE                                                  
*                                                                               
VDTE     NTR1                                                                   
         BAS   RE,VFLD                                                          
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(0,NFLD),QDATE                                       
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         B     XIT                                                              
         EJECT                                                                  
**********************************************************                      
* READ PROGRAM RECORD TO VALIDATE PROGRAM EXCEPTION NAME *                      
*      EXPECTS R2 TO POINT TO PROG CODE HEADER           *                      
**********************************************************                      
VPRO     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
*                                                                               
         MVI   ERROR,INVALID                                                    
*--MAKE SURE CODE IS ALPHA-NUMERIC                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   VPRO30                                                           
*--BECAUSE OF THE CABLE UPLOAD NUMERICS ARE NO LONGER ALLOWED                   
*--IN THE THIRD AND FOURTH POSITION OF THE PROGRAM LITERAL.                     
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   8(3,R2),=CL3'ALL'   ALL NOT ALLOWED                              
         BE    TRAPERR                                                          
         TM    10(R2),X'F0'        TEST FOR NUMERIC IN THE THIRD SPOT           
         BO    TRAPERR                                                          
         TM    11(R2),X'F0'        TEST FOR NUMERIC IN THE FOURTH SPOT          
         BO    TRAPERR                                                          
*                                                                               
         LR    R3,R2                                                            
         LA    R3,8(R3)                                                         
         ZIC   R4,5(R2)                                                         
VPRO10   CLI   0(R3),C'&&'                                                      
         BE    VPRO20                                                           
*        CLI   0(R3),C'/'                                                       
*        BE    VPRO20                                                           
*        CLI   0(R3),C'.'                                                       
*        BE    VPRO20                                                           
*        CLI   0(R3),C'-'                                                       
*        BE    VPRO20                                                           
         CLI   0(R3),C'?'                                                       
         BE    VPRO20                                                           
*        CLI   0(R3),C':'                                                       
*        BE    VPRO20                                                           
         CLI   0(R3),C''''                                                      
         BE    VPRO20                                                           
         CLI   0(R3),X'C1'                                                      
         BL    TRAPERR                                                          
         CLI   0(R3),X'F9'                                                      
         BH    TRAPERR                                                          
VPRO20   LA    R3,1(R3)                                                         
         BCT   R4,VPRO10                                                        
*                                                                               
VPRO30   CLI   ACTNUM,ACTADD                                                    
         BE    XIT                                                              
*                                                                               
         CLI   SOFTCHK,1           CHECK TO BYPASS READ VALIDATION              
         BE    XIT                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),QNETMKT                                                 
         MVC   KEY+5(6),8(R2)       PROG CODE                                   
         OC    KEY+5(6),=6X'40'                                                 
         BAS   RE,NREAD                                                         
         CLC   KEY(11),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         MVC   AIO,AIO1                                                         
         BAS   RE,NGETREC                                                       
         B     XIT                                                              
         EJECT                                                                  
******************************************                                      
* SETS TO READ SPOT FILE                                                        
*                                                                               
******************************************                                      
VSPT     NTR1                                                                   
         MVC   LKEY,=H'13'         SET VALUES FOR SPTFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 3                                                                
*********************************************                                   
* SETS TO READ STATION FILE                                                     
*                                                                               
**********************************************                                  
VSTA     NTR1                                                                   
         MVC   LKEY,=H'15'         SET VALUES FOR STAFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
***************************************                                         
* VALIDATE BILL FORMULA               *                                         
*   OUTPUT - WORK(7)                  *                                         
*   INPUT  - R2 PTS TO THE BILL BASIS *                                         
*   INPUT  - P1 E=EST,P=PROD          *                                         
***************************************                                         
         SPACE 1                                                                
VBFM     NTR1                                                                   
         XC    WORK(7),WORK                                                     
         MVC   WORK+7(1),DMCB      RECORD TYPE                                  
*--CHECK BILL BASIS                                                             
         CLI   8(R2),C'G'          SEE IF GROSS                                 
         BNE   VBFM020                                                          
         MVC   8(5,R2),=C'GROSS'                                                
         OI    6(R2),X'80'                                                      
         B     VBFM030                                                          
VBFM020  MVI   ERROR,INVBAS                                                     
         CLI   8(R2),C'N'          SEE IF NET                                   
         BNE   TRAPERR                                                          
         XC    8(5,R2),8(R2)                                                    
         MVC   8(3,R2),=C'NET'                                                  
         OI    6(R2),X'80'                                                      
         OI    WORK+2,X'10'                                                     
*--CHECK COMMISSION PERCENT                                                     
VBFM030  SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         MVI   ERROR,INVBAS                                                     
         LA    R9,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,(R9))                                            
         CLI   DMCB,X'FF'                                                       
         BE    TRAPERR                                                          
         MVC   WORK+3(4),DMCB+4                                                 
*--CHECK COMM BASIS                                                             
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         CLI   8(R2),C'G'          SEE IF GROSS                                 
         BNE   VBFM080                                                          
         MVC   8(5,R2),=C'GROSS'                                                
         OI    6(R2),X'80'                                                      
         B     VBFM100                                                          
VBFM080  MVI   ERROR,INVBAS                                                     
         CLI   8(R2),C'N'          SEE IF NET                                   
         BNE   TRAPERR                                                          
         XC    8(5,R2),8(R2)                                                    
         MVC   8(3,R2),=C'NET'                                                  
         OI    6(R2),X'80'                                                      
         OI    WORK+2,X'01'                                                     
*--CHECK EFFECTIVE DATE                                                         
VBFM100  CLI   WORK+7,C'E'                                                      
         BE    VBFMEX                                                           
         CLI   WORK+7,C'P'                                                      
         BE    *+6                                                              
         DS    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         LA    R9,8(R2)                                                         
         GOTO1 DATVAL,DMCB,(2,(R9)),QDATE                                       
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
*                                                                               
         MVC   FULL,DMCB+4                                                      
         GOTO1 DATCON,DMCB,(0,FULL),(2,WORK)                                    
*                                                                               
VBFMEX   B     XIT                                                              
         EJECT                                                                  
************************************                                            
* VALIDATE REP                     *                                            
************************************                                            
         SPACE                                                                  
VREP     NTR1                                                                   
         BAS   RE,VFLD                                                          
         BNZ   VREP10                                                           
         MVC   8(3,R2),=CL3'000'   DEFAULT VALUE                                
         B     VREPREK                                                          
VREP10   CLC   NFLD(3),=CL3'000'   DEFAULT VALUE                                
         BE    VREPREK                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPREC,R4                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,NFLD        ASSUMES AFTER VFLD                           
         MVC   REPKAGY,AGENCY                                                   
         MVC   REPKFILL,=CL10'0000000000'                                       
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                                                             
         BE    VREPREK                                                          
         MVI   ERROR,INVREP                                                     
         B     TRAPERR                                                          
VREPREK  BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
************************************                                            
* VALIDATE MARKET                  *                                            
************************************                                            
         SPACE                                                                  
VMKT     NTR1                                                                   
         BAS   RE,VFLD                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTREC,R4                                                        
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'N'                                                     
         MVC   MKTKMKT,NFLD        ASSUMES AFTER VFLD                           
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,=CL9'000000000'                                         
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                                                             
         BE    VMKTREK                                                          
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
VMKTREK  BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*****************************************************************               
***********************************************************************         
* THIS ROUTINE FACILITATES THE ACTION OF SWITCHING, WITHIN A SINGLE   *         
* TRANSACTION, BETWEEN ONE OVERLAY AND ANOTHER BY CHANGING THE RECORD,*         
* ACTION, AND KEY FIELDS ON THE SCREEN AND CALLING GENCON AGAIN. FIRST*         
* IT SAVES THE CURRENT TWA IN TEMPSTR AND PUSHES THE CURRENT OVERLAY  *         
* NUMBER ONTO A STACK.  THEN IT CHANGES THE RECORD, ACTION AND KEY    *         
* FIELDS TO THE DATA SPECIFIED IN THE PARAMETER LIST. FINALLY, IT SETS*         
* THE FLAG 'GOAGAIN' TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON  *         
* WHICH RETURNS BACK TO THE CONTROLLER. THE CONTROLLER THEN RECOGNIZES*         
* THE FLAG AND CALLS GENCON AGAIN. THE PARAMTER LIST LOOKS AS FOLLOWS:*         
*                                                                     *         
*       RECORD,ACTION,KEY1,KEY2,KEY3,KEY4,...,0                       *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
TRANSF   DS    0H                                                               
         LR    R4,R1               ENTRY POINT FOR VTRANSF                      
         B     CALL6                                                            
*                                                                               
CALL     LR    R4,R1               SAVE POINTER TO PARMS                        
         ZIC   R3,CALLSP           GET STACK POINTER                            
         LR    R2,R3               R2=ORIGINAL STACK POINTER VALUE              
         LA    RF,CALLSTK(R3)      RF=A(NEXT POSITION)                          
         MVC   0(1,RF),OVERLAY     SLOT IN OVERLAY NUMBER                       
         MVC   1(1,RF),TWALACT     STORE ACTION NUMBER                          
         MVC   2(1,RF),TWALREC     STORE RECORD NUMBER                          
*                                                                               
         LA    R3,3(R3)                                                         
         STC   R3,CALLSP                                                        
         CLI   CALLSP,L'CALLSTK    TEST MORE THAN ALLOWED NEST LEVELS           
         BNH   CALL2                                                            
         LA    R2,CONRECH          RETURN CURSOR TO RECORD FIELD                
         MVI   ERROR,BADNEST                                                    
         B     TRAPERR                                                          
*                                                                               
CALL2    SRL   R2,1                DIVIDE ORIGINAL LEVEL BY TWO                 
         LA    R2,3(R2)            ADD BACK THREE TWA PAGES                     
         SLL   R2,32-8             MOVE TO HIGH ORDER BYTE                      
         ICM   R2,3,TERM                                                        
         PRINT GEN                                                              
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         PRINT NOGEN                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CALL4    STC   R3,BYTE             SAVE STACK LEVEL                             
         LA    R1,SAVAREA-CONRECH  MOVE RECORD HEADER UNTIL TWA0 SAVE           
         L     RE,ATIA             RE=DESTINATION                               
         LA    RF,3072             MOVE/CLEAR HALF A TWA                        
         LA    R0,CONRECH          START AT RECORD HEADER                       
         TM    BYTE,X'01'          TEST FOR ODD NUMBER                          
         BO    *+6                 YES                                          
         AR    RE,RF               NO-MOVE TO SECOND HALF OF TWA                
         MVCL  RE,R0                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R2),ATIA,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CALL6    XC    CONREC,CONREC       SET NEW RECORD TYPE                          
         OI    CONRECH+6,X'80'                                                  
         OI    CONRECH+4,X'80'     INPUT THIS TIME                              
         NI    CONRECH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONRECH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONREC(0),0(RF)                                                  
*                                                                               
CALL8    LA    R4,4(R4)            BUMP TO SECOND PARM                          
         XC    CONACT,CONACT       SET NEW ACTION TYPE                          
         OI    CONACTH+6,X'80'                                                  
         NI    CONACTH+4,X'DF'                                                  
         OI    CONACTH+4,X'80'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONACT(0),0(RF)                                                  
*                                                                               
CALL10   LA    R4,4(R4)            BUMP TO THIRD PARM                           
         XC    CONKEY,CONKEY       SET NEW KEY FIELDS                           
         OI    CONKEYH+6,X'80'                                                  
         NI    CONKEYH+4,X'DF'                                                  
         OI    CONKEYH+4,X'80'                                                  
         LA    R2,BLOCK            BUILD KEY FIELD FROM THIRD PARM ON           
         LR    R3,R2                                                            
*                                                                               
CALL12   L     RF,0(R4)            ADD PARM TO KEY FIELD                        
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       EXTRACT KEY FIELD                            
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),SPACES      SPACE PAD THE FIELD                          
*                                                                               
         LA    R2,0(RE,R2)         R2=A(LAST BYTE IN PARM)                      
         LA    RE,1(RE)            RESTORE PARM LENGTH                          
         CLI   0(R2),C' '          TEST FOR SIGNIFICANT CHARACTER               
         BH    CALL14              FOUND ONE                                    
         BCTR  R2,0                BACK UP TO PREVIOUS BYTE                     
         BCT   RE,*-10                                                          
*                                                                               
         LA    R2,1(R2)            ADVANCE TO FIRST POSITION                    
         MVI   0(R2),C','          INSERT POSITIONAL COMMA                      
*                                                                               
CALL14   LA    R4,4(R4)            BUMP TO NEXT PARM                            
         OC    0(4,R4),0(R4)       TEST FOR END OF PARM LIST                    
         BZ    CALL15              YES                                          
*                                                                               
         CLI   0(R2),C','          TEST LAST PARM ENDED WITH COMMA              
         BE    *+12                                                             
         LA    R2,1(R2)            NO-SO ADD ONE AFTER PARM                     
         MVI   0(R2),C','                                                       
*                                                                               
         LA    R2,1(R2)                                                         
         B     CALL12                                                           
*                                                                               
CALL15   LA    R2,1(R2)                                                         
         SR    R2,R3               COMPUTE LENGTH OF STRING                     
         LA    R0,L'CONKEY                                                      
         CR    R2,R0                                                            
         BL    *+6                                                              
         LR    R2,R0               DO NOT MOVE MORE THAN L'CONKEY               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CONKEY(0),BLOCK                                                  
         LA    R2,1(R2)            RESTORE LENGTH                               
         STC   R2,CONKEYH+5                                                     
*                                                                               
CALLX    MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         MVC   CALLER,OVERLAY      LET OVERLAY KNOW WHO CALLED IT               
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
VGMSPACK NTR1                                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
         LA    R4,WORK                                                          
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
*        MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* PFKEY INITIALIZATION                                                          
*                                                                               
* ON ENTRY:    PARAM 1             A(PFKEY VAL. TABLE) OR ZEROS                 
**********************************************************************          
VINITPFK DS    0H                                                               
         ICM   R3,7,1(R1)                                                       
         GOTO1 =A(PFKYINIT),DMCB,(RC),(R3),RR=RELO                              
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER/SCREEN TO         *          
* THE VALUE ON TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN' *          
* FLAG TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN        *          
* THE CONTROLLER REGAINS CONTROL, IT CALLS GENCON AGAIN WITH THE     *          
* RESTORED SCREEN.                                                   *          
**********************************************************************          
         SPACE 3                                                                
RETURN   DS    0H                                                               
         ZIC   R3,CALLSP           GET STACK POINTER                            
         AHI   R3,-3               DECREMENT POINTER TO POP STACK               
         STC   R3,CALLSP                                                        
         LA    RF,CALLSTK(R3)                                                   
         MVC   RETURNED,LASTOV     NOTE OVERLAY RETURNING FROM                  
         MVC   OVERLAY,0(RF)       EXTRACT NEW OVERLAY NUMBER                   
         MVC   ACTNUM,1(RF)                                                     
         MVC   RECNUM,2(RF)                                                     
*                                                                               
RETURN2  LR    R2,R3                                                            
         SRL   R2,1                DIVIDE LEVEL BY TWO                          
         LA    R2,3(R2)            START AT TWA PAGE 3                          
         SLL   R2,32-8             MOVE PAGE TO HOB                             
         ICM   R2,3,TERM                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RETURN4  DS    0H                                                               
         LA    RF,SAVAREA-CONRECH  MOVE RECORD HEADER UP TO SAVE AREA           
         LA    RE,CONRECH                                                       
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         LA    R3,1(R3)            RESTORE ORIGINAL LEVEL                       
         STC   R3,BYTE                                                          
         TM    BYTE,X'01'          TEST FOR ODD LEVEL                           
         BO    *+8                 YES                                          
         A     R0,=F'3072'         NO-MUST BE IN SECOND HALF OF PAGE            
         MVCL  RE,R0                                                            
*                                                                               
RETURN6  LA    R2,CONRECH                                                       
         PRINT GEN                                                              
         ZIC   RF,0(R2)                                                         
         PRINT NOGEN                                                            
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-12                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
         LA    RE,OVSCRTAB         RE=A(OVERLAY/SCREEN TABLE)                   
*                                                                               
RETURN7  CLC   OVERLAY,0(RE)                                                    
         BE    RETURN8                                                          
         LA    RE,L'OVSCRTAB(RE)                                                
         CLI   0(RE),X'FF'         TEST FOR EOT                                 
         BNE   RETURN7                                                          
         DC    H'0'                                                             
*                                                                               
RETURN8  MVC   TWASCR,1(RE)        SET SCREEN NUMBER RESTORED                   
         CLC   =C'SEL',CONACT                                                   
         BNE   *+8                                                              
         MVI   GOAGAIN,C'Y'        SET TO CALL GENCON AGAIN                     
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
OVSCRTAB DS    0CL2                X'EDIT OVERLAY'  X'SCREEN OVERLAY'           
         SPACE 1                                                                
         DC    X'13',X'F7'         PROGRAM DISPLAY                              
         DC    X'14',X'F9'         UNIVERSE DISPLAY                             
         DC    X'18',X'E6'         NAD PROGRAM DISPLAY                          
         DC    X'19',X'E8'         NAD UNIVERSE DISPLAY                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO READ/WRITE TEMPSTR PAGES                              
         SPACE 2                                                                
*                                  P1, BYTE  0=BIT SETTINGS/PAGE NUMBER         
*                                  P1, BYTES 1-3=READ/WRITE ADDRESS             
VGETTWA  DS    0H                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
         SPACE 1                                                                
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA2                                                            
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA2                                                            
         MVC   COMMAND(6),=C'DMREAD'                                            
         SPACE 1                                                                
GTWA2    NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
         SPACE 1                                                                
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
         SPACE 1                                                                
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* CHECK TERMINAL ACCESS FOR CLIENT AND PRODUCT                                  
*   SHOULD USE A TABLE IF MORE ARE NEEDED                                       
* THIS IS CALLED THE FIRST TIME BASE GETS CONTROL                               
* THIS IS CALLED AGAIN BY THE OVERLAY IN CASE USER IS TRYING TO                 
*   CHA/DEL IN A LIST                                                           
**********************************************************************          
VTACC    NTR1                                                                   
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   XIT                                                              
         GOTO1 =A(VTACCRT),RR=RELO                                              
         B     XIT                                                              
**********************************************************************          
* CHECK LIMITED ACCESS SECURITY                                                 
*        INPUT:                                                                 
*        PARAM 1 - AGENCY/MEDIA                                                 
*        PARAM 2 - CLIENT                                                       
**********************************************************************          
VLACC    NTR1                                                                   
         L     RF,0(R1)            AGENCY/MEDIA                                 
         MVC   TMPAM,0(RF)                                                      
         L     RF,4(R1)            CLIENT                                       
         MVC   TMPCLT,0(RF)                                                     
*                                                                               
         GOTO1 =A(VLACCRT),RR=RELO                                              
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AUTHORIZATION NUMBER                                                 
*                                                                               
* ON ENTRY:    PARAM 1             A(SECURITY TABLE)   OR ZEROS                 
*              SECURITY AGENCY(2),AUTHORIZATION #(2),PERSONAL ID(8)             
***********************************************************************         
VAUTH    DS    0H                                                               
         ICM   R3,7,1(R1)                                                       
         GOTO1 =A(AUTH),DMCB,(RC),(R3),RR=RELO                                  
         B     XIT                                                              
         EJECT                                                                  
****************************************************                            
* RESET TO READ UNIT FILE                          *                            
* CALLED FROM VALIDATION RTNS THAT READ SPOT/STAT FILE                          
* MAKE SURE YOU DO NOTHING TO RESET CONDITION CODE -(WHY? GEP, 5/22/86)         
****************************************************                            
RESET    DS    0H                  DETAILS OF DIRECTORY AND KEY                 
         CLC   =C'AOR',CONREC      FOR AOR RECORDS  (**NB- RESETS CC)           
         BER   RE                  SKIP RESET                                   
         CLC   =C'SPB',CONREC      FOR SPB REC (SPLIT BILLING)                  
         BER   RE                                                               
         CLC   =C'PGS',CONREC      FOR PGSPB REC (SPLIT BILLING)                
         BER   RE                                                               
         CLC   =C'REP',CONREC      FOR REP RECS                                 
         BER   RE                                                               
         CLC   =C'NUN',CONREC      FOR NUN RECS                                 
         BER   RE                                                               
         CLC   =C'DUN',CONREC      FOR NUNIV RECS (ALTERNATE NAME)              
         BER   RE                                                               
         CLC   =C'PGE',CONREC      FOR PGEST RECS                               
         BER   RE                                                               
         CLC   =C'GFE',CONREC      FOR GFEST RECS                               
         BER   RE                                                               
         CLC   =C'PGR',CONREC      FOR PRODUCT GROUP RECORDS                    
         BER   RE                                                               
         CLC   =C'CGR',CONREC      FOR CLIENT GROUP RECORDS                     
         BER   RE                                                               
         CLC   =C'PRO',CONREC      FOR PRO,PROG RECS                            
         BER   RE                                                               
         CLC   =C'UNI',CONREC      FOR UNI RECS                                 
         BER   RE                                                               
         CLC   =C'CLI',CONREC      FOR CLI RECS                                 
         BER   RE                                                               
         CLC   =C'MAR',CONREC      FOR MAR RECS                                 
         BER   RE                                                               
         CLC   =C'SGD',CONREC      FOR SGDEF RECS                               
         BER   RE                                                               
         CLC   =C'SGR',CONREC      FOR SGROUP RECS                              
         BER   RE                                                               
         CLC   =C'SGA',CONREC      FOR SGASSIGN RECS                            
         BER   RE                                                               
         CLC   =C'MAS',CONREC      FOR MAS RECS                                 
         BER   RE                                                               
         CLC   =C'ADD',CONREC      FOR ADD RECS                                 
         BER   RE                                                               
         CLC   =C'NAD',CONREC      FOR NAD RECS                                 
         BER   RE                                                               
         CLC   =C'DEM',CONREC      FOR NAD RECS (ALTERNATE NAME)                
         BER   RE                                                               
         CLC   =C'NPR',CONREC      FOR NPROG RECS                               
         BER   RE                                                               
         CLC   =C'DPR',CONREC      FOR NPROG RECS (ALTERNATE NAME)              
         BER   RE                                                               
         CLC   =C'UDE',CONREC      FOR USER DEFINITION RECORDS                  
         BER   RE                                                               
         CLC   =C'CLR',CONREC      FOR CLEARENCE RECS                           
         BER   RE                                                               
         CLC   =C'REA',CONREC      FOR REASON CODE RECORDS                      
         BER   RE                                                               
         CLC   =C'PRT',CONREC      FOR PROGRAM TYPE RECORDS                     
         BER   RE                                                               
         CLC   =C'DIS',CONREC      FOR DISTRIBUTION RECORDS                     
         BER   RE                                                               
         CLC   =C'BIL',CONREC      FOR BILL FORMULA RECORDS                     
         BER   RE                                                               
         CLC   =C'PRG',CONREC      FOR PROPGRAM GROUP RECORDS                   
         BER   RE                                                               
         CLC   =C'EST',CONREC      FOR ESTIMATE RECORDS                         
         BER   RE                                                               
         CLC   =C'UCO',CONREC      FOR UCOMM RECORDS                            
         BER   RE                                                               
         CLC   =C'BUYG',CONREC      FOR BUYGRP RECORDS                          
         BER   RE                                                               
         CLC   =C'SUP',CONREC      FOR SUPV RECORDS                             
         BER   RE                                                               
         CLC   =C'BUYER',CONREC      FOR BUYER RECORDS                          
         BER   RE                                                               
         CLC   =C'SMAP',CONREC      FOR SECTIONAL MAPS                          
         BER   RE                                                               
         CLC   =C'CLCOPY',CONREC      FOR CLIENT COPY OV REPORT                 
         BER   RE                                                               
         CLC   =C'NNL',CONREC         FOR DUNNING LETTERS                       
         BER   RE                                                               
         CLC   =C'NNT',CONREC         FOR DUNNING LETTERS                       
         BER   RE                                                               
         CLC   =C'LIMIT',CONREC                                                 
         BER   RE                                                               
****     CLC   =C'BILLF',CONREC                                                 
****     BER   RE                                                               
         CLC   =C'BFORM',CONREC                                                 
         BER   RE                                                               
         CLC   =C'AGENCY',CONREC                                                
         BER   RE                                                               
         CLC   =C'PXCREC',CONREC                                                
         BER   RE                                                               
         CLC   =C'COMDEF',CONREC     FOR COMDEF RECS                            
         BER   RE                                                               
         CLC   =C'CUN',CONREC        FOR CUNIV RECS                             
         BER   RE                                                               
         CLC   =C'CPR',CONREC        FOR CPROG RECS                             
         BER   RE                                                               
         CLC   =C'CLFRZ',CONREC      FOR CLIENT FREEZE OV REPORT                
         BER   RE                                                               
         CLC   =C'I2COM',CONREC      FOR I2COM RECORDS                          
         BE    RE05                                                             
         CLC   =C'BHOLD',CONREC      FOR BHOLD RECORDS                          
         BE    RE05                                                             
         CLC   =C'AUTOPAY',CONREC                                               
         BE    RE05                                                             
         CLC   =C'FLIGHT',CONREC                                                
         BE    RE05                                                             
         B     RE10                                                             
*                                                                               
RE05     MVC   LKEY,=H'32'         SET VALUES FOR XSPFILE                       
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL'                                                
         MVC   SYSDIR,=C'XSPDIR'                                                
         MVI   USEIO,0                                                          
         MVI   FILENAME,0                                                       
         MVC   FILENAME+1(L'FILENAME-1),FILENAME                                
         BER   RE                                                               
*                                                                               
RE10     DS    0H                                                               
         MVC   LKEY,=H'20'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'     USUALLY NETFILE                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
*                                                                               
         MVI   USEIO,0                                                          
         CLC   =C'DAYP',CONREC     DAYPART RECORDS?                             
         BNE   *+8                                                              
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVI   FILENAME,0                                                       
         MVC   FILENAME+1(L'FILENAME-1),FILENAME                                
         BR    RE                                                               
         EJECT                                                                  
**********************************************                                  
*              DATAMGR INTERFACE                                                
         SPACE 3                                                                
NHIGH    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         B     DIRALL                                                           
         SPACE 1                                                                
NSEQ     NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
         SPACE 1                                                                
NREAD    NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         SPACE 1                                                                
DIRALL   MVC   SYSFIL,=C'SPTDIR  '         DIRECTORIES                          
         CLI   NFILE,C'S'                                                       
         BE    DRL2                                                             
         MVC   SYSFIL,=C'UNTDIR  '                                              
         CLI   NFILE,C'U'                                                       
         BE    DRL2                                                             
         MVC   SYSFIL,=C'STATION '                                              
         CLI   NFILE,C'T'                                                       
DRL2     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,SYSFIL,KEY,KEY,0                            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
NGETREC  NTR1                                                                   
         LA    R3,KEY+14                                                        
         MVC   SYSFIL,=C'SPTFILE '     FILE                                     
         MVC   DATADISP,=H'24'                                                  
         CLI   NFILE,C'S'                                                       
         BE    GETREC2                                                          
         MVC   SYSFIL,=C'UNTFILE '                                              
         MVC   DATADISP,=H'27'                                                  
         LA    R3,KEY+21                                                        
         SPACE 1                                                                
GETREC2  L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,(X'00',=C'GETREC'),SYSFIL,(R3),(R2),DMWORK          
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'92'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* LIST OF SCREENS FROM WHICH RETURNS ARE ALLOWED                                
*                                                                               
RETTAB   DS    0XL1                                                             
         DC    X'82'               CPROG                                        
         DC    X'D8'               NAD DEFINITION                               
         DC    X'E6'               NAD PROGRAM                                  
         DC    X'E8'               NAD UNIVERSE                                 
         DC    X'F7'               PROGRAM                                      
         DC    X'F9'               UNIVERSE                                     
RETURNS  EQU   (*-RETTAB)/L'RETTAB                                              
         SPACE 1                                                                
TRAPERR  BRAS  RE,AEFLDNO                                                       
         GOTO1 ERREX                                                            
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(CLPACK)                                                        
         DC    V(CLUNPK)                                                        
         DC    V(DUMMY)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         EJECT                                                                  
         LTORG                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 1                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'COM     ',AL1(02),X'00C2'                                
         DC    X'01',C'INTG    ',AL1(03),X'00C2'                                
         DC    X'01',C'REP     ',AL1(04),X'00C2'                                
         DC    X'01',C'CLIENT  ',AL1(07),X'00C2'                                
         DC    X'01',C'PRODUCT ',AL1(08),X'00C2'                                
         DC    X'01',C'MARKET  ',AL1(09),X'00C2'                                
         DC    X'01',C'MASTER  ',AL1(10),X'00C2'                                
         DC    X'01',C'PROGRAM ',AL1(11),X'00C2'                                
         DC    X'01',C'UNIVERSE',AL1(12),X'00C2'                                
         DC    X'01',C'SPB     ',AL1(13),X'00C2'                                
******** DC    X'01',C'INDEX   ',AL1(14),X'00C2'                                
         DC    X'01',C'PGSPB   ',AL1(16),X'00C2'                                
         DC    X'01',C'AOR     ',AL1(17),X'00C2'                                
         DC    X'01',C'PGROUP  ',AL1(19),X'00C2'                                
         DC    X'01',C'PGRDEF  ',AL1(18),X'00C2'                                
         DC    X'01',C'PGEST   ',AL1(20),X'00C2'                                
         DC    X'01',C'GFEST   ',AL1(21),X'00C2'                                
         DC    X'01',C'NUNIV   ',AL1(22),X'00C2'                                
         DC    X'01',C'DUNIV   ',AL1(22),X'00C2'                                
         DC    X'01',C'NADDEF  ',AL1(23),X'00C2'                                
         DC    X'01',C'DEMDEF  ',AL1(23),X'00C2'                                
         DC    X'01',C'NPROG   ',AL1(24),X'00C2'                                
         DC    X'01',C'DPROG   ',AL1(24),X'00C2'                                
         DC    X'01',C'CGRDEF  ',AL1(25),X'00C2'                                
         DC    X'01',C'CGROUP  ',AL1(26),X'00C2'                                
         DC    X'01',C'CGASSIGN',AL1(27),X'00C2'                                
         DC    X'01',C'PGASSIGN',AL1(28),X'00C2'                                
         DC    X'01',C'CLRST   ',AL1(29),X'00C2'                                
         DC    X'01',C'UDEF    ',AL1(30),X'00C2'                                
         DC    X'01',C'PSR     ',AL1(31),X'00C2'                                
****     DC    X'01',C'BILLF   ',AL1(32),X'00C2'                                
         DC    X'01',C'REASON  ',AL1(33),X'00C2'                                
         DC    X'01',C'ADDRESS ',AL1(34),X'00C2'                                
         DC    X'01',C'SGDEF   ',AL1(35),X'00C2'                                
         DC    X'01',C'SGROUP  ',AL1(36),X'00C2'                                
         DC    X'01',C'DISTRIB ',AL1(37),X'00C2'                                
         DC    X'01',C'SGASSIGN',AL1(38),X'00C2'                                
         DC    X'01',C'TSUPP   ',AL1(39),X'00C2'                                
         DC    X'01',C'PRGGROUP',AL1(40),X'00C2'                                
         DC    X'01',C'ESTIMATE',AL1(41),X'00C2'                                
         DC    X'01',C'UCOMM   ',AL1(42),X'00C2'                                
         DC    X'01',C'BUYGRP  ',AL1(43),X'00C2'                                
         DC    X'01',C'SUPV    ',AL1(44),X'00C2'                                
         DC    X'01',C'BUYER   ',AL1(45),X'00C2'                                
         DC    X'01',C'DAYPART ',AL1(46),X'00C2'                                
         DC    X'01',C'PRTYPE  ',AL1(47),X'00C2'                                
         DC    X'01',C'SMAP    ',AL1(48),X'00C2'                                
         DC    X'01',C'I2COM   ',AL1(50),X'00C2'                                
         DC    X'01',C'CLCOPY  ',AL1(51),X'00C2'                                
         DC    X'01',C'CMSEP   ',AL1(52),X'00C2'                                
         DC    X'01',C'NN      ',AL1(53),X'00C2'                                
         DC    X'01',C'NNL     ',AL1(54),X'00C2'                                
         DC    X'01',C'NNT     ',AL1(55),X'00C2'                                
         DC    X'01',C'AGENCY  ',AL1(56),X'00C2'                                
         DC    X'01',C'AUTOPAY ',AL1(57),X'00C2'                                
         DC    X'01',C'BFORM   ',AL1(58),X'00C2'                                
         DC    X'01',C'ESTDEMOS',AL1(59),X'00C2'                                
         DC    X'01',X'FF',C'PROGRAM',AL1(60),X'00C2'                           
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'BHOLD   ',AL1(61),X'00C2'                                
         DC    X'01',C'PXCREC  ',AL1(62),X'00C2'                                
         DC    X'01',C'FLIGHT  ',AL1(63),X'00C2'                                
         DC    X'01',C'FLIGHTS ',AL1(63),X'00C2'                                
         DC    X'01',C'LIMIT   ',AL1(64),X'00C2'                                
         DC    X'01',C'COMDEF  ',AL1(65),X'00C2'                                
****     DC    X'01',C'CUNIV   ',AL1(66),X'00C2'                                
         DC    X'01',C'CPROG   ',AL1(67),X'00C2'                                
         DC    X'01',C'BILLADR ',AL1(68),X'00C2'                                
         DC    X'01',C'CLFRZ   ',AL1(69),X'00C2'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
         DC    X'02',C'COPY    ',AL1(07,07,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'MAINTAIN',AL1(16,16,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 1                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                                    CLIENT                     
         DC    X'03',AL1(07,01),X'C1010000C0',C'    '        ADD                
         DC    X'03',AL1(07,02),X'C1010000C0',C'    '        CHANGE             
         DC    X'03',AL1(07,03),X'C1010000C0',C'    '        DISPLAY            
         DC    X'03',AL1(07,04),X'C1010000C0',C'    '        DELETE             
         DC    X'03',AL1(07,05),X'C1010000C0',C'    '        SELECT             
         DC    X'03',AL1(07,06),X'C1010000C0',C'    '        RESTORE            
         DC    X'03',AL1(07,10),X'C2010000D0',C'PCPC'        LIST               
*                                                    PRODUCT                    
         DC    X'03',AL1(08,01),X'C3020000C0',C'    '        ADD                
         DC    X'03',AL1(08,02),X'C3020000C0',C'    '        CHANGE             
         DC    X'03',AL1(08,03),X'C3020000C0',C'    '        DISPLAY            
         DC    X'03',AL1(08,04),X'C3020000C0',C'    '        DELETE             
         DC    X'03',AL1(08,05),X'C3020000C0',C'    '        SELECT             
         DC    X'03',AL1(08,06),X'C3020000C0',C'    '        RESTORE            
         DC    X'03',AL1(08,10),X'C4020000D0',C'PCPC'        LIST               
*                                                    MARKET                     
         DC    X'03',AL1(09,01),X'C7050000C0',C'    '        ADD                
         DC    X'03',AL1(09,02),X'C7050000C0',C'    '        CHANGE             
         DC    X'03',AL1(09,03),X'C7050000C0',C'    '        DISPLAY            
         DC    X'03',AL1(09,04),X'C7050000C0',C'    '        DELETE             
         DC    X'03',AL1(09,05),X'C7050000C0',C'    '        SELECT             
         DC    X'03',AL1(09,06),X'C7050000C0',C'    '        RESTORE            
         DC    X'03',AL1(09,10),X'C8050000C0',C'MANI'        LIST               
*                                                    STATION                    
         DC    X'03',AL1(10,01),X'C9060000C0',C'    '        ADD                
         DC    X'03',AL1(10,02),X'C9060000C0',C'    '        CHANGE             
         DC    X'03',AL1(10,03),X'C9060000C0',C'    '        DISPLAY            
         DC    X'03',AL1(10,04),X'C9060000C0',C'    '        DELETE             
         DC    X'03',AL1(10,05),X'C9060000C0',C'    '        SELECT             
         DC    X'03',AL1(10,06),X'C9060000C0',C'    '        RESTORE            
         DC    X'03',AL1(10,10),X'CA060000C0',C'STNI'        LIST               
*                                                    PROGRAM                    
         DC    X'03',AL1(11,01),X'F7130000C0',C'    '        ADD                
         DC    X'03',AL1(11,02),X'F7130000C0',C'    '        CHANGE             
         DC    X'03',AL1(11,03),X'F7130000C0',C'    '        DISPLAY            
         DC    X'03',AL1(11,04),X'F7130000C0',C'    '        DELETE             
         DC    X'03',AL1(11,05),X'F7130000C0',C'    '        SELECT             
         DC    X'03',AL1(11,06),X'F7130000C0',C'    '        RESTORE            
         DC    X'03',AL1(11,10),X'F8130000C0',C'PGNI'        LIST               
         DC    X'03',AL1(11,12),X'F813000078',C'PGFM'        REPORT             
*                                                    UNIVERSE                   
         DC    X'03',AL1(12,01),X'F9140000C0',C'    '        ADD                
         DC    X'03',AL1(12,02),X'F9140000C0',C'    '        CHANGE             
         DC    X'03',AL1(12,03),X'F9140000C0',C'    '        DISPLAY            
         DC    X'03',AL1(12,04),X'F9140000C0',C'    '        DELETE             
         DC    X'03',AL1(12,05),X'F9140000C0',C'    '        SELECT             
         DC    X'03',AL1(12,06),X'F9140000C0',C'    '        RESTORE            
         DC    X'03',AL1(12,10),X'FA140000C0',C'UNNI'        LIST               
*                                                    COM                        
         DC    X'03',AL1(02,01),X'F1100000C0',C'    '        ADD                
         DC    X'03',AL1(02,02),X'F1100000C0',C'    '        CHANGE             
         DC    X'03',AL1(02,03),X'F1100000C0',C'    '        DISPLAY            
         DC    X'03',AL1(02,04),X'F1100000C0',C'    '        DELETE             
         DC    X'03',AL1(02,05),X'F1100000C0',C'    '        SELECT             
         DC    X'03',AL1(02,06),X'F1100000C0',C'    '        RESTORE            
         DC    X'03',AL1(02,10),X'F2100000C0',C'ELFL'        LIST               
*                                                    INTG                       
         DC    X'03',AL1(03,01),X'F3200000C0',C'    '        ADD                
         DC    X'03',AL1(03,02),X'F3200000C0',C'    '        CHANGE             
         DC    X'03',AL1(03,03),X'F3200000C0',C'    '        DISPLAY            
         DC    X'03',AL1(03,04),X'F3200000C0',C'    '        DELETE             
         DC    X'03',AL1(03,05),X'F3200000C0',C'    '        SELECT             
         DC    X'03',AL1(03,06),X'F3200000C0',C'    '        RESTORE            
         DC    X'03',AL1(03,10),X'F4200000C0',C'ITFI'        LIST               
*                                                    REP                        
         DC    X'03',AL1(04,01),X'F5120000C0',C'    '        ADD                
         DC    X'03',AL1(04,02),X'F5120000C0',C'    '        CHANGE             
         DC    X'03',AL1(04,03),X'F5120000C0',C'    '        DISPLAY            
         DC    X'03',AL1(04,04),X'F5120000C0',C'    '        DELETE             
         DC    X'03',AL1(04,05),X'F5120000C0',C'    '        SELECT             
         DC    X'03',AL1(04,06),X'F5120000C0',C'    '        RESTORE            
         DC    X'03',AL1(04,10),X'F6120000C0',C'RPFL'        LIST               
*                                                    SPB                        
         DC    X'03',AL1(13,01),X'FC1C0000C0',C'    '        ADD                
         DC    X'03',AL1(13,02),X'FC1C0000C0',C'    '        CHANGE             
         DC    X'03',AL1(13,03),X'FC1C0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(13,04),X'FC1C0000C0',C'    '        DELETE             
         DC    X'03',AL1(13,05),X'FC1C0000C0',C'    '        SELECT             
         DC    X'03',AL1(13,06),X'FC1C0000C0',C'    '        RESTORE            
         DC    X'03',AL1(13,10),X'FD1C0000C0',C'SPNI'        LIST               
*                                                    INDX                       
         DC    X'03',AL1(14,01),X'CB150000C0',C'    '        ADD                
         DC    X'03',AL1(14,02),X'CB150000C0',C'    '        CHANGE             
         DC    X'03',AL1(14,03),X'CB150000C0',C'    '        DISPLAY            
         DC    X'03',AL1(14,04),X'CB150000C0',C'    '        DELETE             
         DC    X'03',AL1(14,05),X'CB150000C0',C'    '        SELECT             
         DC    X'03',AL1(14,06),X'CB150000C0',C'    '        RESTORE            
         DC    X'03',AL1(14,10),X'CC150000C0',C'IXNI'        LIST               
*                                                    PGSPB                      
         DC    X'03',AL1(16,01),X'931D0000C0',C'    '        ADD                
         DC    X'03',AL1(16,02),X'931D0000C0',C'    '        CHANGE             
         DC    X'03',AL1(16,03),X'931D0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(16,04),X'931D0000C0',C'    '        DELETE             
         DC    X'03',AL1(16,05),X'931D0000C0',C'    '        SELECT             
         DC    X'03',AL1(16,06),X'931D0000C0',C'    '        RESTORE            
         DC    X'03',AL1(16,10),X'941D0000C0',C'SPNI'        LIST               
*                                                    AOR                        
         DC    X'03',AL1(17,01),X'D0110000C0',C'    '        ADD                
         DC    X'03',AL1(17,02),X'D0110000C0',C'    '        CHANGE             
         DC    X'03',AL1(17,03),X'D0110000C0',C'    '        DISPLAY            
         DC    X'03',AL1(17,04),X'D0110000C0',C'    '        DELETE             
         DC    X'03',AL1(17,05),X'D0110000C0',C'    '        SELECT             
         DC    X'03',AL1(17,06),X'D0110000C0',C'    '        RESTORE            
         DC    X'03',AL1(17,10),X'D1110000C0',C'AONA'        LIST               
         DC    X'03',AL1(17,12),X'D111000058',C'AONA'        REPORT             
*                                                    PGRDEF                     
         DC    X'03',AL1(18,01),X'D2410000C0',C'    '        ADD                
         DC    X'03',AL1(18,02),X'D2410000C0',C'    '        CHANGE             
         DC    X'03',AL1(18,03),X'D2410000C0',C'    '        DISPLAY            
         DC    X'03',AL1(18,04),X'D2410000C0',C'    '        DELETE             
         DC    X'03',AL1(18,05),X'D2410000C0',C'    '        SELECT             
         DC    X'03',AL1(18,06),X'D2410000C0',C'    '        RESTORE            
         DC    X'03',AL1(18,10),X'D4410000D0',C'PCPC'        LIST               
*                                                    PGROUP                     
         DC    X'03',AL1(19,01),X'D3400000C0',C'    '        ADD                
         DC    X'03',AL1(19,02),X'D3400000C0',C'    '        CHANGE             
         DC    X'03',AL1(19,03),X'D3400000C0',C'    '        DISPLAY            
         DC    X'03',AL1(19,04),X'D3400000C0',C'    '        DELETE             
         DC    X'03',AL1(19,05),X'D3400000C0',C'    '        SELECT             
         DC    X'03',AL1(19,06),X'D3400000C0',C'    '        RESTORE            
         DC    X'03',AL1(19,10),X'D5400000D0',C'PCPC'        LIST               
         DC    X'03',AL1(19,12),X'D540000038',C'PGPG'        REPORT             
*                                                    PGEST                      
         DC    X'03',AL1(20,01),X'CD210000C0',C'    '        ADD                
         DC    X'03',AL1(20,02),X'CD210000C0',C'    '        CHANGE             
         DC    X'03',AL1(20,03),X'CD210000C0',C'    '        DISPLAY            
         DC    X'03',AL1(20,04),X'CD210000C0',C'    '        DELETE             
         DC    X'03',AL1(20,05),X'CD210000C0',C'    '        SELECT             
         DC    X'03',AL1(20,06),X'CD210000C0',C'    '        RESTORE            
         DC    X'03',AL1(20,10),X'CE210000C0',C'PGPG'        LIST               
         DC    X'03',AL1(20,12),X'CE21000058',C'PGPG'        REPORT             
*                                                    GFEST                      
         DC    X'03',AL1(21,01),X'D6220000C0',C'    '        ADD                
         DC    X'03',AL1(21,02),X'D6220000C0',C'    '        CHANGE             
         DC    X'03',AL1(21,03),X'D6220000C0',C'    '        DISPLAY            
         DC    X'03',AL1(21,04),X'D6220000C0',C'    '        DELETE             
         DC    X'03',AL1(21,05),X'D6220000C0',C'    '        SELECT             
         DC    X'03',AL1(21,06),X'D6220000C0',C'    '        RESTORE            
         DC    X'03',AL1(21,10),X'D7220000C0',C'    '        LIST               
         DC    X'03',AL1(21,12),X'D722000078',C'    '        REPORT             
*                                                    NUNIV                      
         DC    X'03',AL1(22,01),X'E8190000C0',C'    '        ADD                
         DC    X'03',AL1(22,02),X'E8190000C0',C'    '        CHANGE             
         DC    X'03',AL1(22,03),X'E8190000C0',C'    '        DISPLAY            
         DC    X'03',AL1(22,04),X'E8190000C0',C'    '        DELETE             
         DC    X'03',AL1(22,05),X'E8190000C0',C'    '        SELECT             
         DC    X'03',AL1(22,06),X'E8190000C0',C'    '        RESTORE            
         DC    X'03',AL1(22,10),X'E9190000C0',C'NUNI'        LIST               
*                                                    NADDEF                     
         DC    X'03',AL1(23,01),X'D8170000C0',C'    '        ADD                
         DC    X'03',AL1(23,02),X'D8170000C0',C'    '        CHANGE             
         DC    X'03',AL1(23,03),X'D8170000C0',C'    '        DISPLAY            
         DC    X'03',AL1(23,04),X'D8170000C0',C'    '        DELETE             
         DC    X'03',AL1(23,05),X'D8170000C0',C'    '        SELECT             
         DC    X'03',AL1(23,06),X'D8170000C0',C'    '        RESTORE            
         DC    X'03',AL1(23,10),X'D9170000C0',C'    '        LIST               
*                                                    NPROG                      
         DC    X'03',AL1(24,01),X'E6180000C0',C'    '        ADD                
         DC    X'03',AL1(24,02),X'E6180000C0',C'    '        CHANGE             
         DC    X'03',AL1(24,03),X'E6180000C0',C'    '        DISPLAY            
         DC    X'03',AL1(24,04),X'E6180000C0',C'    '        DELETE             
         DC    X'03',AL1(24,05),X'E6180000C0',C'    '        SELECT             
         DC    X'03',AL1(24,06),X'E6180000C0',C'    '        RESTORE            
         DC    X'03',AL1(24,10),X'E7180000C0',C'    '        LIST               
*                                                    CGRDEF                     
         DC    X'03',AL1(25,01),X'DA310000C0',C'    '        ADD                
         DC    X'03',AL1(25,02),X'DA310000C0',C'    '        CHANGE             
         DC    X'03',AL1(25,03),X'DA310000C0',C'    '        DISPLAY            
         DC    X'03',AL1(25,04),X'DA310000C0',C'    '        DELETE             
         DC    X'03',AL1(25,05),X'DA310000C0',C'    '        SELECT             
         DC    X'03',AL1(25,06),X'DA310000C0',C'    '        RESTORE            
         DC    X'03',AL1(25,10),X'DB310000D0',C'PCPC'        LIST               
*                                                    CGROUP                     
         DC    X'03',AL1(26,01),X'DC300000C0',C'    '        ADD                
         DC    X'03',AL1(26,02),X'DC300000C0',C'    '        CHANGE             
         DC    X'03',AL1(26,03),X'DC300000C0',C'    '        DISPLAY            
         DC    X'03',AL1(26,04),X'DC300000C0',C'    '        DELETE             
         DC    X'03',AL1(26,05),X'DC300000C0',C'    '        SELECT             
         DC    X'03',AL1(26,06),X'DC300000C0',C'    '        RESTORE            
         DC    X'03',AL1(26,10),X'DD300000D0',C'PCPC'        LIST               
         DC    X'03',AL1(26,12),X'DD30000038',C'CGCG'        REPORT             
*                                                    CGASSIGN                   
         DC    X'03',AL1(27,01),X'E0320000C1',C'    '        ADD                
         DC    X'03',AL1(27,02),X'E0320000C1',C'    '        CHANGE             
         DC    X'03',AL1(27,03),X'E0320000C1',C'    '        DISPLAY            
         DC    X'03',AL1(27,04),X'E0320000C1',C'    '        DELETE             
         DC    X'03',AL1(27,05),X'E0320000C1',C'    '        SELECT             
         DC    X'03',AL1(27,06),X'E0320000C1',C'    '        RESTORE            
*                                                    PGASSIGN                   
         DC    X'03',AL1(28,01),X'E4320000C1',C'    '        ADD                
         DC    X'03',AL1(28,02),X'E4320000C1',C'    '        CHANGE             
         DC    X'03',AL1(28,03),X'E4320000C1',C'    '        DISPLAY            
         DC    X'03',AL1(28,04),X'E4320000C1',C'    '        DELETE             
         DC    X'03',AL1(28,05),X'E4320000C1',C'    '        SELECT             
         DC    X'03',AL1(28,06),X'E4320000C1',C'    '        RESTORE            
*                                                    CLRST                      
         DC    X'03',AL1(29,10),X'EB330000D0',C'    '        LIST               
*                                                    UDEFD                      
         DC    X'03',AL1(30,01),X'EC340000D0',C'    '        ADD                
         DC    X'03',AL1(30,02),X'EC340000D0',C'    '        CHANGE             
         DC    X'03',AL1(30,03),X'EC340000D0',C'    '        DISPLAY            
         DC    X'03',AL1(30,04),X'EC340000D0',C'    '        DELETE             
         DC    X'03',AL1(30,05),X'EC340000D0',C'    '        SELECT             
         DC    X'03',AL1(30,06),X'EC340000D0',C'    '        RESTORE            
*                                                    UDEFL                      
         DC    X'03',AL1(30,10),X'ED350000D0',C'    '        LIST               
*                                                    PSR                        
         DC    X'03',AL1(31,01),X'EA360000D0',C'    '        ADD                
         DC    X'03',AL1(31,02),X'EA360000D0',C'    '        CHANGE             
         DC    X'03',AL1(31,03),X'EA360000D0',C'    '        DISPLAY            
         DC    X'03',AL1(31,04),X'EA360000D0',C'    '        DELETE             
         DC    X'03',AL1(31,05),X'EA360000D0',C'    '        SELECT             
         DC    X'03',AL1(31,06),X'EA360000D0',C'    '        RESTORE            
         DC    X'03',AL1(31,10),X'EE360000D0',C'    '        LIST               
*                                                    BILLF                      
*****    DC    X'03',AL1(32,16),X'9F3B000081',C'    '        MAINTAIN           
*                                                    REASD                      
         DC    X'03',AL1(33,01),X'B1240000C0',C'    '        ADD                
         DC    X'03',AL1(33,02),X'B1240000C0',C'    '        CHANGE             
         DC    X'03',AL1(33,03),X'B1240000C0',C'    '        DISPLAY            
         DC    X'03',AL1(33,04),X'B1240000C0',C'    '        DELETE             
         DC    X'03',AL1(33,05),X'B1240000C0',C'    '        SELECT             
         DC    X'03',AL1(33,06),X'B1240000C0',C'    '        RESTORE            
         DC    X'03',AL1(33,10),X'B2240000C0',C'    '        LIST               
*                                                    ADDRESS                    
         DC    X'03',AL1(34,01),X'B3250000C0',C'    '        ADD                
         DC    X'03',AL1(34,02),X'B3250000C0',C'    '        CHANGE             
         DC    X'03',AL1(34,03),X'B3250000C0',C'    '        DISPLAY            
         DC    X'03',AL1(34,04),X'B3250000C0',C'    '        DELETE             
         DC    X'03',AL1(34,05),X'B3250000C0',C'    '        SELECT             
         DC    X'03',AL1(34,06),X'B3250000C0',C'    '        RESTORE            
         DC    X'03',AL1(34,10),X'B4250000C0',C'ADNI'        LIST               
*                                                    SGDEF                      
         DC    X'03',AL1(35,01),X'B5260000C0',C'    '        ADD                
         DC    X'03',AL1(35,02),X'B5260000C0',C'    '        CHANGE             
         DC    X'03',AL1(35,03),X'B5260000C0',C'    '        DISPLAY            
         DC    X'03',AL1(35,04),X'B5260000C0',C'    '        DELETE             
         DC    X'03',AL1(35,05),X'B5260000C0',C'    '        SELECT             
         DC    X'03',AL1(35,06),X'B5260000C0',C'    '        RESTORE            
         DC    X'03',AL1(35,10),X'B6260000C0',C'    '        LIST               
*                                                    SGROUP                     
         DC    X'03',AL1(36,01),X'B7270000C0',C'    '        ADD                
         DC    X'03',AL1(36,02),X'B7270000C0',C'    '        CHANGE             
         DC    X'03',AL1(36,03),X'B7270000C0',C'    '        DISPLAY            
         DC    X'03',AL1(36,04),X'B7270000C0',C'    '        DELETE             
         DC    X'03',AL1(36,05),X'B7270000C0',C'    '        SELECT             
         DC    X'03',AL1(36,06),X'B7270000C0',C'    '        RESTORE            
         DC    X'03',AL1(36,10),X'B8270000C0',C'    '        LIST               
*                                                    DIST                       
         DC    X'03',AL1(37,01),X'B9280000C0',C'    '        ADD                
         DC    X'03',AL1(37,02),X'B9280000C0',C'    '        CHANGE             
         DC    X'03',AL1(37,03),X'B9280000C0',C'    '        DISPLAY            
         DC    X'03',AL1(37,04),X'B9280000C0',C'    '        DELETE             
         DC    X'03',AL1(37,05),X'B9280000C0',C'    '        SELECT             
         DC    X'03',AL1(37,06),X'B9280000C0',C'    '        RESTORE            
         DC    X'03',AL1(37,10),X'BA280000C0',C'    '        LIST               
*                                                    SGASSIGN                   
         DC    X'03',AL1(38,01),X'BB290000C0',C'    '        ADD                
         DC    X'03',AL1(38,02),X'BB290000C0',C'    '        CHANGE             
         DC    X'03',AL1(38,03),X'BB290000C0',C'    '        DISPLAY            
         DC    X'03',AL1(38,04),X'BB290000C0',C'    '        DELETE             
         DC    X'03',AL1(38,05),X'BB290000C0',C'    '        SELECT             
         DC    X'03',AL1(38,06),X'BB290000C0',C'    '        RESTORE            
*                                                    TSUPP                      
         DC    X'03',AL1(39,01),X'BC370000C0',C'    '        ADD                
         DC    X'03',AL1(39,02),X'BC370000C0',C'    '        CHANGE             
         DC    X'03',AL1(39,03),X'BC370000C0',C'    '        DISPLAY            
         DC    X'03',AL1(39,04),X'BC370000C0',C'    '        DELETE             
         DC    X'03',AL1(39,05),X'BC370000C0',C'    '        SELECT             
         DC    X'03',AL1(39,06),X'BC370000C0',C'    '        RESTORE            
         DC    X'03',AL1(39,10),X'BD370000C0',C'TSNI'        LIST               
*                                                    PGROUP                     
         DC    X'03',AL1(40,01),X'CF360000C0',C'    '        ADD                
         DC    X'03',AL1(40,02),X'CF360000C0',C'    '        CHANGE             
         DC    X'03',AL1(40,03),X'CF360000C0',C'    '        DISPLAY            
         DC    X'03',AL1(40,04),X'CF360000C0',C'    '        DELETE             
         DC    X'03',AL1(40,05),X'CF360000C0',C'    '        SELECT             
         DC    X'03',AL1(40,06),X'CF360000C0',C'    '        RESTORE            
         DC    X'03',AL1(40,10),X'C6360000C0',C'    '        LIST               
         DC    X'03',AL1(40,12),X'C636000070',C'    '        REPORT             
*                                                    ESTIMATE                   
         DC    X'03',AL1(41,01),X'BE030000C0',C'    '        ADD                
         DC    X'03',AL1(41,02),X'BE030000C0',C'    '        CHANGE             
         DC    X'03',AL1(41,03),X'BE030000C0',C'    '        DISPLAY            
         DC    X'03',AL1(41,04),X'BE030000C0',C'    '        DELETE             
         DC    X'03',AL1(41,05),X'BE030000C0',C'    '        SELECT             
         DC    X'03',AL1(41,06),X'BE030000C0',C'    '        RESTORE            
         DC    X'03',AL1(41,07),X'B0070000C0',C'    '        COPY               
         DC    X'03',AL1(41,10),X'BF040000C0',C'    '        LIST               
*                                                    ESTDEMOS                   
         DC    X'03',AL1(59,01),X'97540000C0',C'    '        ADD                
         DC    X'03',AL1(59,02),X'97540000C0',C'    '        CHANGE             
         DC    X'03',AL1(59,03),X'97540000C0',C'    '        DISPLAY            
         DC    X'03',AL1(59,04),X'97540000C0',C'    '        DELETE             
         DC    X'03',AL1(59,05),X'97540000C0',C'    '        SELECT             
         DC    X'03',AL1(59,06),X'97540000C0',C'    '        RESTORE            
*                                                    UCOMM                      
         DC    X'03',AL1(42,01),X'685B0000C0',C'    '        ADD                
         DC    X'03',AL1(42,02),X'685B0000C0',C'    '        CHANGE             
         DC    X'03',AL1(42,03),X'685B0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(42,04),X'685B0000C0',C'    '        DELETE             
         DC    X'03',AL1(42,05),X'685B0000C0',C'    '        SELECT             
         DC    X'03',AL1(42,06),X'685B0000C0',C'    '        RESTORE            
         DC    X'03',AL1(42,10),X'695B0000C0',C'    '        LIST               
*                                                    BUYGRP                     
         DC    X'03',AL1(43,01),X'A04A0000C0',C'    '        ADD                
         DC    X'03',AL1(43,02),X'A04A0000C0',C'    '        CHANGE             
         DC    X'03',AL1(43,03),X'A04A0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(43,04),X'A04A0000C0',C'    '        DELETE             
         DC    X'03',AL1(43,05),X'A04A0000C0',C'    '        SELECT             
         DC    X'03',AL1(43,06),X'A04A0000C0',C'    '        RESTORE            
         DC    X'03',AL1(43,10),X'A14A0000C0',C'    '        LIST               
*                                                    SUPV                       
         DC    X'03',AL1(44,01),X'A24B0000C0',C'    '        ADD                
         DC    X'03',AL1(44,02),X'A24B0000C0',C'    '        CHANGE             
         DC    X'03',AL1(44,03),X'A24B0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(44,04),X'A24B0000C0',C'    '        DELETE             
         DC    X'03',AL1(44,05),X'A24B0000C0',C'    '        SELECT             
         DC    X'03',AL1(44,06),X'A24B0000C0',C'    '        RESTORE            
         DC    X'03',AL1(44,10),X'A34B0000C0',C'    '        LIST               
*                                                    BUYER                      
         DC    X'03',AL1(45,01),X'A44C0000C0',C'    '        ADD                
         DC    X'03',AL1(45,02),X'A44C0000C0',C'    '        CHANGE             
         DC    X'03',AL1(45,03),X'A44C0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(45,04),X'A44C0000C0',C'    '        DELETE             
         DC    X'03',AL1(45,05),X'A44C0000C0',C'    '        SELECT             
         DC    X'03',AL1(45,06),X'A44C0000C0',C'    '        RESTORE            
         DC    X'03',AL1(45,10),X'A64C0000C0',C'    '        LIST               
*                                                    DAYPART                    
         DC    X'03',AL1(46,01),X'A8080000C0',C'    '        ADD                
         DC    X'03',AL1(46,02),X'A8080000C0',C'    '        CHANGE             
         DC    X'03',AL1(46,03),X'A8080000C0',C'    '        DISPLAY            
         DC    X'03',AL1(46,04),X'A8080000C0',C'    '        DELETE             
         DC    X'03',AL1(46,05),X'A8080000C0',C'    '        SELECT             
         DC    X'03',AL1(46,06),X'A8080000C0',C'    '        RESTORE            
         DC    X'03',AL1(46,10),X'A9080000C0',C'DPDP'        LIST               
*                                                    PRTYPE                     
         DC    X'03',AL1(47,01),X'AA390000C0',C'    '        ADD                
         DC    X'03',AL1(47,02),X'AA390000C0',C'    '        CHANGE             
         DC    X'03',AL1(47,03),X'AA390000C0',C'    '        DISPLAY            
         DC    X'03',AL1(47,04),X'AA390000C0',C'    '        DELETE             
         DC    X'03',AL1(47,05),X'AA390000C0',C'    '        SELECT             
         DC    X'03',AL1(47,06),X'AA390000C0',C'    '        RESTORE            
         DC    X'03',AL1(47,10),X'AB390000C0',C'    '        LIST               
*                                                    SMAP                       
         DC    X'03',AL1(48,01),X'AC420000C0',C'    '        ADD                
         DC    X'03',AL1(48,02),X'AC420000C0',C'    '        CHANGE             
         DC    X'03',AL1(48,03),X'AC420000C0',C'    '        DISPLAY            
         DC    X'03',AL1(48,04),X'AC420000C0',C'    '        DELETE             
         DC    X'03',AL1(48,05),X'AC420000C0',C'    '        SELECT             
         DC    X'03',AL1(48,06),X'AC420000C0',C'    '        RESTORE            
         DC    X'03',AL1(48,10),X'AD420000C0',C'    '        LIST               
*                                                    I2COM                      
         DC    X'03',AL1(50,01),X'61450000C0',C'    '        ADD                
         DC    X'03',AL1(50,02),X'61450000C0',C'    '        CHANGE             
         DC    X'03',AL1(50,03),X'61450000C0',C'    '        DISPLAY            
         DC    X'03',AL1(50,04),X'61450000C0',C'    '        DELETE             
         DC    X'03',AL1(50,05),X'61450000C0',C'    '        SELECT             
         DC    X'03',AL1(50,06),X'61450000C0',C'    '        RESTORE            
         DC    X'03',AL1(50,10),X'62460000C0',C'    '        LIST               
*                                                    CLCOPY                     
         DC    X'03',AL1(51,12),X'AE47000018',C'C5C5'        REPORT             
*                                                    CMSEP                      
         DC    X'03',AL1(52,12),X'AF48000038',C'CNCN'        REPORT             
*                                                    NN                         
         DC    X'03',AL1(53,12),X'9E49000010',C'NNNN'        REPORT             
*                                                    NNL                        
         DC    X'03',AL1(54,01),X'90500000C0',C'    '        ADD                
         DC    X'03',AL1(54,02),X'90500000C0',C'    '        CHANGE             
         DC    X'03',AL1(54,03),X'90500000C0',C'    '        DISPLAY            
         DC    X'03',AL1(54,04),X'90500000C0',C'    '        DELETE             
         DC    X'03',AL1(54,05),X'90500000C0',C'    '        SELECT             
         DC    X'03',AL1(54,06),X'90500000C0',C'    '        RESTORE            
*                                                    NNT                        
         DC    X'03',AL1(55,01),X'91510000C0',C'    '        ADD                
         DC    X'03',AL1(55,02),X'91510000C0',C'    '        CHANGE             
         DC    X'03',AL1(55,03),X'91510000C0',C'    '        DISPLAY            
         DC    X'03',AL1(55,04),X'91510000C0',C'    '        DELETE             
         DC    X'03',AL1(55,05),X'91510000C0',C'    '        SELECT             
         DC    X'03',AL1(55,06),X'91510000C0',C'    '        RESTORE            
         DC    X'03',AL1(55,10),X'92520000C0',C'    '        LIST               
*                                                    AGENCY                     
         DC    X'03',AL1(56,01),X'9D090000C0',C'    '        ADD                
         DC    X'03',AL1(56,02),X'9D090000C0',C'    '        CHANGE             
         DC    X'03',AL1(56,03),X'9D090000C0',C'    '        DISPLAY            
         DC    X'03',AL1(56,04),X'9D090000C0',C'    '        DELETE             
         DC    X'03',AL1(56,05),X'9D090000C0',C'    '        SELECT             
         DC    X'03',AL1(56,06),X'9D090000C0',C'    '        RESTORE            
*                                                    AUTOPAY                    
         DC    X'03',AL1(57,03),X'96530000C0',C'    '        DISPLAY            
         DC    X'03',AL1(57,05),X'96530000C0',C'    '        SELECT             
         DC    X'03',AL1(57,10),X'95530000C0',C'    '        LIST               
*                                                    BFORM                      
         DC    X'03',AL1(58,01),X'4E6C0000C0',C'    '        ADD                
         DC    X'03',AL1(58,02),X'4E6C0000C0',C'    '        CHANGE             
         DC    X'03',AL1(58,03),X'4E6C0000C0',C'    '        DISPLAY            
         DC    X'03',AL1(58,04),X'4E6C0000C0',C'    '        DELETE             
         DC    X'03',AL1(58,05),X'4E6C0000C0',C'    '        SELECT             
         DC    X'03',AL1(58,06),X'4E6C0000C0',C'    '        RESTORE            
         DC    X'03',AL1(58,10),X'4F6C0000F8',C'BFFM'        LIST               
         DC    X'03',AL1(58,12),X'4F6C000078',C'BFFM'        REPORT             
*                                                    PROGRAM                    
         DC    X'03',AL1(60,01),X'C5130000C0',C'    '        ADD                
         DC    X'03',AL1(60,02),X'C5130000C0',C'    '        CHANGE             
         DC    X'03',AL1(60,03),X'C5130000C0',C'    '        DISPLAY            
         DC    X'03',AL1(60,04),X'C5130000C0',C'    '        DELETE             
         DC    X'03',AL1(60,05),X'C5130000C0',C'    '        SELECT             
         DC    X'03',AL1(60,06),X'C5130000C0',C'    '        RESTORE            
*                                                    BHOLD                      
         DC    X'03',AL1(61,01),X'98550000C0',C'    '        ADD                
         DC    X'03',AL1(61,02),X'98550000C0',C'    '        CHANGE             
         DC    X'03',AL1(61,03),X'98550000C0',C'    '        DISPLAY            
         DC    X'03',AL1(61,04),X'98550000C0',C'    '        DELETE             
         DC    X'03',AL1(61,05),X'98550000C0',C'    '        SELECT             
         DC    X'03',AL1(61,06),X'98550000C0',C'    '        RESTORE            
         DC    X'03',AL1(61,10),X'99550000C0',C'    '        LIST               
*                                                       (VIA DDLINK)            
*                                                    PXCREC                     
         DC    X'03',AL1(62,01),X'9A560000C0',C'    '        ADD                
         DC    X'03',AL1(62,02),X'9A560000C0',C'    '        CHANGE             
         DC    X'03',AL1(62,03),X'9A560000C0',C'    '        DISPLAY            
         DC    X'03',AL1(62,04),X'9A560000C0',C'    '        DELETE             
         DC    X'03',AL1(62,05),X'9A560000C0',C'    '        SELECT             
         DC    X'03',AL1(62,06),X'9A560000C0',C'    '        RESTORE            
         DC    X'03',AL1(62,10),X'80560000C0',C'    '        LIST               
*                                                    FLIGHT                     
         DC    X'03',AL1(63,01),X'9B570000C0',C'    '        ADD                
         DC    X'03',AL1(63,02),X'9B570000C0',C'    '        CHANGE             
         DC    X'03',AL1(63,03),X'9B570000C0',C'    '        DISPLAY            
         DC    X'03',AL1(63,04),X'9B570000C0',C'    '        DELETE             
         DC    X'03',AL1(63,05),X'9B570000C0',C'    '        SELECT             
         DC    X'03',AL1(63,06),X'9B570000C0',C'    '        RESTORE            
         DC    X'03',AL1(63,10),X'9C570000C0',C'    '        LIST               
         DC    X'03',AL1(63,12),X'9C57000078',C'WBWB'        REPORT             
*                                                    LIMIT                      
         DC    X'03',AL1(64,01),X'81580000C0',C'    '        ADD                
         DC    X'03',AL1(64,02),X'81580000C0',C'    '        CHANGE             
         DC    X'03',AL1(64,03),X'81580000C0',C'    '        DISPLAY            
         DC    X'03',AL1(64,04),X'81580000C0',C'    '        DELETE             
         DC    X'03',AL1(64,05),X'81580000C0',C'    '        SELECT             
         DC    X'03',AL1(64,06),X'81580000C0',C'    '        RESTORE            
         DC    X'03',AL1(64,10),X'80580000C0',C'    '        LIST               
*                                                    COMDEF                     
         DC    X'03',AL1(65,01),X'D8170000C0',C'    '        ADD                
         DC    X'03',AL1(65,02),X'D8170000C0',C'    '        CHANGE             
         DC    X'03',AL1(65,03),X'D8170000C0',C'    '        DISPLAY            
         DC    X'03',AL1(65,04),X'D8170000C0',C'    '        DELETE             
         DC    X'03',AL1(65,05),X'D8170000C0',C'    '        SELECT             
         DC    X'03',AL1(65,06),X'D8170000C0',C'    '        RESTORE            
         DC    X'03',AL1(65,10),X'D9170000C0',C'    '        LIST               
*                                                    CUNIV                      
         DC    X'03',AL1(66,01),X'E8190000C0',C'    '        ADD                
         DC    X'03',AL1(66,02),X'E8190000C0',C'    '        CHANGE             
         DC    X'03',AL1(66,03),X'E8190000C0',C'    '        DISPLAY            
         DC    X'03',AL1(66,04),X'E8190000C0',C'    '        DELETE             
         DC    X'03',AL1(66,05),X'E8190000C0',C'    '        SELECT             
         DC    X'03',AL1(66,06),X'E8190000C0',C'    '        RESTORE            
         DC    X'03',AL1(66,10),X'E9190000C0',C'CUNI'        LIST               
*                                                    CPROG                      
         DC    X'03',AL1(67,01),X'82180000C0',C'    '        ADD                
         DC    X'03',AL1(67,02),X'82180000C0',C'    '        CHANGE             
         DC    X'03',AL1(67,03),X'82180000C0',C'    '        DISPLAY            
         DC    X'03',AL1(67,04),X'82180000C0',C'    '        DELETE             
         DC    X'03',AL1(67,05),X'82180000C0',C'    '        SELECT             
         DC    X'03',AL1(67,06),X'82180000C0',C'    '        RESTORE            
         DC    X'03',AL1(67,10),X'E7180000C0',C'    '        LIST               
*                                                    BILLADR                    
         DC    X'03',AL1(68,01),X'5C590000C0',C'    '        ADD                
         DC    X'03',AL1(68,02),X'5C590000C0',C'    '        CHANGE             
         DC    X'03',AL1(68,03),X'5C590000C0',C'    '        DISPLAY            
         DC    X'03',AL1(68,04),X'5C590000C0',C'    '        DELETE             
         DC    X'03',AL1(68,05),X'5C590000C0',C'    '        SELECT             
         DC    X'03',AL1(68,06),X'5C590000C0',C'    '        RESTORE            
         DC    X'03',AL1(68,10),X'5D590000C0',C'    '        LIST               
*                                                                               
*                                                    CLFRZ                      
         DC    X'03',AL1(69,12),X'5E60000030',C'CZCZ'        REPORT             
*                                                                               
         DC    X'FF'                                                            
*                                                                               
********************************************************************            
* GET TOKEN RECORD TO VERIFY ACCESS TO RENTRAK DEMOS                            
*                                                                               
         USING GTTOKD,R2                                                        
GETTOKEN NTR1  BASE=*,LABEL=*,WORK=(R2,GTTOKL)                                  
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         NI    USRIDFLG,X'FF'-USRRNTKQ                                          
         MVC   HALF,TWAAGY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,GTTOKIO             
         LA    R4,GTTOKIO                                                       
         LA    R6,CT5DATA                                                       
         USING CTSEAD,R6                                                        
         MVI   ELCODE,CTSEAELQ     SECURITY AGENCY ALPHA                        
         BRAS  RE,FIRSTEL                                                       
         BNE   *+10                                                             
         MVC   HALF,CTSEAAID                                                    
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY                              
         MVC   TOKKAAGY,TWAAGY                                                  
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   GTOKENX                                                          
*                                                                               
         LA    R4,GTTOKIO                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
*                                                                               
         AHI   R4,TOKFIRST                                                      
         USING RTAUTHD,R4                                                       
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GTOKENX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GTOKENX                                                          
         OI    USRIDFLG,USRRNTKQ   HAS ACCESS TO RENTRAK DEMOS                  
*                                                                               
GTOKENX  DS    0H                                                               
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
GTTOKD   DSECT                                                                  
GTTOKIO  DS    XL2000                                                           
GTTOKL   EQU   *-GTTOKD                                                         
T31C00   CSECT                                                                  
*                                                                               
**********************************************************************          
* VALIDATE DAYPART                                                              
*                                                                               
* AT ENTRY R2 CONTAINS A(DAYPART FIELD ON SCREEN)                               
*                                                                               
* IF PARAM1 ENTERED, BYTE 0 =  X'01' (DAYPART EQUATE ENTERED)                   
*                    BYTES 1-3 = A(ONE BYTE DAYPARTE EQUATE)                    
*                                                                               
* ON RETURN                                                                     
* QDPT = 1 OR 2 BYTE DAYPART CODE                                               
* QDPT2 = 2 BYTE ALPHA DAYPART CODE                                             
* DPTNAME = 14 BYTE DAYPART DESCRIPTION                                         
**********************************************************************          
VALDPT   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         XC    QDPT,QDPT                                                        
         XC    QDPT2,QDPT2                                                      
         XC    DPTNAME,DPTNAME                                                  
*                                                                               
         CLI   0(R1),X'01'         DAYPART EQUATE ENTERED?                      
         BE    VDPT200                                                          
*                                                                               
         LA    R3,DAYPARTS         CHECK IN DAYPART TABLE                       
         OC    8(2,R2),=C'  '                                                   
*                                                                               
VDPT005  DS    0H                                                               
         CLI   0(R3),X'FF'         NOT IN TABLE - CHECK RECORDS                 
         BE    VDPT010                                                          
         CLC   8(2,R2),0(R3)                                                    
         BE    DPTOK                                                            
*                                                                               
         LA    R3,10(R3)                                                        
         B     VDPT005                                                          
*                                                                               
DPTOK    DS    0H                                                               
         MVC   QDPT,0(R3)          PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,0(R3)         PASS BACK DAYPART                            
         MVC   DPTNAME,2(R3)       PASS BACK DESCRIPTION                        
         B     VDPTX                                                            
*                                                                               
VDPT010  DS    0H                  CHECK DAYPART RECORDS                        
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,BAGYMD                                                   
*                                                                               
VDPT020  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                 
         B     VDPT040                                                          
*                                                                               
VDPT030  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                 
*                                                                               
VDPT040  CLC   KEY(5),KEYSAVE      AGY LEVEL/CLIENT LEVEL                       
         BNE   VDPT100             NO                                           
*                                                                               
         OC    8(2,R2),=C'  '                                                   
         CLC   NDPTDPTA,8(R2)      MATCH ON CODE?                               
         BNE   VDPT030                                                          
*                                                                               
         MVC   QDPT,NDPTDPTE       PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,NDPTDPTA      PASS BACK DAYPART                            
         MVC   DPTNAME,NDPTDES     PASS BACK DESCRIPTION                        
         B     VDPTX                                                            
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL-RESET KEY-MOVE CLIENT CODE IN-RESTART SEARCH                  
* IF CLIENT LEVEL-EXIT ROUTINE-DPT WAS INVALID                                  
*                                                                               
VDPT100  OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   TRAPERR                                                          
*                                                                               
         OC    BCLT,BCLT           CHECK CLIENT LEVEL?                          
         BZ    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),BCLT                                                    
         B     VDPT020                                                          
*                                                                               
VDPT200  DS    0H                  DAYPART EQUATE ENTERED                       
         LA    R3,DAYPARTS         CHECK IN DAYPART TABLE                       
         L     R5,DMCB             A(1 BYTE DAYPART EQUATE FIELD)               
         MVC   WORK(1),0(R5)                                                    
*                                                                               
VDPT205  DS    0H                                                               
         CLI   0(R3),X'FF'         NOT IN TABLE - CHECK RECORDS                 
         BE    VDPT210                                                          
         CLC   WORK(1),0(R3)                                                    
         BE    DPTEOK                                                           
*                                                                               
         LA    R3,10(R3)                                                        
         B     VDPT205                                                          
*                                                                               
DPTEOK   DS    0H                                                               
         MVC   QDPT,0(R3)          PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,0(R3)         PASS BACK DAYPART                            
         MVC   DPTNAME,2(R3)       PASS BACK DESCRIPTION                        
         B     VDPTX                                                            
*                                                                               
VDPT210  DS    0H                  CHECK DAYPART RECORDS                        
         L     R5,DMCB             A(1 BYTE DAYPART EQUATE FIELD)               
         MVC   WORK(1),0(R5)                                                    
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,BAGYMD                                                   
*                                                                               
         CLI   WORK,127            CHECK CLIENT LEVEL DAYPART                   
         BH    *+10                                                             
         MVC   NDPTCLT,BCLT                                                     
*                                                                               
         MVC   NDPTDPTE,WORK       DAYPART EQUATE                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                 
         CLC   KEY(6),KEYSAVE                                                   
         BNE   VDPTX                                                            
*                                                                               
         MVC   QDPT,NDPTDPTE       PASS BACK DAYPART EQUATE                     
         MVC   QDPT2,NDPTDPTA      PASS BACK DAYPART                            
         MVC   DPTNAME,NDPTDES     PASS BACK DESCRIPTION                        
*                                                                               
VDPTX    DS    0H                                                               
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
DAYPARTS DS    0H                                                               
         DC    CL2'D ',CL8'DAYTIME'                                             
         DC    CL2'F ',CL8'FRINGE'                                              
         DC    CL2'P ',CL8'PRIME'                                               
         DC    CL2'K ',CL8'KIDS'                                                
         DC    CL2'S ',CL8'SPORTS'                                              
         DC    CL2'N ',CL8'NEWS'                                                
         DC    CL2'L ',CL8'LATE'                                                
         DC    CL2'Y ',CL8'YOUTH'                                               
         DC    CL2'E ',CL8'EARLY'                                               
         DC    CL2'T ',CL8'TEENS'                                               
         DC    CL2'C ',CL8'CABLE'                                               
         DC    CL2'X ',CL8'SYND'                                                
         DC    CL2'I ',CL8'SPECIAL'                                             
         DC    CL2'O ',CL8'OLYMPICS'                                            
         DC    CL2'W ',CL8'WEEKEND'                                             
         DC    CL2'U ',CL8'UNWIRED'                                             
         DC    CL2'V ',CL8'OVERNITE'                                            
         DC    CL2'W ',CL8'WKNDPM'                                              
         DC    CL2'M ',CL8'WKNDAM'                                              
         DC    CL2'H ',CL8'OTHER'                                               
         DC    CL2'B ',CL8'CBLSPORT'                                            
         DC    CL2'R ',CL8'RADIO'                                               
         DC    CL2'J ',CL8'PROMO-ID'                                            
         DC    CL2'A ',CL8'ACCESS'                                              
         DC    CL2'Q ',CL8'INTRACTV'                                            
         DC    XL1'FF',CL8' '      END OF TABLE                                 
*                                                                               
**********************************************************************          
* CHECK LIMITED ACCESS                                                          
*        ON RETURN:                                                             
*        DMCB+4 - 0     OK                                                      
*        DMCB+4 - X'FF' IF LOCKED                                               
**********************************************************************          
VLACCRT  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'S'           DEFAULT SPOT SYSTEM                          
         CLC   SYSFIL,=C'UNTFIL  ' DID IT COME FROM UNTFILE?                    
         BNE   *+8                                                              
         MVI   BYTE,C'U'                                                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(48),KEY     SAVE AWAY KEY                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),TMPAM                                                   
         MVC   KEY+2(2),TMPCLT                                                  
*                                                                               
         MVI   NFILE,C'S'                                                       
         BRAS  RE,NREAD                                                         
         BNZ   VLACCERR                                                         
         BRAS  RE,NGETREC                                                       
*                                                                               
***         L     RF,AIO                                                        
***         USING CLTHDRD,RF                                                    
***         MVC   BOFFICE,COFFICE     GET OFFICE #                              
***         DROP  RF                                                            
*                                                                               
***         MVI   DMCB+4,0                                                      
***         MVI   ERROR,SECLOCK                                                 
*                                                                               
***         OC    T31CFFD+6(2),T31CFFD+6  TEST ANY SECURITY LIMIT               
***         BZ    VLACCX                                                        
***         CLI   T31CFFD+6,C'+'          TEST FOR MARKET LOCKOUT               
***         BE    VLACCX                                                        
***         CLI   T31CFFD+6,C'$'          TEST OFFICE LIMITS                    
***         BE    VLACC4                                                        
***         CLI   T31CFFD+6,C'*'          TEST OFFICE LOCKOUT                   
***         BE    VLACC5                                                        
***         CLC   T31CFFD+6(2),TMPCLT    ELSE SINGLE CLIENT ACCESS              
***         BNE   VLACCERR                                                      
***         B     VLACCX                                                        
*                                                                               
***VLACC4   DS    0H               * TEST OFFICE LIST SECURITY *                
***         XC    DMCB(8),DMCB                                                  
***         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                   
***         L     RF,CALLOV                                                     
***         GOTO1 (RF),DMCB                                                     
***         CLI   4(R1),X'FF'                                                   
***         BNE   *+6                                                           
***         DC    H'0'                                                          
***         XC    DUB,DUB                                                       
***         LA    R1,DUB                                                        
***         USING OFFICED,R1                                                    
***         MVI   OFCSYS,C'S'         SYSTEM ID                                 
***         MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                             
***         MVC   OFCAGY,AGENCY                                                 
***         MVC   OFCOFC,BOFFICE                                                
*                                                                               
***         L     RF,DMCB                                                       
***         GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                      
***         CLI   0(R1),0                                                       
***         BNE   VLACCERR                                                      
*                                                                               
***         MVI   DMCB+4,0                                                      
***         B     VLACCX                                                        
*                                                                               
**********************                                                          
*                                                                               
*  NEW SECURITY FILTER                                                          
*                                                                               
         LHI   R3,(SVAREA-T31CFFD)+BASETWA2                                     
         A     R3,ATWA                                                          
         USING SVAREA,R3                                                        
*                                                                               
         XC    SVOFFBLK,SVOFFBLK                                                
         LA    R2,SVOFFBLK                                                      
         USING OFFICED,R2                                                       
*                                                                               
         L     R4,AIO                                                           
         USING CLTHDRD,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 CLUNPK,DMCB,(CPROF+6,TMPCLT),OFCCLT                              
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,TMPAM                                                   
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         LA    RF,SVSECRET                                                      
         ST    RF,OFCSECD                                                       
*                                                                               
         MVI   ERROR,SECLOCK                                                    
*                                                                               
*  CALL OFFICER                                                                 
*                                                                               
         XC    DMCB(16),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',SVOFFBLK),ACOMFACS                               
         CLI   0(R1),0                                                          
         BNE   VLACCERR                                                         
*                                                                               
         MVI   DMCB+4,0                                                         
         B     VLACCX                                                           
*                                                                               
         DROP  R2,R3,R4                                                         
**********************                                                          
*                                                                               
VLACC5   CLC   T31CFFD+7(1),BOFFICE   MATCH OFFICE CODE                         
         BE    VLACCX                                                           
*                                                                               
VLACCERR DS    0H                                                               
         MVI   DMCB+4,X'FF'                                                     
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VLACCEX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,ELEMENT                                                      
         MVC   NFILE,BYTE                                                       
*                                                                               
         BRAS  RE,NREAD                                                         
         BRAS  RE,NGETREC                                                       
         MVI   DMCB+4,X'FF'                                                     
         XIT1                                                                   
*                                                                               
VLACCX   DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VLACCEX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,ELEMENT                                                      
         MVC   NFILE,BYTE                                                       
*                                                                               
         BRAS  RE,NREAD                                                         
         BRAS  RE,NGETREC                                                       
VLACCEX  DS    0H                                                               
         XIT1                                                                   
**********************************************************************          
* CHECK TERMINAL ACCESS FOR CLIENT AND PRODUCT                                  
*   SHOULD USE A TABLE IF MORE ARE NEEDED                                       
* THIS IS CALLED THE FIRST TIME BASE GETS CONTROL                               
* THIS IS CALLED AGAIN BY THE OVERLAY IN CASE USER IS TRYING TO                 
*   CHA/DEL IN A LIST                                                           
**********************************************************************          
VTACCRT  NTR1  BASE=*,LABEL=*                                                   
         MVC   AUTHCODE,T31CFFD+12 GET ACCESS CODE                              
*                                                                               
         TM    AUTHCODE+1,X'0F'    X'0F' SECURITY RECORDS ONLY                  
         BZ    TACCX                                                            
*                                                                               
         TM    AUTHCODE,X'FF'       DISPLAY ONLY                                
         BO    TACC200                                                          
*                                                                               
         CLC   =C'CLIENT',CONREC                                                
         BNE   TACC10                                                           
         TM    AUTHCODE,X'40'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC10   CLC   =C'PRODUCT',CONREC                                               
         BNE   TACC20                                                           
         TM    AUTHCODE,X'20'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC20   CLC   =C'MASTER',CONREC                                                
         BNE   TACC30                                                           
         TM    AUTHCODE,X'80'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC30   CLC   =C'MARKET',CONREC                                                
         BNE   TACC40                                                           
         TM    AUTHCODE,X'80'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC40   CLC   =C'REP',CONREC                                                   
         BNE   TACC50                                                           
         TM    AUTHCODE,X'80'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC50   CLC   =C'ADDRESS',CONREC                                               
         BNE   TACC60                                                           
         TM    AUTHCODE,X'80'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC60   CLC   =C'ESTIMATE',CONREC                                              
         BNE   TACC70                                                           
         TM    AUTHCODE,X'10'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC70   CLC   =C'PROGRAM',CONREC                                               
         BE    TACC75                                                           
         CLI   CONREC,X'FF'                                                     
         BNE   TACC80                                                           
         CLC   =C'PROGRAM',CONREC+1 COULD BE AUDIENCE ESTIMATOR                 
         BNE   TACC80                                                           
TACC75   TM    AUTHCODE,X'02'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
TACC80   CLC   =C'DAYPART',CONREC                                               
         BNE   TACCX                                                            
         TM    AUTHCODE,X'08'      ALLOW CHANGE/ADD?                            
         BO    TACCX               YES                                          
         B     TACC200                                                          
*                                                                               
TACC200  DS    0H                  DISPLAY AND LIST ONLY                        
         TM    AUTHCODE+1,X'1F'    NEW FF1F SECURITY FOR I2?                    
         BZ    *+14                                                             
         CLC   =C'I2',CONREC       I2COM RECORD?                                
         BE    TACCX                                                            
*                                                                               
         TM    AUTHCODE,X'F1'       DISPLAY ONLY                                
         BO    TACC300                                                          
*                                                                               
TACC210  CLC   =C'LIS',CONACT                                                   
         BE    TACCX                                                            
         CLC   =C'SEL',CONACT                                                   
         BNE   TACC300                                                          
         CLI   MODE,VALREC         ONLY SELECT FROM LIST                        
         BE    TACC250                                                          
         CLI   MODE,RECDEL         ONLY SELECT FROM LIST                        
         BNE   TACCX                                                            
TACC250  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         GOTO1 ERREX                                                            
TACC300  CLC   =C'DIS',CONACT      DISPLAY ONLY                                 
         BE    TACCX                                                            
TACCERR  MVC   CONACT(8),=C'INVALID ' FAKE OUT GENCON AND GET GENCON            
         MVI   CONACTH+5,7         TO DISPLAY ERROR MESSAGE                     
         OI    CONACTH+6,X'80'     XMIT                                         
TACCX    DS    0H                                                               
EXIT     XIT1                                                                   
         SPACE 1                                                                
***********************************************************************         
* FOR AUDIENCE ESTIMATOR,SEND FIELD NUMBER TO PC UNDER MAP CODE #10   *         
* AT ENTRY, R2 POINTS TO FIELD IN ERROR                               *         
***********************************************************************         
AEFLDNO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   RECNUM,60           TEST REQUEST FROM AE SCREEN/PC               
         BNE   AEFLDNOX                                                         
                                                                                
         TM    1(R2),X'02'         TEST EXTENDED FIELD HEADER                   
         BNO   AEFLDNOX                                                         
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               POINT RF TO NEXT FIELD                       
         SHI   RE,8                BACK UP TO START OF FIELD EXTENSION          
         MVC   BYTE,0(RE)          FIELD NUMBER                                 
                                                                                
         GOTOR RESLIOB             SET RELEASED FLAG FOR GENNEW                 
                                                                                
         L     R3,ALIOB            SEND FIELD NUMBER                            
         USING LIOBD,R3                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAPUT',LIOBD),('LIOTRAW',MAPFLDQ),        *        
               ('LD_CBINQ',BYTE),(L'BYTE,0)                                     
         DROP  R3                                                               
                                                                                
AEFLDNOX J     EXIT                                                             
MAPFLDQ  EQU   10                                                               
         LTORG                                                                  
***********************************************************************         
* VALIDATE AUTHORIZATION NUMBER                                       *         
* ON ENTRY     PARAM 1             HAD BETTER HAVE A(GEND) !!!        *         
* ON ENTRY:    PARAM 2             A(SECURITY BLOCK)                  *         
* ON ENTRY:    AIO                 ADDRESS OF IO TO USE               *         
***********************************************************************         
AUTH     DS    0H                                                               
         NMOD1 0,**AUTH**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
*                                                                               
         L     R4,DMCB+4           SECURITY BLOCK                               
         USING AUTHD,R4                                                         
         XC    PRSNLID,PRSNLID                                                  
         OC    PASSWD,PASSWD       CHANGED BY FIELD                             
         BZ    AUTHX                                                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT0REC,R6                                                        
         MVI   CT0KTYP,CT0KTEQU    RECORD TYPE '0'                              
         MVC   CT0KAGY,SECRAGY     GET SECURITY AGENCY                          
         CLC   SECRAGY,SPACES                                                   
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY                                                   
         MVC   CT0KNUM,PASSWD      PERSON AUTH NUMBER                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                                                       
         BNE   AUTHX                                                            
         LA    RE,28(R6)                                                        
*                                                                               
AUTH10   DS    0H                                                               
         CLC   =X'C30A',0(RE)      NEW SECURITY - PERSON ELEMENT                
         BE    AUTH20                                                           
         LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   AUTH10                                                           
         B     AUTHX                                                            
*                                                                               
AUTH20   MVC   PRSNLID,2(RE)       PERSONAL ID                                  
AUTHX    B     XIT2                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY INITIALIZATION                                                          
* ON ENTRY     PARAM 1             HAD BETTER HAVE A(GEND) !!!                  
* ON ENTRY:    PARAM 2             A(PFKEY VAL. TABLE) OR ZEROS                 
* WALTER -- TAKE NOTE  --                                                       
* BRANCHING TO ADDRESSES COVERED BY BASER7 IS HAZARDOUS !!!                     
***********************************************************************         
PFKYINIT DS    0H                                                               
         NMOD1 0,**PFIN**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
*                                                                               
         ICM   R3,7,5(R1)          IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BNE   INITX               TAKE DUMMY ERROR EXIT FOR GOAGAIN            
* THIS CODE REPLICATED FROM UNREACHABLE INSTRUCTION IN DUMMYERR                 
         MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
         L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T31CFFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
INIT10   DS    0H                                                               
*&&DO                                                                           
INIT10   MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
*                                                                               
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
*                                                                               
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
*                                                                               
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
*                                                                               
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
*                                                                               
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT20                                                           
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT30                                                           
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT20                                                           
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT30                                                           
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT30                                                           
*                                                                               
         TM    CTLRFLG1,CF1NOCLR       DON'T CLEAR APPLICATION STORAGE?         
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1NOCLR  YES, BUT CLEAR IT NEXT TIME             
         B     INIT30                                                           
*                                                                               
INIT20   LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SHI   RF,3520+64          L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
*                                                                               
INIT30   MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
*&&                                                                             
INITX    B     XIT2                                                             
         EJECT                                                                  
*              LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                           
*                                                                               
*                                  P1  BYTES 1-3 = A(PFKEY VAL. TABLE)          
PFVAL    NTR1                                                                   
         CLI   PFAID,0             USER HIT ENTER?                              
         BE    NO2                 YES                                          
*                                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BE    PFERR2                                                           
         CLC   PFAID,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO2                                                              
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES2                IF RETURNS, RETURN CC EQUAL                  
         EJECT                                                                  
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         TM    PFTSTAT2,PFTCLRKY   DO WE CLEAR THE PFKEY?                       
         BNZ   *+8                 NO                                           
         MVI   PFAID,0             CLEAR PFKEY FOR NEXT SCREEN                  
*                                                                               
         L     R1,SYSPARMS         RE=A(TRANSLATOR I/O BLOCK)                   
         L     RE,0(R1)                                                         
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    *+12                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         B     DUMYERR2            TAKE DUMMY ERROR XIT FOR GOAGAIN             
*                                                                               
         CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BE    PFI8                                                             
         MVC   CONREC,PFTREC       MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFIX                                                             
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
PFIX     B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
*              R3=A(PFKEY TABLE)                                                
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         OC    8(3,R2),SPACES                                                   
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         CLC   PFTSEL,8(R2)        MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
*                                                                               
TSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    MVC   8(3,R2),SPACES      FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    RE,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFAID,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    RE,RA                                                            
         STCM  RF,3,CURDISP                                                     
*        STH   RE,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT2                                                             
         EJECT                                                                  
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR               
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN                   
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL          
* BE CALLED TO RESTORE THE SCREEN.                                              
*                                                                               
CPROG    NTR1                                                                   
*        MVC   CALLSTCK,TWASCR     SAVE SCREEN NUMBER ON STACK                  
*        MVI   CALLSP,1                                                         
         LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
         AHI   R1,TWAENDLQ-2       SPSFM HAS NO ROOM IN ITS SAVE AREA           
         MVC   0(1,R1),TWASCR        FOR CALLSTCK AND CALLSP                    
         MVI   1(R1),1                                                          
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),ASTARTSV)                                      
*                                                                               
         B     XIT2                                                             
         EJECT                                                                  
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*                                                                               
RPROG    NTR1                                                                   
*        CLI   CALLSP,0                                                         
         LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
         AHI   R1,TWAENDLQ-2         SPSFM HAS NO ROOM IN ITS SAVE AREA         
         CLI   1(R1),0                                                          
         BE    PFERR2              ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LHI   RF,TWAMXLEN                                                      
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
*^^NOP   GOTO1 GETTWA,DMCB,((R2),ASTARTSV)                                      
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
         L     RE,ATIA             RESTORE SAVE AREA FROM TIA                   
         LH    RF,=Y(SYSDEND-SVSTART)                                           
         L     R0,ASTARTSV                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        MVI   CALLSP,0            DECREMENT STACK POINTER                      
*        MVC   TWASCR,CALLSTCK     EXTRACT TWASCR                               
         LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
         AHI   R1,TWAENDLQ-2         SPSFM HAS NO ROOM IN ITS SAVE AREA         
         MVI   1(R1),0               FOR CALLSTCK AND CALLSP                    
         MVC   TWASCR,0(R1)        EXTRACT TWASCR                               
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRNSTAT1,RETURND     SET THAT RETPROG HAS BEEN CALLED            
*                                                                               
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                   
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
         LA    R6,TMPSTOR                                                       
         USING TMPSTORD,R6                                                      
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,3,CURDISP                                                     
*        LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR2                                                           
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         ZIC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TO WORK                                 
         AR    R2,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R2),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         LA    R2,1(R2)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT2                                                             
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         SPACE 5                                                                
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     NO2                 RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     YES2                                                             
         DROP  R6,RA                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ/WRITE TEMPSTR PAGES                                           
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    BIT SETTINGS/PAGE NUMBER                     
*              PARAM 1  BYTES 1-3  READ/WRITE ADDRESS                           
***********************************************************************         
GETTWA   NTR1                                                                   
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
*                                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA10                                                           
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA10                                                           
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA10   NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT2                                                             
         DC    H'0'                                                             
         DROP  RA                                                               
*                                                                               
*PFERR2   MVI   ERROR,ERINVPFK      INVALID PF KEY                              
PFERR2   MVI   ERROR,INVALID       INVALID PF KEY                               
         TM    TRNSTAT1,NOVALERR    NO ERREX ON VALI RTNS                       
         BZ    *+12                                                             
         OI    TRNSTAT1,BASERR      SET FLAG FOR ERROR TO APPL                  
         B     XIT2                                                             
         GOTO1 ERREX               HOPEFULLY, NEVER TO RETURN                   
         B     XIT2                ELSE GENCON WAS SLAVED                       
*                                                                               
DUMYERR2 MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
         L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T31CFFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
         DROP  RA                                                               
*                                                                               
YES2     SR    RC,RC               SET CC EQ                                    
NO2      LTR   RC,RC               SET CC NEQ                                   
XIT2     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                **** MYWORK AREA ***                           
MYWORKD  DS    CL200                                                            
TMPAM    DS    XL1                                                              
TMPCLT   DS    XL2                                                              
*                                                                               
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
REPHDRD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
INDHDRD  DSECT                                                                  
       ++INCLUDE NEGENIND                                                       
         EJECT                                                                  
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDLINKD                                                        
         EJECT                                                                  
       ++INCLUDE NEGENDPT                                                       
         EJECT                                                                  
       ++INCLUDE GEGENTOK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONHEAD-64                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
*  SECOND SAVE STORAGE AREA                                                     
*                                                                               
         ORG   CONHEADH-64+BASETWA2                                             
SVAREA   DS    0H                                                               
SVSECRET DS    CL1024                                                           
SVOFFBLK DS    CL100               OFICCER BLOCK                                
         SPACE 3                                                                
*                                                                               
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
*                                                                               
T31CFFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T31CFFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
CALLSP   DS    X                   CALL ROUTINE STACK POINTER                   
CALLSTK  DS    XL9                 STACK (LIST OF OVERLAYS)                     
LASTOV   DS    X                   LAST OVERLAY                                 
SVLIST   DS    XL188               LISTDIR SAVE AREA                            
AUTHCODE DS    H                   TERMINAL ACCESS CODE                         
TMPSTOR  DS    XL58                                                             
*              DSECT TO COVER THE EXTRA TEMPORARY STORAGE                       
*                                                                               
TMPSTORD DSECT                                                                  
TMPKEYH  DS    XL8                 DUMMY KEY FIELD HEADER FOR GENCON            
TMPKEY   DS    CL50                DUMMY KEY FIELD                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'245NESFM00   05/13/20'                                      
         END                                                                    
