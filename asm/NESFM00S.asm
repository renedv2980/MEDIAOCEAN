*          DATA SET NESFM00S   AT LEVEL 117 AS OF 05/01/02                      
*PHASE T31C00A,+0                                                               
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T31C00 - NETWORK SUPER FILE MAINT CONTROLLER'                   
T31C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 2000,T31C00,RR=R2                                                
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T31C00+4096,R7                                                   
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         LA    RF,2000             CLEAR SELECTED STORAGE                       
         SLL   RF,3                                                             
         XCEF                                                                   
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
         AH    R9,=H'6000'         GRABBING 3 2000 BYTE I/O AREAS               
         LA    R9,24(R9)           NEED SPACE FOR 3 8BYTE LABELS                
         USING SYSD,R9                                                          
         ST    R9,ASUBSYSD                                                      
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         SPACE 1                                                                
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
*                                                                               
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,VCOUNT                                                        
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
         MVI   NTWA,1                                                           
*        MVC   LSVTWA0,=X'0900'                                                 
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'20'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'     USUALLY NETFILE                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         MVC   LWORK,=F'16000'     WE TOOK 16000 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9031C00'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R0,SVSTART          SET SAVED STORAGE START                      
         ST    R0,ASTARTSV                                                      
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
         CLC   =C'DIS',CONREC      FOR DISTRIBUTION RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'BIL',CONREC      FOR BILL FORUMLA RECORDS                     
         BE    SETSPOT                                                          
         CLC   =C'EST',CONREC      FOR BILL FORUMLA RECORDS                     
         BE    SETSPOT                                                          
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
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
*                                                                               
SYS11    LA    R1,SVSTART                                                       
         ST    R1,ASTARTSV                                                      
*                                                                               
SYS15    DS    0H                                                               
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
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
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
         B     XIT                                                              
         EJECT                                                                  
         SPACE                                                                  
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
         MVC   QCLT(3),0(R1)                                                    
         B     CLT3                                                             
*                                                                               
VCLI     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
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
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         SPACE 1                                                                
* SAVE USER DEFINITION LIST                                                     
         XC    SVP1USER(SVULNQ),SVP1USER                                        
         MVC   SVP1USER(SVULNQ),CPU1  MOVE ALL INFO TO SAVED STORAGE            
*                                                                               
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
*                                                                               
         MVI   ERROR,SECLOCK                                                    
         OC    T31CFFD+6(2),T31CFFD+6  TEST ANY SECURITY LIMIT                  
         BZ    CLTX                                                             
         CLI   T31CFFD+6,C'+'          TEST FOR MARKET LOCKOUT                  
         BE    CLTX                                                             
         CLI   T31CFFD+6,C'$'          TEST OFFICE LIMITS                       
         BE    CLT4                                                             
         CLI   T31CFFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    CLT5                                                             
         CLC   T31CFFD+6(2),BCLT       ELSE SINGLE CLIENT ACCESS                
         BNE   CLIERR                                                           
         B     CLTX                                                             
*                                                                               
CLT4     DS    0H               * TEST OFFICE LIST SECURITY *                   
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
CLT5     CLC   T31CFFD+7(1),COFFICE   MATCH OFFICE CODE                         
         BNE   CLIERR                                                           
CLTX     BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R6                                                               
         SPACE                                                                  
*                                                                               
CLIERR   B     TRAPERR                                                          
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
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         MVC   ESTNAME,EDESC                                                    
         MVC   QEST,NFLD                                                        
         BAS   RE,RESET                                                         
         B     XIT                                                              
         DROP  R4,R6                                                            
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
****************************                                                    
* VALIDATE A DAYPART       *                                                    
*    OUTPUT - DPTNAME,QDPT *                                                    
****************************                                                    
VDPT     NTR1                                                                   
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
         SPACE                                                                  
*&&DO                                                                           
DAYPARTS DS    0H                                                               
         DC    CL1'D',CL8'DAYTIME'                                              
         DC    CL1'F',CL8'FRINGE'                                               
         DC    CL1'P',CL8'PRIME'                                                
         DC    CL1'K',CL8'KIDS'                                                 
         DC    CL1'S',CL8'SPORTS'                                               
         DC    CL1'N',CL8'NEWS'                                                 
         DC    CL1'L',CL8'LATE'                                                 
         DC    CL1'Y',CL8'YOUTH'                                                
         DC    CL1'E',CL8'EARLY'                                                
         DC    CL1'T',CL8'TEENS'                                                
         DC    CL1'C',CL8'CABLE'                                                
         DC    CL1'X',CL8'SYND'                                                 
         DC    CL1'I',CL8'SPECIAL'                                              
         DC    CL1'O',CL8'OLYMPICS'                                             
         DC    CL1'W',CL8'WEEKEND'                                              
         DC    CL1'U',CL8'UNWIRED'                                              
         DC    CL1'V',CL8'OVERNITE'                                             
         DC    CL1'W',CL8'WKNDPM'                                               
         DC    CL1'M',CL8'WKNDAM'                                               
         DC    CL1'H',CL8'OTHER'                                                
         DC    CL1'B',CL8'CBLSPORT'                                             
         DC    CL1'R',CL8'RADIO'                                                
         DC    CL1'J',CL8'PROMO-ID'                                             
         DC    CL1'A',CL8'ACCESS'                                               
         DC    CL1'Q',CL8'INTRACTV'                                             
         DC    XL1'FF',CL8' '      END OF TABLE                                 
*&&                                                                             
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
         CLI   0(R3),C':'                                                       
         BE    VPRO20                                                           
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
         GOTO1 =A(VTACCRT),RR=RELO                                              
         B     XIT                                                              
         SPACE 1                                                                
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
         CLC   =C'DIS',CONREC      FOR DISTRIBUTION RECORDS                     
         BER   RE                                                               
         CLC   =C'BIL',CONREC      FOR BILL FORMULA RECORDS                     
         BER   RE                                                               
         CLC   =C'PRG',CONREC      FOR PROPGRAM GROUP RECORDS                   
         BER   RE                                                               
         CLC   =C'EST',CONREC      FOR PROPGRAM GROUP RECORDS                   
         BER   RE                                                               
         MVC   LKEY,=H'20'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'     USUALLY NETFILE                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVI   USEIO,0                                                          
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
         DC    X'D8'               NAD DEFINITION                               
         DC    X'E6'               NAD PROGRAM                                  
         DC    X'E8'               NAD UNIVERSE                                 
         DC    X'F7'               PROGRAM                                      
         DC    X'F9'               UNIVERSE                                     
RETURNS  EQU   (*-RETTAB)/L'RETTAB                                              
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
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
         DC    X'01',C'INDEX   ',AL1(14),X'00C2'                                
         DC    X'01',C'AOR     ',AL1(17),X'00C2'                                
         DC    X'01',C'PGRDEF  ',AL1(18),X'00C2'                                
         DC    X'01',C'PGROUP  ',AL1(19),X'00C2'                                
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
         DC    X'01',C'BILLF   ',AL1(32),X'00C2'                                
         DC    X'01',C'REASON  ',AL1(33),X'00C2'                                
         DC    X'01',C'ADDRESS ',AL1(34),X'00C2'                                
         DC    X'01',C'SGDEF   ',AL1(35),X'00C2'                                
         DC    X'01',C'SGROUP  ',AL1(36),X'00C2'                                
         DC    X'01',C'DISTRIB ',AL1(37),X'00C2'                                
         DC    X'01',C'SGASSIGN',AL1(38),X'00C2'                                
         DC    X'01',C'TSUPP   ',AL1(39),X'00C2'                                
         DC    X'01',C'PRGGROUP',AL1(40),X'00C2'                                
         DC    X'01',C'ESTIMATE',AL1(41),X'00C2'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
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
         SPACE 1                                                                
         DC    X'03',AL1(07,01),X'C1010000C0',C'    '  CLIENT   MAINT           
         DC    X'03',AL1(07,10),X'C2010000D0',C'PCPC'           LIST            
         DC    X'03',AL1(08,01),X'C3020000C0',C'    '  PRODUCT  MAINT           
         DC    X'03',AL1(08,10),X'C4020000D0',C'PCPC'           LIST            
         DC    X'03',AL1(09,01),X'C7050000C0',C'    '  MARKET   MAINT           
         DC    X'03',AL1(09,10),X'C8050000C0',C'MANI'           LIST            
         DC    X'03',AL1(10,01),X'C9060000C0',C'    '  STATION  MAINT           
         DC    X'03',AL1(10,10),X'CA060000C0',C'STNI'           LIST            
         DC    X'03',AL1(11,01),X'F7130000C0',C'    '  PROGRAM  MAINT           
         DC    X'03',AL1(11,10),X'F8130000C0',C'PGNI'           LIST            
         DC    X'03',AL1(11,12),X'F813000078',C'PGFM'           REPORT          
         DC    X'03',AL1(12,01),X'F9140000C0',C'    '  UNIVERSE MAINT           
         DC    X'03',AL1(12,10),X'FA140000C0',C'UNNI'           LIST            
         DC    X'03',AL1(02,01),X'F1100000C0',C'    '  COM      MAINT           
         DC    X'03',AL1(02,10),X'F2100000C0',C'ELFL'           LIST            
         DC    X'03',AL1(03,01),X'F3200000C0',C'    '  INTG     MAINT           
         DC    X'03',AL1(03,10),X'F4200000C0',C'ITFI'           LIST            
         DC    X'03',AL1(04,01),X'F5120000C0',C'    '  REP      MAINT           
         DC    X'03',AL1(04,10),X'F6120000C0',C'RPFL'           LIST            
         DC    X'03',AL1(13,01),X'FC1C0000C0',C'    '  SPB      MAINT           
         DC    X'03',AL1(13,10),X'FD1C0000C0',C'SPNI'           LIST            
         DC    X'03',AL1(14,01),X'CB150000C0',C'    '  INDX     MAINT           
         DC    X'03',AL1(14,10),X'CC150000C0',C'IXNI'           LIST            
         DC    X'03',AL1(17,01),X'D0110000C0',C'    '  AOR      MAINT           
         DC    X'03',AL1(17,10),X'D1110000C0',C'AONA'           LIST            
         DC    X'03',AL1(17,12),X'D111000058',C'AONA'           REPORT          
         DC    X'03',AL1(18,01),X'D2410000C0',C'    '  PGRDEF   MAINT           
         DC    X'03',AL1(18,10),X'D4410000D0',C'PCPC'           LIST            
         DC    X'03',AL1(19,01),X'D3400000C0',C'    '  PGROUP   MAINT           
         DC    X'03',AL1(19,10),X'D5400000D0',C'PCPC'           LIST            
         DC    X'03',AL1(19,12),X'D540000038',C'PGPG'           REPORT          
         DC    X'03',AL1(20,01),X'CD210000C0',C'    '  PGEST    MAINT           
         DC    X'03',AL1(20,10),X'CE210000C0',C'PGPG'           LIST            
         DC    X'03',AL1(20,12),X'CE21000058',C'PGPG'           REPORT          
         DC    X'03',AL1(21,01),X'D6220000C0',C'    '  GFEST    MAINT           
         DC    X'03',AL1(21,12),X'D722000078',C'    '           REPORT          
         DC    X'03',AL1(21,10),X'D7220000C0',C'    '           LISTT           
         DC    X'03',AL1(22,01),X'E8190000C0',C'    '  NUNIV    MAINT           
         DC    X'03',AL1(22,10),X'E9190000C0',C'NUNI'           LIST            
         DC    X'03',AL1(23,01),X'D8170000C0',C'    '  NADDEF   MAINT           
         DC    X'03',AL1(23,10),X'D9170000C0',C'    '           LIST            
         DC    X'03',AL1(24,01),X'E6180000C0',C'    '  NPROG    MAINT           
         DC    X'03',AL1(24,10),X'E7180000C0',C'    '           LIST            
         DC    X'03',AL1(25,01),X'DA310000C0',C'    '  CGRDEF   MAINT           
         DC    X'03',AL1(25,10),X'DB310000D0',C'PCPC'           LIST            
         DC    X'03',AL1(26,01),X'DC300000C0',C'    '  CGROUP   MAINT           
         DC    X'03',AL1(26,10),X'DD300000D0',C'PCPC'           LIST            
         DC    X'03',AL1(26,12),X'DD30000038',C'CGCG'           REPORT          
         DC    X'03',AL1(27,01),X'E0320000C1',C'    '  CGASSIGN MAINT           
         DC    X'03',AL1(28,01),X'E4320000C1',C'    '  PGASSIGN MAINT           
         DC    X'03',AL1(29,10),X'EB330000D0',C'    '  CLRST    LIST            
         DC    X'03',AL1(30,01),X'EC340000D0',C'    '  UDEFD    LIST            
         DC    X'03',AL1(30,10),X'ED350000D0',C'    '  UDEFL    LIST            
         DC    X'03',AL1(31,01),X'EA360000D0',C'    '  PSR      MAINT           
         DC    X'03',AL1(31,10),X'EE360000D0',C'    '  PSR      LIST            
         DC    X'03',AL1(32,16),X'9F3B000081',C'    '  BILLF    MAINT           
         DC    X'03',AL1(33,01),X'B1240000C0',C'    '  REASD    MAINT           
         DC    X'03',AL1(33,10),X'B2240000C0',C'    '           LIST            
         DC    X'03',AL1(34,01),X'B3250000C0',C'    '  ADDREC   MAINT           
         DC    X'03',AL1(34,10),X'B4250000C0',C'ADNI'           LIST            
         DC    X'03',AL1(35,01),X'B5260000C0',C'    '  SGDEF    MAINT           
         DC    X'03',AL1(35,10),X'B6260000C0',C'    '           LIST            
         DC    X'03',AL1(36,01),X'B7270000C0',C'    '  SGROUP   MAINT           
         DC    X'03',AL1(36,10),X'B8270000C0',C'    '           LIST            
         DC    X'03',AL1(36,04),X'B7270000C0',C'    '           DELETE          
         DC    X'03',AL1(37,01),X'B9280000C0',C'    '  DIST     MAINT           
         DC    X'03',AL1(37,10),X'BA280000C0',C'    '           LIST            
         DC    X'03',AL1(38,01),X'BB290000C0',C'    '  SGASSIGN MAINT           
         DC    X'03',AL1(39,01),X'BC370000C0',C'    '  TSUPP    MAINT           
         DC    X'03',AL1(39,10),X'BD370000C0',C'TSNI'           LIST            
         DC    X'03',AL1(40,01),X'CF360000C0',C'    '  PRGGROUP MAINT           
         DC    X'03',AL1(40,10),X'C6360000C0',C'    '           LIST            
         DC    X'03',AL1(40,12),X'C636000070',C'    '           REPORT          
         DC    X'03',AL1(41,01),X'BE030000C0',C'    '  ESTIMATE MAINT           
         DC    X'03',AL1(41,10),X'BF040000C0',C'    '           LIST            
         DC    X'03',AL1(41,07),X'B0070000C0',C'    '           COPY            
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
DAYPARTS DS    0H                                                               
         DC    CL1'D',CL8'DAYTIME'                                              
         DC    CL1'F',CL8'FRINGE'                                               
         DC    CL1'P',CL8'PRIME'                                                
         DC    CL1'K',CL8'KIDS'                                                 
         DC    CL1'S',CL8'SPORTS'                                               
         DC    CL1'N',CL8'NEWS'                                                 
         DC    CL1'L',CL8'LATE'                                                 
         DC    CL1'Y',CL8'YOUTH'                                                
         DC    CL1'E',CL8'EARLY'                                                
         DC    CL1'T',CL8'TEENS'                                                
         DC    CL1'C',CL8'CABLE'                                                
         DC    CL1'X',CL8'SYND'                                                 
         DC    CL1'I',CL8'SPECIAL'                                              
         DC    CL1'O',CL8'OLYMPICS'                                             
         DC    CL1'W',CL8'WEEKEND'                                              
         DC    CL1'U',CL8'UNWIRED'                                              
         DC    CL1'V',CL8'OVERNITE'                                             
         DC    CL1'W',CL8'WKNDPM'                                               
         DC    CL1'M',CL8'WKNDAM'                                               
         DC    CL1'H',CL8'OTHER'                                                
         DC    CL1'B',CL8'CBLSPORT'                                             
         DC    CL1'R',CL8'RADIO'                                                
         DC    CL1'J',CL8'PROMO-ID'                                             
         DC    CL1'A',CL8'ACCESS'                                               
         DC    CL1'Q',CL8'INTRACTV'                                             
         DC    XL1'FF',CL8' '      END OF TABLE                                 
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
         BNE   TACCX                                                            
         TM    AUTHCODE,X'02'       DISPLAY AND LIST ONLY                       
         BO    TACC200                                                          
         B     TACCX                                                            
*                                                                               
TACC200  DS    0H                  DISPLAY AND LIST ONLY                        
         TM    AUTHCODE,X'F1'       DISPLAY ONLY                                
         BO    TACC300                                                          
         CLC   =C'LIS',CONACT                                                   
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
         XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
         SPACE 2                                                                
MYWORKD  DS    CL200                                                            
*                                **** MYWORK AREA ***                           
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
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONHEAD-64                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T31CFFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T31CFFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
CALLSP   DS    X                   CALL ROUTINE STACK POINTER                   
CALLSTK  DS    XL9                 STACK (LIST OF OVERLAYS)                     
LASTOV   DS    X                   LAST OVERLAY                                 
SVLIST   DS    XL188               LISTDIR SAVE AREA                            
AUTHCODE DS    H                   TERMINAL ACCESS CODE                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117NESFM00S  05/01/02'                                      
         END                                                                    
