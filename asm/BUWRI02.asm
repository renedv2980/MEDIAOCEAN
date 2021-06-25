*          DATA SET BUWRI02    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T50302A,*                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T50302 - BUDGET WRITER REQUEST VALIDATION/REPORT PROGRAM    *         
*                                                                     *         
*  COMMENTS: VALIDATES BUDGET REPORT REQUESTS, GENERATES DRIVER INPUT *         
*            CODE, AND USES DRIVER TO GENERATE REPORT.                *         
*                                                                     *         
*  CALLED FROM: BUWRI00, WHICH CALLS GENCON, WHICH CALLS THIS.        *         
*                                                                     *         
*  CALLS TO:    NODIO, DATAMGR                                        *         
*                                                                     *         
*  INPUTS: SCREEN BUWRIF2 (T503F2) -- MAINTENANCE                     *         
*                                                                     *         
*  OUTPUTS: DRIVER-PRODUCED REPORT                                    *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH                                *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - BUDGET RECORD                                         *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - TWA                                                   *         
*          R8 - THIRD BASE                                            *         
*          R9 - SYSD                                                  *         
*          RA - SECOND BASE                                           *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'T50302 - BUDGET WRITER REQUEST PROGRAM'                         
T50302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**WR02**,RA,R8,RR=R3                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO             SAVE PROGRAM RELOCATION FACTOR               
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
         LA    R0,BUFRSTEL-BUKEY   DISP TO FIRST BUDGET ELEMENT                 
         STH   R0,DATADISP         FOR GETEL MACRO                              
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    VR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALIZATION FOR PRINTED REPORT                                             
*                                                                               
PR       L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         LA    R1,HEADING          HEADING LINES FOR REPORT                     
         ST    R1,SPECS                                                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK                                                      
         L     R1,ABOX                                                          
         ST    R1,AMYBOX           HANG ON TO A(BOX AREA)                       
         MVI   USEHDHK,C'Y'        OK TO USE HEADHOOK                           
         DROP  RF                                                               
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9050303'                                           
         GOTO1 CALLOV,DMCB         DRIVER GLOBAL STORAGE OVERLAY                
         L     R6,DMCB                                                          
         ST    R6,AGLOBALS                                                      
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A39'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADRONE,DMCB         A(DRONE)                                     
*                                                                               
         LA    R1,DTYPETAB                                                      
         XCEF  (R1),1500           CLEAR DATA TYPE TABLE                        
         ST    R1,ADTYPE           A(BEGINNING OF DATA TYPE TABLE)              
*                                                                               
         XC    DRGEN,DRGEN         CLEAR DRONE BLOCK                            
         MVI   DRWHO,DRABCWHO      INITIALIZE DRONE                             
         MVI   DRACTION,DRINIT                                                  
         MVC   DRCOMFAC,ACOMFACS                                                
         L     R0,=A(BUFFER)                                                    
         A     R0,RELO                                                          
         ST    R0,DRSTBUF                                                       
         AH    R0,=H'4096'                                                      
         ST    R0,DRENDBUF                                                      
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0           DIE IF ERROR OFFLINE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
VR       DS    0H                                                               
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         LA    R2,WRICLIH          CLIENT FIELD                                 
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES           ZERO FILL OUTPUT                             
         GOTO1 VFVAL                                                            
*                                                                               
         MVI   ERROR,MISSING       FIELD IS REQUIRED                            
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         XC    CLTVALS,CLTVALS     CLEAR PREVIOUS VALUES                        
         XC    PRDVALS,PRDVALS                                                  
         XC    PLANVALS,PLANVALS                                                
         XC    OUTVALS,OUTVALS                                                  
*                                                                               
         MVI   ERROR,INVALID                                                    
         MVC   CLTCODE,FLD         SAVE INPUT CODE                              
*                                                                               
         CLC   SIGNON,=CL8'AYJW'   TEST JWT USER ON AYNY                        
         BNE   *+14                                                             
         CLC   CLTCODE,=C'DBS'     ONLY VALID TO VIEW CLT DBS                   
         BNE   TRAPERR                                                          
*                                                                               
         BAS   RE,SETKEY           SET NODIO KEY                                
         LA    R1,VALCLIHK         IN-LINE NODIO HOOK                           
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VALPRD                                                           
         L     R2,FADDR                                                         
         B     NODERR                                                           
         EJECT                                                                  
* NODIO HOOK ROUTINE FOR CLIENT                                                 
*                                                                               
VALCLIHK ST    RE,SAVERE                                                        
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BE    VALCLIHX                                                         
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNE   VALCLIHX                                                         
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCCLT     TEST FOR CLIENT RECORD                       
         BZ    VALCLIHX                                                         
         CLI   NDLEV,1             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
         XC    WRICLID,WRICLID     CLEAR CLIENT NAME                            
         MVC   WRICLID(L'CLTNAM),CLTNAM                                         
         OI    WRICLIDH+6,X'80'    XMIT                                         
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VALCLIHX                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         MVC   GLABCMT,CLTTYPE     FISCAL MONTH TYPE                            
         DROP  R4,R6                                                            
*                                                                               
VALCLIHX L     RE,SAVERE                                                        
         BR    RE                  RETURN TO NODIO                              
         EJECT                                                                  
* EDIT AND VALIDATE PRODUCT                                                     
*                                                                               
VALPRD   LA    R2,WRIPRDH          PRODUCT FIELD                                
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES           ZERO FILL OUTPUT                             
         GOTO1 VFVAL                                                            
*                                                                               
         MVI   ERROR,MISSING       FIELD IS REQUIRED                            
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         XC    PRDVALS,PRDVALS     CLEAR PREVIOUS VALUES                        
         XC    PLANVALS,PLANVALS                                                
         XC    OUTVALS,OUTVALS                                                  
*                                                                               
         MVC   PRDCODE,FLD         SAVE INPUT CODE                              
         BAS   RE,SETKEY           SET NODIO KEY                                
         LA    R1,VALPRDHK         IN-LINE NODIO HOOK                           
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VALPLAN                                                          
         L     R2,FADDR                                                         
         B     NODERR                                                           
*                                                                               
* NODIO HOOK ROUTINE FOR PRODUCT                                                
*                                                                               
VALPRDHK ST    RE,SAVERE                                                        
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BE    VALPRDHX                                                         
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNE   VALPRDHX                                                         
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPRO     TEST FOR PRODUCT RECORD                      
         BZ    VALPRDHX                                                         
         CLI   NDLEV,2             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
         MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
         XC    WRIPRDD,WRIPRDD     CLEAR PRODUCT NAME                           
         MVC   WRIPRDD(L'PRDNAM),PRDNAM                                         
         OI    WRIPRDDH+6,X'80'    XMIT                                         
*                                                                               
VALPRDHX L     RE,SAVERE                                                        
         BR    RE                  RETURN TO NODIO                              
         DROP  R4                                                               
         EJECT                                                                  
* EDIT AND VALIDATE PLAN CODE                                                   
*                                                                               
VALPLAN  LA    R2,WRIPLANH         PLAN FIELD                                   
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
*                                                                               
         MVI   ERROR,MISSING       FIELD IS REQUIRED                            
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         XC    PLANVALS,PLANVALS   CLEAR PREVIOUS VALUES                        
         XC    OUTVALS,OUTVALS                                                  
*                                                                               
         MVC   PLANCODE,FLD                                                     
*                                                                               
         BAS   RE,SETKEY                                                        
         LA    R1,VALPLAHK         IN-LINE HOOK                                 
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+12                                                             
         L     R2,FADDR                                                         
         B     NODERR                                                           
*                                                                               
         MVI   ERROR,NOOUTERR                                                   
         OC    PLANCNT,PLANCNT     TEST FOR ANY OUTLINES                        
         BZ    TRAPERR             NO-STOP REPORT                               
*                                                                               
         OI    DRFLAGI,X'80'       INPUT ARGUMENTS                              
         MVC   DRTYPEI,=X'C30001'                                               
         ZIC   R3,PLANLOW          COMPUTE WIDTH OF ROWS                        
         BCTR  R3,0                                                             
         MH    R3,=H'3'                                                         
         LA    R3,20(R3)                                                        
         LR    R1,R3                                                            
         LA    R1,2(R1)            TOTAL WIDTH SO FAR                           
         STH   R1,CURWIDTH         HANG ON TO THIS                              
         STC   R3,DRLENI                                                        
         MVC   DRRTNI,=C'ABCROWS '                                              
*                                                                               
         OI    DRFLAGO,X'80'       OUTPUT ARGUMENTS                             
         MVC   DRTYPEO,=X'C300'                                                 
         MVC   DRLENO,DRLENI                                                    
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VALOUTL                                                          
*                                                                               
         MVI   DRACTION,DRGENROW   BUILD ROW ELEMENTS                           
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0           DIE IF ERROR OFFLINE                         
         BE    VALOUTL                                                          
         DC    H'0'                                                             
         EJECT                                                                  
* NODIO HOOK ROUTINE FOR PLAN                                                   
*                                                                               
VALPLAHK ST    RE,SAVERE                                                        
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BE    VALPLAHX                                                         
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNE   VALPLAHX                                                         
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPLN     TEST FOR PLAN RECORD                         
         BZ    VALPLAHX                                                         
         CLI   NDLEV,3             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
         MVC   SVPARKEY,NODKEY     SAVE NODAL KEY                               
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   NEXTNODE,NDLVNOD2   EXTRACT NODE ESTABLISHED BY PLAN             
         MVI   NEXTLEV,1           INITIALIZE NEXT OUTLINE LEVEL TO 1           
         MVC   SVPLNVAL,PLANVALS                                                
         MVC   PLANKEY,NDLVKEY     SAVE NODAL PLAN KEY                          
         XC    WRIPLAD,WRIPLAD                                                  
         MVC   WRIPLAD(L'PLANNAM),PLANNAM                                       
         OI    WRIPLADH+6,X'80'                                                 
*                                                                               
VALPLAHX L     RE,SAVERE           RETURN TO NODIO                              
         BR    RE                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
* EDIT AND VALIDATE OUTLINE CODE(S)                                             
*                                                                               
VALOUTL  XC    WRIOUTD,WRIOUTD     CLEAR OUTLINE NAME FIELD                     
         MVC   WORK,BLANKS                                                      
         XC    OUTVALS,OUTVALS     CLEAR PREVIOUS OUTLINE VALUES                
*                                                                               
         LA    R2,WRIOUTLH         OUTLINE FIELD                                
         CLI   5(R2),0             TEST OUTLINE(S) GIVEN                        
         BE    VALOUTLX            NO -- VALIDATE NEXT FIELD                    
*                                                                               
         XCEF  SCANBLK,512                                                      
         GOTO1 SCANNER,DMCB,(R2),SCANBLK,C',=- '                                
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         CLI   DMCB+4,3                                                         
         BL    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         XC    DMCB,DMCB                                                        
*                                                                               
         LA    R4,SCANBLK          A(SCANNER BLOCK)                             
         XC    OUTCOD,OUTCOD                                                    
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTCOD(0),12(R4)    START OUTLINE CODE                           
         GOTO1 FINDOUT,PARAS,OUTCOD,NDIOA                                       
         BNE   TRAPERR             OUTLINE NOT FOUND                            
*                                                                               
         BAS   RE,TRACE                                                         
         ZIC   R0,NDLEV                                                         
         SH    R0,=H'3'                                                         
         STC   R0,OUTLEV           OUTLINE LEVEL NUMBER                         
         L     R3,NDIOA            A(OUTLINE RECORD)                            
*                                                                               
         USING BURECD,R3                                                        
         MVC   OUTCOD,BUKCODE                                                   
         MVI   ELCODE,BUOUTELQ     OUTLINE DESCRIPTION ELEMENT                  
         LA    R3,BUFRSTEL                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                OUTLINE DESCRIPTION MUST BE THERE            
*                                                                               
         USING BUOUTD,R3                                                        
         MVC   OUTNAM,BUOUTNAM     SAVE OUTLINE RECORD VALUES                   
         MVC   WORK(L'OUTNAM),OUTNAM                                            
         DROP  R3                                                               
*                                                                               
         XC    EOUTCOD,EOUTCOD                                                  
         LA    R4,32(R4)           BUMP TO NEXT SCANNER ENTRY                   
         CLI   0(R4),0             TEST END OUTLINE GIVEN                       
         BE    VALOUTL5            NO                                           
*                                                                               
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   EOUTCOD(0),12(R4)   END OUTLINE CODE                             
         GOTO1 FINDOUT,PARAS,EOUTCOD,NDIOA                                      
         BNE   TRAPERR             OUTLINE NOT FOUND                            
*                                                                               
         BAS   RE,TRACE                                                         
         ZIC   R0,NDLEV                                                         
         SH    R0,=H'3'                                                         
         STC   R0,EOUTLEV          END OUTLINE LEVEL NUMBER                     
         L     R3,NDIOA            A(OUTLINE RECORD)                            
         DROP  R5                                                               
*                                                                               
         USING BURECD,R3                                                        
         MVC   EOUTCOD,BUKCODE                                                  
         MVI   ELCODE,BUOUTELQ     OUTLINE DESCRIPTION ELEMENT                  
         LA    R3,BUFRSTEL                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                OUTLINE DESCRIPTION MUST BE THERE            
*                                                                               
         USING BUOUTD,R3                                                        
         MVC   EOUTNAM,BUOUTNAM                                                 
         DROP  R3                                                               
*                                                                               
         MVI   WORK+21,C'-'        SEPERATE NAMES WITH A HYPHEN                 
         MVC   WORK+23(L'EOUTNAM),EOUTNAM                                       
         GOTO1 SQUASHER,DMCB,WORK,43                                            
*                                                                               
VALOUTL5 MVC   SVOUTVAL,OUTVALS                                                 
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VALOUTLX                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         MVI   GLFHEADL,10         LEAVE AN EXTRA LINE BEFORE COLUMNS           
         MVI   GLLHEADL,12         FOR DOWNLOADING                              
         DROP  R6                                                               
*                                                                               
VALOUTLX MVC   WRIOUTD,WORK                                                     
         OI    WRIOUTDH+6,X'80'    XMIT                                         
         CLI   MODE,DISPREC        DON'T VALIDATE ANY MORE ON DISPLAY           
         BE    EXIT                                                             
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
         LA    R2,WRIPERH          PERIOD FIELD                                 
         XC    PERIOD,PERIOD                                                    
         MVC   PERHEAD,BLANKS                                                   
         MVC   PERHEAD(6),=C'PERIOD'                                            
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    VALOPT              NO - VALIDATE OPTIONS                        
*                                                                               
         MVC   WORK,BLANKS         BUILD PERIOD EXPRESSION HERE                 
         LA    R3,WORK                                                          
         LA    R5,WRIPER           A(START OF FIELD)                            
         LA    R0,WRIPERX          A(END OF FIELD)                              
*                                                                               
VPER10   CR    R5,R0               TEST END OF FIELD                            
         BE    VPER15              YES                                          
         CLI   0(R5),C'('          TEST BEGINNING OF HEADING                    
         BE    VPER20                                                           
         CLI   0(R5),C' '          TEST END OF EXPRESSION                       
         BH    *+14                                                             
*                                                                               
VPER15   MVC   PERHEAD,WORK        USE PERIOD EXPRESSION AS HEADING             
         B     VPER40                                                           
*                                                                               
         MVC   0(1,R3),0(R5)       BUILD PERIOD EXPRESSION IN WORK              
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         B     VPER10                                                           
*                                                                               
VPER20   LA    R3,PERHEAD          PUT HEADING HERE                             
         MVC   PERHEAD,BLANKS      BLANK HEADING IS ALLOWED                     
         LA    R5,1(R5)            BUMP PAST '('                                
         SR    R4,R4               COUNT CHARACTERS IN HEADING                  
*                                                                               
VPER30   CR    R5,R0               TEST END OF FIELD                            
         BE    INVHEAD             YES - NEVER FOUND ')'                        
         CLI   0(R5),C')'          TEST END OF HEADING                          
         BE    VPER40              YES                                          
         LA    R4,1(R4)            INCREMENT CHARACTER COUNT                    
         CH    R4,=H'16'           NO MORE THAN 16 CHARACTERS ALLOWED           
         BH    INVHEAD                                                          
*                                                                               
         MVC   0(1,R3),0(R5)       BUILD HEADING IN PERHEAD                     
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         B     VPER30                                                           
*                                                                               
VPER40   CLC   WORK,BLANKS         TEST A PERIOD WAS GIVEN                      
         BE    VALOPT              NO                                           
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+8(1),PLANST+1  PLAN START MONTH                             
         MVC   DMCB+9(1),CLTTYPE   FISCAL MONTH TYPE                            
         MVI   DMCB+11,C'Y'        ALLOW CROSS FISCAL YEAR                      
         GOTO1 VMONVAL,DMCB,WORK,PLANST                                         
         MVC   PERIOD,DMCB+4       RETURNED YM/YM                               
         OC    PERIOD,PERIOD       TEST PERIOD RETURNED                         
         BZ    INVMON              NO -- FIELD INVALID                          
         MVC   PEREXP,WORK         SAVE PERIOD EXPRESSION                       
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
VALOPT   MVI   REPWIDTH,132        DEFAULT REPORT WIDTH=132                     
         XC    STKDTYPS,STKDTYPS   TABLE OF STACKED DATA TYPES                  
         MVI   STKDWID,0                                                        
         MVI   STKHWID,0                                                        
         MVI   NOEJECT,C'N'                                                     
         L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         MVI   RCSUBPRG,1                                                       
         DROP  RF                                                               
*                                                                               
         LA    R2,WRIOPTSH         OPTIONS FIELD                                
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    VALCOL              NO - VALIDATE COLUMNS                        
*                                                                               
         XCEF  SCANBLK,512                                                      
         GOTO1 SCANNER,DMCB,(42,(R2)),SCANBLK,0                                 
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         MVI   OPTNUM,1            FIRST OPTION                                 
         LA    R4,SCANBLK          A(SCANNER BLOCK)                             
         ZIC   R0,DMCB+4           NUMBER OF SCANNER ENTRIES                    
         XC    DMCB,DMCB                                                        
*                                                                               
* DOWNLOAD OPTION                                                               
*                                                                               
VOPT10   CLC   =CL10'DOWN',12(R4)  TEST DOWNLOAD OPTION                         
         BNE   VOPT20                                                           
         CLI   1(R4),0             'DOWN' TAKES NO SUB-OPTION                   
         BNE   INVOPT                                                           
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         OI    GLDOWNLD,X'80'      TURN ON DOWNLOAD SWITCH                      
         B     VOPTNEXT                                                         
         DROP  R6                                                               
*                                                                               
* SPACING OPTION                                                                
*                                                                               
VOPT20   CLC   =CL10'SPACE',12(R4) TEST SPACING OPTION                          
         BNE   VOPT30                                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLC   =CL26'2',22(R4)     TEST SPACE=2                                 
         BNE   VOPT25                                                           
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         MVI   GLSPACE,2                                                        
         B     VOPTNEXT                                                         
         DROP  R6                                                               
*                                                                               
VOPT25   CLC   =CL26'3',22(R4)     TEST SPACE=3                                 
         BNE   INVOPT                                                           
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         MVI   GLSPACE,3                                                        
         B     VOPTNEXT                                                         
         DROP  R6                                                               
*                                                                               
* FOOTLINE OPTION                                                               
*                                                                               
VOPT30   CLC   =CL10'FOOT',12(R4)  TEST FOOTLINE OPTION                         
         BNE   VOPT40                                                           
         MVC   WORK(2),=C'**'      DEFAULT FILTER                               
         CLI   1(R4),0             TEST ANY FILTER GIVEN                        
         BE    VOPT35              NO                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),2             MAXIMUM LENGTH OF FILTER                     
         BNH   *+12                                                             
         MVI   ERROR,INVFILT                                                    
         B     TRAPSCAN                                                         
         MVC   WORK(1),22(R4)      FIRST FILTER BYTE                            
         CLI   1(R4),1             TEST SECOND FILTER BYTE GIVEN                
         BE    VOPT35              NO                                           
         MVC   WORK+1(1),23(R4)    SECOND FILTER BYTE                           
*                                                                               
VOPT35   CLI   MODE,PRINTREP                                                    
         BNE   VOPTNEXT                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         OI    GLABCTXT,X'40'      TURN ON FOOTLINE OPTION                      
         MVC   GLABCTFC,WORK       SAVE FILTER                                  
         B     VOPTNEXT                                                         
         DROP  R6                                                               
*                                                                               
* WIDE PRINTING OPTION                                                          
*                                                                               
VOPT40   CLC   =CL10'WIDE',12(R4)  TEST 'WIDE' REPORT OPTION                    
         BNE   VOPT50                                                           
         CLI   1(R4),0             'WIDE' TAKES NO SUB-OPTION                   
         BNE   INVOPT                                                           
*                                                                               
         MVI   REPWIDTH,165        REPORT IS 165 CHARACTERS WIDE                
         L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         MVI   RCSUBPRG,2                                                       
         DROP  RF                                                               
         B     VOPTNEXT                                                         
*                                                                               
* TRACE OPTION                                                                  
*                                                                               
VOPT50   CLC   =CL10'TRACE',12(R4) TEST TRACE OPTION                            
         BNE   VOPT60                                                           
         CLI   1(R4),0             'TRACE' TAKES NO SUB-OPTION                  
         BNE   INVOPT                                                           
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         MVI   GLTRACE,C'Y'        TURN ON DRIVER TRACE                         
         B     VOPTNEXT                                                         
         DROP  R6                                                               
*                                                                               
* LINES OPTION                                                                  
*                                                                               
VOPT60   CLC   =CL10'LINES',12(R4) TEST LINES OPTION                            
         BNE   VOPT70              NO                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
*                                                                               
         TM    3(R4),X'80'         TEST SUB-OPTION IS NUMERIC                   
         BZ    INVOPT              NO                                           
         ICM   R1,15,8(R4)         NUMBER OF LINES                              
         CH    R1,=H'40'                                                        
         BL    INVOPT                                                           
         CH    R1,=H'60'                                                        
         BH    INVOPT                                                           
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         STC   R1,MAXLINES                                                      
         DROP  RF                                                               
         B     VOPTNEXT                                                         
*                                                                               
* NO PAGE EJECT OPTION                                                          
*                                                                               
VOPT70   CLC   =CL10'NOEJECT',12(R4) TEST NOEJECT OPTION                        
         BNE   VOPT80                                                           
         CLI   1(R4),0             'NOEJECT' TAKES NO SUB-OPTION                
         BNE   INVOPT                                                           
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         MVI   NOEJECT,C'Y'        TURN ON NOEJECT FLAG                         
         B     VOPTNEXT                                                         
*                                                                               
* STACKED COLUMNS OPTION                                                        
*                                                                               
VOPT80   CLC   =CL10'STACK',12(R4) TEST STACKED COLUMNS                         
         BNE   VOPT120                                                          
*                                                                               
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             MAKE SURE THERE'S A SUB-OPTION               
         BE    INVOPT              THERE ISN'T                                  
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         XC    FLDH,FLDH           BUILD DUMMY FIELD HEADER FOR SCANNER         
         ZIC   R1,1(R4)                                                         
         STC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),22(R4)                                                    
         LA    R1,9(R1)                                                         
         STC   R1,FLDH                                                          
*                                                                               
         GOTO1 SCANNER,DMCB,FLDH,BLOCK,C',=/ '                                  
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R6,STKDTYPS         TABLE OF STACKED DATA TYPES                  
         USING STACKD,R6                                                        
         LA    R5,BLOCK            A(SCANNER BLOCK)                             
         ZIC   R1,DMCB+4           NUMBER OF SCANNER ENTRIES                    
         XC    DMCB,DMCB                                                        
         CH    R1,=H'4'            TEST MORE THAN 4 DATA TYPES                  
         BH    TOOSTACK            YES                                          
         MVI   SUBOPT,1                                                         
*                                                                               
VOPT90   CLI   0(R5),9             TEST FOR MORE THAN 9 CHARS                   
         BH    NOSTACK             YES-BLATANT ERROR                            
         BL    *+12                                                             
         CLI   12(R5),C'*'         EXACTLY 9-MUST START WITH ASTERISK           
         BNE   NOSTACK                                                          
*                                                                               
         CLI   12(R5),C'*'         TEST PERIOD OVERRIDE FLAG                    
         BNE   VOPT92                                                           
*                                                                               
         MVC   12(9,R5),13(R5)     SLIDE OVER REST OF INPUT                     
         OI    STKINDS,STKIPER     NOTE PERIOD OVERRIDE FLAG                    
*                                                                               
VOPT92   LA    R3,KEY              BUILD DATA TYPE DIRECTORY KEY                
         USING BURECD,R3                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'PLANKEY),PLANKEY BEGINS WITH PLAN KEY                      
         MVI   BUDSUB,BUDSUBQ      DATA TYPE CODE                               
         MVC   BUDTYP,12(R5)       ALLEGED DATA CODE                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUKEY),KEYSAVE TEST DIRECTORY ENTRY FOUND                  
         BNE   NOSTACK                                                          
*                                                                               
         MVC   STKDTYPE,BUDTYP     HANG ON TO DATA CODE                         
         MVC   STKDSKAD,BUKDA      HANG ON TO DISK ADDRESS                      
         MVC   STKPOSNO,SUBOPT     STACK NUMBER                                 
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET THE DATA TYPE RECORD                     
         L     R3,AIO              A(DATA TYPE RECORD)                          
         MVI   ELCODE,BUDTELQ      DATA TYPE DESCRIPTION ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDTD,R3                                                         
         CLC   STKDWID,BUDTCOL     STKDWID MUST BE WIDEST COLUMN                
         BH    *+10                                                             
         MVC   STKDWID,BUDTCOL                                                  
*                                                                               
         L     R3,AIO              A(DATA TYPE RECORD)                          
         MVI   ELCODE,BUSTELQ      STACKED HEADING ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   VOPT100             NONE PRESENT                                 
         USING BUSTD,R3                                                         
         ZIC   RF,BUSTLEN                                                       
         SH    RF,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STKHEAD(0),BUSTNAME SAVE HEADING                                 
         B     VOPT110                                                          
*                                                                               
VOPT100  L     R3,AIO              A(DATA TYPE RECORD)                          
         MVI   ELCODE,BUDHELQ      DATA TYPE HEADING ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE                         
         USING BUDHD,R3                                                         
         ZIC   RF,BUDHLEN                                                       
         SH    RF,=H'4'            3 FOR OVERHEAD, 1 FOR EX                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STKHEAD(0),BUDHEAD  SAVE HEADING                                 
         DROP  R3                                                               
*                                                                               
VOPT110  LA    RF,1(RF)            RESTORE HEADING WIDTH                        
         CLM   RF,1,STKHWID                                                     
         BNH   *+8                                                              
         STC   RF,STKHWID          WIDTH OF WIDEST HEADING                      
*                                                                               
         LA    R6,STACKLEN(R6)     BUMP TO NEXT ENTRY IN TABLE                  
         LA    R5,32(R5)           NEXT ENTRY IN SCANNER BLOCK                  
         ZIC   RF,SUBOPT                                                        
         LA    RF,1(RF)            INCREMENT SUB-OPTION NUMBER                  
         STC   RF,SUBOPT                                                        
*                                                                               
         BCT   R1,VOPT90           VALIDATE NEXT DATA TYPE                      
         B     VOPTNEXT                                                         
*                                                                               
* SUPPRESS ZERO LINES OPTION                                                    
*                                                                               
VOPT120  CLC   =CL10'ZERO',12(R4)                                               
         BNE   INVOPT                                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST FOR PARAMETER VALUE                     
         BE    INVOPT                                                           
         CLI   1(R4),2                                                          
         BH    INVOPT                                                           
         BL    VOPT122                                                          
*                                                                               
         CLC   =C'NO',22(R4)                                                    
         BE    VOPT125                                                          
         B     INVOPT                                                           
*                                                                               
VOPT122  CLI   22(R4),C'N'                                                      
         BNE   INVOPT                                                           
*                                                                               
VOPT125  CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VOPTNEXT                                                         
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
         OI    GLABCOPT,GLABCSUP   SUPPRESS ZERO LINES                          
         B     VOPTNEXT                                                         
         DROP  R6                                                               
*                                                                               
* VALIDATE NEXT OPTION                                                          
*                                                                               
VOPTNEXT LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         BCT   R0,VOPT10                                                        
         EJECT                                                                  
* VALIDATE COLUMNS                                                              
*                                                                               
VALCOL   MVI   TEXTCOLS,0          NO TEXT COLUMNS YET                          
         MVI   REFFOUND,C'N'       NO REF DATA FOUND YET                        
         MVI   COLFOUND,C'N'       NO COLUMN DATA FOUND YET                     
         MVI   DATAFND,C'N'        NO DATA COLUMNS FOUND YET                    
         LA    R2,WRICOLSH         COLUMNS FIELD                                
*                                                                               
         CLI   STKDWID,0           TEST ANY STACKED COLUMNS                     
         BE    VCOL10              NO                                           
         LA    R6,STKDTYPS         TABLE OF STACKED DATA TYPES                  
         USING STACKD,R6                                                        
*                                                                               
         ZIC   RF,STKHWID          WIDTH OF 'BANNER' COLUMN                     
         AH    RF,CURWIDTH         ADD WIDTH OF ROWS                            
         LA    RF,1(RF)            PLUS ONE FOR VERTICAL LINE                   
         STH   RF,CURWIDTH                                                      
*                                                                               
         OI    DRFLAGO,X'80'       BUILD 'BANNER' COLUMN                        
         MVC   DRTYPEO,=X'C300'                                                 
         MVC   DRLENO,STKHWID                                                   
         MVC   DRRTNO,=C'ABCSTACK'                                              
*                                                                               
         LA    RF,STKDTYPS         A(STACKED DATA TYPE TABLE)                   
         STCM  RF,15,DRARGSO                                                    
         MVC   DRARGSO+4(1),STKHWID                                             
         MVI   DRNARGSO,5                                                       
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VCOL10              NO                                           
         MVI   DRACTION,DRGENCOL   BUILD COLUMN ELEMENTS                        
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0           DIE IF ERROR OFFLINE                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCOL10   CLI   5(R2),0             TEST ANY INPUT                               
         BE    VCOL1000                                                         
*                                                                               
         MVI   COLFOUND,C'Y'       WE HAVE COLUMN DATA                          
         XCEF  SCANBLK,512                                                      
         GOTO1 SCANNER,DMCB,(42,(R2)),SCANBLK,0                                 
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         MVI   OPTNUM,1            FIRST OPTION                                 
         LA    R4,SCANBLK          A(SCANNER BLOCK)                             
         ZIC   R0,DMCB+4           NUMBER OF SCANNER ENTRIES                    
         XC    DMCB,DMCB                                                        
*                                                                               
         MVI   TEXTFLAG,C'Y'                                                    
         CLC   =CL10'TEXT',12(R4)  TEST 'TEXT' OPTION                           
         BE    VCOL200                                                          
         CLC   =CL10'REFNUM',12(R4) TEST 'REFNUM' OPTION                        
         BE    VCOL300                                                          
         CLC   =CL10'REFLET',12(R4) TEST 'REFLET' OPTION                        
         BE    VCOL300                                                          
         CLC   =CL10'REFNUMA',12(R4) TEST 'REFNUMA' OPTION                      
         BE    VCOL300                                                          
         CLC   =CL10'REFLETA',12(R4) TEST 'REFLETA' OPTION                      
         BE    VCOL300                                                          
*                                                                               
         MVI   TEXTFLAG,C'N'       THIS ENTRY NOT TEXT                          
         MVI   DATAFND,C'Y'        FLAG DATA COLOMN FOUND                       
         MVC   AIO,AIO2            FOR DATA TYPE RECORD                         
         CLI   STKDWID,0           TEST ANY STACKED COLUMNS                     
         BE    VCOL12              NO                                           
*                                                                               
         MVC   DTYPECD,STKDTYPE    HANG ON TO DATA CODE                         
         MVC   DTYPEDA,STKDSKAD    HANG ON TO DISK ADDRESS                      
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),SYSFIL,DTYPEDA,AIO,DMWORK            
         B     VCOL15                                                           
*                                                                               
VCOL12   LA    R3,KEY              BUILD DATA TYPE DIRECTORY KEY                
         USING BURECD,R3                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'PLANKEY),PLANKEY BEGINS WITH PLAN KEY                      
         MVI   BUDSUB,BUDSUBQ      DATA TYPE CODE                               
         MVC   BUDTYP,12(R4)       ALLEGED DATA CODE                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUKEY),KEYSAVE TEST DIRECTORY ENTRY FOUND                  
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPSCAN                                                         
*                                                                               
         MVC   DTYPEDA,BUKDA       HANG ON TO DISK ADDRESS                      
         MVC   DTYPECD,BUDTYP      HANG ON TO DATA CODE                         
         GOTO1 GETREC              GET THE DATA TYPE RECORD                     
*                                                                               
VCOL15   L     R3,AIO              A(DATA TYPE RECORD)                          
         OI    DRFLAGI,X'80'       FILL DRONE BLOCK W DATA TYPE PARAMS          
         MVC   DRTYPEI,=X'E24E01'                                               
         MVI   DRLENI,8                                                         
         MVC   DRRTNI,BUDTYP                                                    
         MVI   DRNARGSI,7                                                       
*                                                                               
         MVI   ELCODE,BUDTELQ      DATA TYPE DESCRIPTION ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDTD,R3                                                         
*                                                                               
         OI    DRFLAGO,X'80'                                                    
         MVC   DRTYPEO,=X'D500'                                                 
         MVC   DRLENO,BUDTCOL                                                   
         MVC   DRSCALEO,BUDTSC                                                  
         MVC   DRDECO,BUDTDEC                                                   
*                                                                               
         TM    BUDTFORM,BUDTZERO                                                
         BZ    *+8                                                              
         OI    DROPTSO,DRZEROO                                                  
         TM    BUDTFORM,BUDTCOMS                                                
         BZ    *+8                                                              
         OI    DROPTSO,DRCOMMAO                                                 
         TM    BUDTFORM,BUDTNEGS                                                
         BO    *+12                                                             
         OI    DROPTSO,DRBKMINO                                                 
         B     *+8                                                              
         OI    DROPTSO,DRMINUSO                                                 
         TM    BUDTFORM,BUDTLEFT                                                
         BO    *+12                                                             
         OI    DROPTSO,DRALGNRO                                                 
         B     *+8                                                              
         OI    DROPTSO,DRALGNLO                                                 
         TM    BUDTFORM,BUDTCURS                                                
         BZ    *+8                                                              
         MVI   DRFLOATO,C'$'                                                    
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,BUDHELQ      DATA TYPE HEADING ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDHD,R3                                                         
*                                                                               
         LA    R5,DRHEAD1          BEGIN WITH HEADLINE 1                        
         USING DRHEADD,R5                                                       
*                                                                               
         CLI   STKDWID,0           TEST ANY STACKED DTYPES                      
         BE    VCOL20              NO                                           
         MVC   DRLENO,STKDWID      USE WIDEST WIDTH                             
         MVI   DRPOSO,C'P'         SET UP STACKING OF PRINT LINES               
         MVI   DRPOSO+1,C'+'       ASSUME IT'S NOT THE FIRST LINE               
         CLI   STKPOSNO,1                                                       
         BNE   *+8                 RIGHT                                        
         MVI   DRPOSO+1,1          WRONG - START AT TOP OF COLUMN               
         B     VCOL25              DON'T LOOK FOR HEADINGS                      
*                                                                               
VCOL20   OI    DRHEAD,X'80'                                                     
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=H'3'                                                         
         STC   R1,DRHLITL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRHLIT(0),BUDHEAD                                                
*                                                                               
         LA    R5,L'DRHDLEND(R5)   USE NEXT AVAILABLE HEADLINE                  
         BAS   RE,NEXTEL                                                        
         BE    VCOL20                                                           
         DROP  R3                                                               
*                                                                               
VCOL25   CLI   STKDWID,0           TEST ANY STACKED DTYPES                      
         BE    *+12                NO - ALWAYS DO HEADING                       
         CLI   STKPOSNO,1                                                       
         BNE   *+8                                                              
         OI    DRHEAD,X'80'        GENERATE HEADING CODE                        
*                                                                               
         MVC   DRHRTN,=CL8'HEADPERD'                                            
         MVC   DRHARGS,BLANKS                                                   
         MVC   COLPERD,PERIOD      DEFAULT PERIOD IS GIVEN ABOVE                
         MVC   DRARGSI(4),COLPERD                                               
         XC    COLASAT,COLASAT     CLEAR 'AS AT' DATE                           
         XC    COLPERD2,COLPERD2   CLEAR ALTERNATE PERIOD                       
*                                                                               
         CLI   STKDWID,0           TEST ANY STACKED DTYPES                      
         BNE   VCOL35              YES - THIS MUST BE A PERIOD                  
*                                                                               
         LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0             TEST ANY INPUT                               
         BNE   VCOL30              YES                                          
*                                                                               
         MVI   DRHNARG,16                                                       
         CLI   DRLENO,16           TEST COLUMN CAN ALWAYS HOLD HEADING          
         BL    *+14                NO                                           
         MVC   DRHARGS,PERHEAD     USE DEFAULT HEADING AND PERIOD               
         B     VCOL150                                                          
*                                                                               
         ZIC   R1,DRLENO           TRUNCATE HEADING                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRHARGS(0),PERHEAD  USE DEFAULT HEADING AND PERIOD               
         B     VCOL150                                                          
*                                                                               
VCOL30   ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
*                                                                               
VCOL35   MVC   WORK,BLANKS                                                      
         LA    R3,WORK                                                          
         LA    R1,12(R4)                                                        
*                                                                               
VCOL40   CLI   0(R1),C'('          TEST BEGINNING OF HEADING                    
         BE    VCOL50                                                           
         CLI   0(R1),C' '          TEST END OF FIELD                            
         BNH   VCOL70              YES                                          
*                                                                               
         MVC   0(1,R3),0(R1)       BUILD PERIOD EXPRESSION IN WORK              
         LA    R3,1(R3)                                                         
         LA    R1,1(R1)                                                         
         B     VCOL40                                                           
*                                                                               
VCOL50   LA    R3,DRHARGS          PUT HEADING HERE                             
         MVI   DRHNARG,16                                                       
         MVC   DRHARGS,BLANKS      BLANK HEADING IS ALLOWED                     
         LA    R1,1(R1)            BUMP PAST '('                                
         LA    R0,16               MAXIMUM OF 16 CHARACTERS IN HEADING          
*                                                                               
VCOL60   CLI   0(R1),C')'          TEST END OF HEADING                          
         BE    VCOL70                                                           
         MVC   0(1,R3),0(R1)       BUILD HEADING IN DRHARGS                     
         LA    R3,1(R3)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VCOL60                                                        
         CLI   0(R1),C')'          THIS MUST BE THE END                         
         BNE   INVHEAD                                                          
*                                                                               
VCOL70   XC    DMCB,DMCB                                                        
         MVC   DMCB+8(1),PLANST+1  PLAN START MONTH                             
         MVC   DMCB+9(1),CLTTYPE   FISCAL MONTH TYPE                            
         MVI   DMCB+11,C'Y'        ALLOW CROSS FISCAL YEAR                      
         GOTO1 VMONVAL,DMCB,WORK,PLANST                                         
         OC    DMCB+4(4),DMCB+4    TEST RETURNED YM/YM                          
         BNZ   VCOL75                                                           
*                                                                               
         CLI   12(R4),C'('         TEST THIS WAS JUST A HEADING                 
         BE    VCOL80              YES                                          
*                                                                               
         MVI   DRHNARG,16                                                       
         CLI   DRLENO,16           TEST COLUMN CAN ALWAYS HOLD HEADING          
         BL    *+14                NO                                           
         MVC   DRHARGS,PERHEAD                                                  
         B     VCOL90                                                           
*                                                                               
         ZIC   R1,DRLENO           USE PERIOD HEADING - TRUNCATE                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRHARGS(0),PERHEAD                                               
         B     VCOL90                                                           
*                                                                               
VCOL75   MVC   COLPERD,DMCB+4      USE SPECIFIED PERIOD                         
         MVC   DRARGSI(4),COLPERD                                               
         CLI   DRHNARG,16          TEST USE PERIOD EXPR AS HEADING              
         BE    VCOL80              NO                                           
*                                                                               
         MVI   DRHNARG,16                                                       
         CLI   DRLENO,16           TEST COLUMN CAN ALWAYS HOLD HEADING          
         BL    *+14                NO                                           
         MVC   DRHARGS,WORK                                                     
         B     VCOL80                                                           
*                                                                               
         ZIC   R1,DRLENO           YES - TRUNCATE                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRHARGS(0),WORK                                                  
*                                                                               
VCOL80   LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0             TEST ANY INPUT                               
         BE    VCOL150             NO                                           
*                                                                               
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
*                                                                               
VCOL85   CLI   1(R4),0             TEST FOR A DIVIDED FIELD                     
         BE    VCOL90              NO-EDIT FOR 'ASAT'                           
*                                                                               
         CLI   0(R4),1                                                          
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPSCAN                                                         
         CLI   12(R4),C'P'         TEST 'P' = PERIOD OVERRIDE                   
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPSCAN                                                         
         CLI   STKDWID,0           TEST STACKED OPTION                          
         BE    INVWOSTK            NO-ITS AN ERROR TO USE THIS                  
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+8(1),PLANST+1  PLAN START MONTH                             
         MVC   DMCB+9(1),CLTTYPE   FISCAL MONTH TYPE                            
         MVI   DMCB+11,C'Y'        PERMIT CROSS FISCAL YEAR                     
         GOTO1 VMONVAL,DMCB,22(R4),PLANST                                       
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    INVPER                                                           
         MVC   COLPERD2,DMCB+4     SAVE SECOND DATE                             
*                                                                               
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT SCANNER OPTION NUMBER              
         STC   R1,OPTNUM                                                        
         LA    R4,64(R4)                                                        
         CLI   0(R4),0                                                          
         BE    VCOL150             NO MORE INPUT                                
*                                                                               
VCOL90   XC    WORK,WORK           VALIDATE 'AS AT' DATE                        
         GOTO1 DATVAL,DMCB,(0,12(R4)),WORK                                      
         ZIC   R1,0(R4)            LENGTH OF INPUT FIELD                        
         C     R1,DMCB             TEST RETURNED LENGTH THE SAME                
         BE    *+12                YES                                          
         MVI   ERROR,INVDATE                                                    
         B     TRAPSCAN                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,COLASAT)                                 
         LA    R5,L'DRHDLEND(R5)   USE NEXT AVAILABLE HEADLINE                  
         CLI   STKDWID,0           TEST ANY STACKED DTYPES                      
         BE    *+12                NO - DO ASAT                                 
         CLI   STKPOSNO,1                                                       
         BNE   *+8                                                              
         OI    DRHEAD,X'80'        GENERATE 'ASAT' HEADING                      
*                                                                               
         MVC   DRHRTN,=CL8'HEADASAT'                                            
         MVC   DRHARGS(3),COLASAT                                               
         MVI   DRHNARG,3                                                        
         MVC   DRARGSI+4(3),COLASAT                                             
         DROP  R5                                                               
*                                                                               
         LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         CLI   0(R4),0             TEST ANY INPUT                               
         BNE   INVOPT              YES -- BUT NOTHING FOLLOWS 'ASAT'            
*                                                                               
VCOL150  CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VCOL900                                                          
*                                                                               
         L     R3,ADTYPE           A(CURRENT DATA TYPE TABLE ENTRY)             
         USING DTATABD,R3                                                       
         MVC   DTACODE,DTYPECD     FILL IN DATA TYPE TABLE ENTRY                
         MVC   DTADA,DTYPEDA                                                    
         MVC   DTASTDT,COLPERD                                                  
         MVC   DTAENDT,COLPERD+2                                                
         CLI   STKDWID,0           TEST STACK OPTION                            
         BE    VCOL155             NO                                           
*                                                                               
         TM    STKINDS,STKIPER     YES-TEST FOR PERIOD OVERRIDE                 
         BZ    VCOL155             NONE                                         
         OC    COLPERD2,COLPERD2   TEST FOR PERIOD OVERRIDE INPUT               
         BZ    VCOL155             NO                                           
         MVC   DTAPERD,COLPERD2    YES-SET OVERRIDE PERIOD                      
         MVC   DRARGSI(4),COLPERD2                                              
*                                                                               
VCOL155  MVC   DTAASAT,COLASAT                                                  
         LA    R3,DTATABL(R3)      ADVANCE POINTER TO NEXT ENTRY                
         ST    R3,ADTYPE                                                        
         B     VCOL900             THIS COLUMN IS DONE                          
         DROP  R3                                                               
*                                                                               
VCOL200  OI    DRFLAGI,X'80'       FILL IN DRONE BLOCK WITH TEXT PARAMS         
         MVC   DRTYPEI,=X'C30001'                                               
         MVI   DRLENI,X'C8'                                                     
         MVC   DRRTNI,=C'ABCTEXT '                                              
*                                                                               
         OI    DRFLAGO,X'80'                                                    
         MVC   DRTYPEO,=X'C300'                                                 
         MVI   DRLENO,20                                                        
         OI    DROPTSO,DRCHOPO                                                  
*                                                                               
         OI    DRHEAD1,X'80'                                                    
         MVI   DRH1LITL,7                                                       
         MVC   DRH1LIT(7),=C'REMARKS'                                           
*                                                                               
         ZIC   R1,TEXTCOLS         NUMBER OF TEXT COLUMNS USED                  
         LA    R1,1(R1)            INCREMENT                                    
         STC   R1,TEXTCOLS                                                      
         CH    R1,=H'4'            MAXIMUM NUMBER OF TEXT COLUMNS               
         BH    TEXTERR                                                          
*                                                                               
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         MVC   DRARGSI(2),=C'**'   DEFAULT FILTER                               
         MVI   DRNARGSI,2                                                       
         CLI   1(R4),0             TEST ANY FILTER GIVEN                        
         BE    VCOL210             NO                                           
         CLI   1(R4),2             MAXIMUM LENGTH OF FILTER                     
         BNH   *+12                                                             
         MVI   ERROR,INVFILT                                                    
         B     TRAPSCAN                                                         
         MVC   DRARGSI(1),22(R4)   FIRST FILTER BYTE                            
         CLI   1(R4),1             TEST SECOND FILTER BYTE GIVEN                
         BE    VCOL210             NO                                           
         MVC   DRARGSI+1(1),23(R4)                                              
*                                                                               
VCOL210  CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VCOL280                                                          
*                                                                               
         L     RF,AGLOBALS                                                      
         USING GLOBALD,RF                                                       
         OI    GLABCTXT,X'80'      TURN ON TEXT OPTION                          
         ZIC   R3,TEXTCOLS                                                      
         LA    R1,GLABCTCC         FILTERS ARE SAVED HERE POSITIONALLY          
         SH    R1,=H'2'                                                         
         LA    R1,2(R1)                                                         
         BCT   R3,*-4                                                           
         MVC   0(2,R1),DRARGSI     PUT FILTER IN DRIVER GLOBALS                 
         B     VCOL280                                                          
         DROP  RF                                                               
*                                                                               
VCOL220  CLC   =CL10'L',12(R4)     TEST LENGTH OPTION                           
         BNE   VCOL250                                                          
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST LENGTH IS GIVEN                         
         BE    INVOPT                                                           
         TM    3(R4),X'80'         TEST NUMERIC DATA                            
         BZ    INVOPT                                                           
         ICM   R1,15,8(R4)         NUMERIC VALUE                                
         STC   R1,DRLENO           PUT LENGTH IN DRONE BLOCK                    
         B     VCOL280                                                          
*                                                                               
VCOL250  CLC   =CL10'LIT',12(R4)   TEST LITERAL OPTION                          
         BNE   INVOPT                                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST STRING IS GIVEN                         
         BE    INVOPT                                                           
         ZIC   R1,1(R4)                                                         
         STC   R1,DRH1LITL         PUT LENGTH IN DRONE BLOCK                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRH1LIT(0),22(R4)   PUT LITERAL IN DRONE BLOCK                   
*                                                                               
VCOL280  LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         BCT   R0,VCOL220                                                       
         B     VCOL900                                                          
*                                                                               
VCOL300  CLI   REFFOUND,C'Y'       TEST REF WAS ALREADY FOUND                   
         BE    REFERR              YES - ERROR                                  
         MVI   REFFOUND,C'Y'                                                    
*                                                                               
         OI    DRFLAGI,X'80'       FILL IN DRONE BLOCK WITH TEXT PARAMS         
         MVC   DRTYPEI,=X'C30001'                                               
         MVI   DRLENI,X'C8'                                                     
         MVC   DRRTNI,12(R4)       'REFNUM' OR 'REFLET'                         
*                                                                               
         OI    DRFLAGO,X'80'                                                    
         MVC   DRTYPEO,=X'C300'                                                 
         MVI   DRLENO,4                                                         
*                                                                               
         OI    DRHEAD1,X'80'                                                    
         MVI   DRH1LITL,4                                                       
         MVC   DRH1LIT(4),=C'REF.'                                              
*                                                                               
         MVI   ERROR,INVFILT                                                    
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         MVI   DRNARGSI,8                                                       
         CLI   1(R4),0             TEST ANY FILTER GIVEN                        
         BE    TRAPSCAN            NO                                           
*                                                                               
         CLI   1(R4),11            MAXIMUM LENGTH OF ALL 4 FILTERS              
         BH    TRAPSCAN                                                         
*                                                                               
         MVC   DRARGSI,BLANKS                                                   
         LA    R1,22(R4)           R1 POINTS TO REF CODES                       
         LA    R3,DRARGSI                                                       
*                                                                               
VCOL310  CLI   0(R1),C'/'                                                       
         BE    TRAPSCAN                                                         
         CLI   0(R1),C' '                                                       
         BE    TRAPSCAN                                                         
         MVC   0(1,R3),0(R1)       PUT FIRST CHARACTER IN ARGUMENTS             
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   0(R1),C'/'                                                       
         BNE   VCOL320                                                          
         MVI   0(R3),C' '          PAD CODE WITH BLANK                          
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         B     VCOL310                                                          
*                                                                               
VCOL320  CLI   0(R1),C' '          TEST END OF STRING                           
         BE    VCOL340                                                          
         MVC   0(1,R3),0(R1)       PUT SECOND CHARACTER IN ARGS                 
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R1),C'/'                                                       
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     VCOL310                                                          
         CLI   0(R1),C' '                                                       
         BNE   TRAPSCAN                                                         
*                                                                               
VCOL340  CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VCOL380                                                          
*                                                                               
         L     RF,AGLOBALS                                                      
         USING GLOBALD,RF                                                       
         MVC   GLABCTRC,DRARGSI    CODES ARE SAVED HERE POSITIONALLY            
         CLC   =C'REFNUM',DRRTNI   TEST REFNUM                                  
         BNE   *+12                                                             
         OI    GLABCTXT,X'20'      TURN ON REFNUM OPTION                        
         B     VCOL380                                                          
*                                                                               
         CLC   =C'REFLET',DRRTNI   TEST REFLET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    GLABCTXT,X'10'      TURN ON REFLET OPTION                        
         B     VCOL380                                                          
         DROP  RF                                                               
*                                                                               
VCOL360  CLC   =CL10'L',12(R4)     TEST LENGTH OPTION                           
         BNE   VCOL370                                                          
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST LENGTH IS GIVEN                         
         BE    INVOPT                                                           
         TM    3(R4),X'80'         TEST NUMERIC DATA                            
         BZ    INVOPT                                                           
         ICM   R1,15,8(R4)         NUMERIC VALUE                                
         CH    R1,=H'1'            MUST BE FROM 1 TO 4, INCLUSIVE               
         BL    INVOPT                                                           
         CH    R1,=H'4'                                                         
         BH    INVOPT                                                           
         STC   R1,DRLENO           PUT LENGTH IN DRONE BLOCK                    
         B     VCOL380                                                          
*                                                                               
VCOL370  CLC   =CL10'LIT',12(R4)   TEST LITERAL OPTION                          
         BNE   INVOPT                                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST STRING IS GIVEN                         
         BE    INVOPT                                                           
         CLI   1(R4),4                                                          
         BH    INVOPT              MAXIMUM OF 4 CHARACTERS                      
         ZIC   R1,1(R4)                                                         
         STC   R1,DRH1LITL         PUT LENGTH IN DRONE BLOCK                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRH1LIT(0),22(R4)   PUT LITERAL IN DRONE BLOCK                   
*                                                                               
VCOL380  LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         BCT   R0,VCOL360                                                       
*                                                                               
VCOL900  CLI   STKDWID,0           TEST ANY STACKED COLUMNS                     
         BE    *+12                NO                                           
         CLI   STKPOSNO,1          TEST FIRST COLUMN                            
         BNE   VCOL920             NO                                           
*                                                                               
         ZIC   R1,DRLENO           MAKE SURE COLUMN WILL FIT                    
         AH    R1,CURWIDTH                                                      
         LA    R1,1(R1)            PLUS ONE FOR SPACE BETWEEN COLUMNS           
         ZIC   R0,REPWIDTH                                                      
         CR    R1,R0                                                            
         BH    TOOWIDE                                                          
         STH   R1,CURWIDTH                                                      
*                                                                               
VCOL920  CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VCOL1000            NO                                           
         MVI   DRACTION,DRGENCOL   BUILD COLUMN ELEMENTS                        
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0           DIE IF ERROR OFFLINE                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCOL1000 CLI   STKDWID,0           TEST ANY STACKED COLUMNS                     
         BE    VCOL1010            NO                                           
         CLI   TEXTFLAG,C'Y'       TEST COLUMN=TEXT                             
         BE    VCOL1010            YES-DO NOT STACK IT                          
         LA    R6,STACKLEN(R6)     BUMP TO NEXT STACKED COLUMN                  
         CLI   0(R6),0             TEST ANY MORE COLUMNS                        
         BNE   VCOL10              YES                                          
*                                                                               
VCOL1010 LA    R0,WRICLSTH         A(LAST COLUMN FIELD)                         
         CR    R0,R2               TEST THIS WAS LAST COLUMN                    
         BE    VCOL1020            IT WAS                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT COLUMN FIELD                    
         LA    R6,STKDTYPS                                                      
         MVI   TEXTFLAG,0                                                       
         B     VCOL10                                                           
         DROP  R6                                                               
*                                                                               
VCOL1020 CLI   COLFOUND,C'Y'       TEST WE HAVE COLUMN DATA                     
         BE    *+16                                                             
         MVI   ERROR,MISSING       WE DO NOT                                    
         LA    R2,WRICOLSH                                                      
         B     TRAPERR                                                          
*                                                                               
         CLI   STKDWID,0           STACKED COLS?                                
         BNE   VCOL1030            YES                                          
         MVI   ERROR,0                                                          
         CLI   DATAFND,C'Y'        ANY DATA COLUMNS?                            
         BE    VCOL1030                                                         
         LA    R2,WRICOLSH                                                      
         B     NODATACL                                                         
*                                                                               
VCOL1030 CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VALDCON             NO                                           
*                                                                               
         MVI   DRACTION,DRWRAPUP   CONCLUDING CALL TO DRONE                     
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0           DIE IF ERROR OFFLINE                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,DTYPETAB         TEST ANY DATA TYPES FOUND                    
         L     R0,ADTYPE                                                        
         CR    R0,R1                                                            
         BE    VALDCON             NO                                           
*                                                                               
         L     R3,ANODBLK          R3=A(NODIO BLOCK)                            
         USING NODBLKD,R3                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',SVPARKEY,0                          
         CLI   NDERR,0                                                          
         BNE   NODERR                                                           
         DROP  R3                                                               
*                                                                               
         LA    R3,BUILDPL          GOTO BUILDER                                 
         USING BUPARMD,R3                                                       
         MVC   BUPANODB,ANODBLK                                                 
         LA    R0,DTYPETAB                                                      
         ST    R0,BUPADTAB                                                      
         MVC   BUPAFAC1,VNODIO                                                  
         MVC   BUPAFAC2,XSORT                                                   
         GOTO1 VBUILDER,BUPARMD                                                 
         L     R1,BUPAACCD         EXTRACT ACCUM DEFINITION VALUES              
         MVC   ACCDEF,0(R1)                                                     
         LA    R1,ACCDEF                                                        
         USING ACCDEFD,R1                                                       
         ST    R1,BUPAACCD         SET A(ACCDEFD) FOR DRIVER                    
         MVC   AOUTTAB,BUPAOUTT                                                 
*                                                                               
         L     RE,TWAMASTC         RE=A(MASTER CONTROLLER BLOCK)                
         USING MASTD,RE                                                         
         L     RF,BUPAOUTT         REQUEST EXTENDED DUMP PRINTING               
         STCM  RF,15,MCUSRDMP      FOR OUTLINE TABLE                            
         SR    R0,R0                                                            
         ICM   R0,7,ACCOUTL                                                     
         AR    RF,R0                                                            
         STCM  RF,15,MCUSRDMP+4                                                 
         L     RF,BUPAACCS         AND ACCUMULATOR TABLE                        
         STCM  RF,15,MCUSRDMP+8                                                 
         SR    R0,R0                                                            
         ICM   R0,7,ACCACCL                                                     
         AR    RF,R0                                                            
         STCM  RF,15,MCUSRDMP+12                                                
         DROP  R1,R3,RE                                                         
*                                                                               
         CLI   NOEJECT,C'Y'        TEST NOEJECT OPTION IS ON                    
         BNE   VCOL1050            NO                                           
*                                                                               
         L     R5,AOUTTAB          OUTLINE TABLE FROM BUILDER                   
         USING OUTTABD,R5                                                       
VCOL1040 OC    OUTSEQN,OUTSEQN     TEST END OF TABLE                            
         BZ    VCOL1050                                                         
         NI    OUTIND1,X'FF'-BUOUTEJ   DON'T EJECT AFTER OUTLINE                
         LA    R5,OUTTABL(R5)                                                   
         B     VCOL1040                                                         
         DROP  R5                                                               
*                                                                               
VCOL1050 CLI   WRIOUTLH+5,0        TEST OUTLINE WAS GIVEN                       
         BE    VALDCON             NO                                           
*                                                                               
         L     R5,AOUTTAB          OUTLINE TABLE FROM BUILDER                   
         USING OUTTABD,R5                                                       
         OC    OUTCOD,BLANKS                                                    
         OC    EOUTCOD,BLANKS                                                   
*                                                                               
VCOL1100 OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VALDCON             WE'RE DONE                                   
         CLC   OUTCODE,OUTCOD      LOOK FOR FIRST ENTRY OF THIS OUTLINE         
         BE    VCOL1200            GOT IT                                       
         OI    OUTIND1,BUOUTNPR    DON'T PRINT LINE                             
         LA    R5,OUTTABL(R5)                                                   
         B     VCOL1100                                                         
*                                                                               
VCOL1200 LA    R5,OUTTABL(R5)      ADVANCE TO NEXT TABLE ENTRY                  
         OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VALDCON             YES                                          
         CLC   OUTLVLN,OUTLEV      TEST STILL WITHIN START OUTLINE              
         BH    VCOL1200            YES                                          
         CLC   EOUTCOD,BLANKS      TEST END OUTLINE GIVEN                       
         BE    VCOL1300            NO                                           
*                                                                               
VCOL1250 CLC   OUTCODE,EOUTCOD     TEST END OUTLINE                             
         BE    VCOL1275            YES                                          
         LA    R5,OUTTABL(R5)                                                   
         OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VALDCON             YES                                          
         B     VCOL1250                                                         
*                                                                               
VCOL1275 LA    R5,OUTTABL(R5)      ADVANCE TO NEXT TABLE ENTRY                  
         OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VALDCON             YES                                          
         CLC   OUTLVLN,EOUTLEV     TEST STILL WITHIN END OUTLINE                
         BH    VCOL1275            YES                                          
*                                                                               
VCOL1300 OI    OUTIND1,BUOUTNPR    DON'T PRINT LINE                             
         LA    R5,OUTTABL(R5)      ADVANCE TO NEXT TABLE ENTRY                  
         OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BNZ   VCOL1300                                                         
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE DETAIL CONTROL                                                       
*                                                                               
VALDCON  LA    R2,WRIDETLH         DETAIL CONTROL FIELD                         
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    VALTITLE            WE'RE ALL DONE                               
         USING OUTTABD,R5                                                       
*                                                                               
         XCEF  SCANBLK,512                                                      
         GOTO1 SCANNER,DMCB,(42,(R2)),SCANBLK,0                                 
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         MVI   OPTNUM,1            FIRST OPTION                                 
         LA    R4,SCANBLK          A(SCANNER BLOCK)                             
         ZIC   R0,DMCB+4           NUMBER OF SCANNER ENTRIES                    
         XC    DMCB,DMCB                                                        
*                                                                               
         CLC   =CL10'PLAN',12(R4)  TEST 'PLAN' KEYWORD                          
         BNE   VDCON100                                                         
*                                                                               
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST LEVEL IS GIVEN                          
         BE    INVLEV                                                           
         TM    3(R4),X'80'         TEST NUMERIC DATA                            
         BZ    INVLEV                                                           
         OC    8(4,R4),8(R4)       TEST LEVEL ZERO                              
         BZ    INVLEV                                                           
         ICM   R1,15,8(R4)         NUMERIC VALUE                                
         LA    RF,MAXOUTS                                                       
         CR    R1,RF               TEST VALID OUTLINE LEVEL NUMBER              
         BH    INVLEV                                                           
         STC   R1,LEVELNUM         HANG ON TO LEVEL NUMBER                      
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VDCON200                                                         
*                                                                               
         L     R5,AOUTTAB          OUTLINE TABLE FROM BUILDER                   
VDCON20  OC    OUTSEQN,OUTSEQN     TEST END OF TABLE                            
         BZ    VDCON200            YES                                          
         CLC   OUTLVLN,LEVELNUM    TEST THIS LINE IS PRINTED                    
         BL    VDCON50             LEVEL IS LOW -- PRINT IT                     
         BE    *+12                                                             
         OI    OUTIND1,BUOUTNPR    LEVEL IS HIGH -- DON'T PRINT LINE            
         B     VDCON50                                                          
*                                                                               
         OI    OUTIND2,OUTIDET     LEVEL IS EQUAL -- MARK IT AS DETAIL          
         MVI   BYTE,OUTITOT                                                     
         XI    BYTE,X'FF'                                                       
         NC    OUTIND2,BYTE        TURN OFF TOTAL INDICATOR BIT                 
*                                                                               
VDCON50  LA    R5,OUTTABL(R5)      ADVANCE TO NEXT TABLE ENTRY                  
         B     VDCON20                                                          
*                                                                               
VDCON100 CLC   =CL10'PLAN',12(R4)  TEST 'PLAN' KEYWORD                          
         BE    INVDCON             NOT ALLOWED HERE                             
*                                                                               
         CLC   =CL10'PT',12(R4)    TEST 'PT' KEYWORD (PLAN TOTAL)               
         BNE   VDCON105            NO                                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLC   =CL26'NO',22(R4)    ONLY OPTION FOR NOW IS 'NO'                  
         BNE   INVLEV                                                           
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VDCON200                                                         
*                                                                               
         L     R5,AOUTTAB          OUTLINE TABLE FROM BUILDER                   
         OC    OUTNODE,OUTNODE     TEST PLAN OUTLINE                            
         BZ    *+6                                                              
         DC    H'0'                SHOULD BE FIRST IN TABLE                     
         OI    OUTIND1,BUOUTNTO    DON'T PRINT PLAN TOTAL LINE                  
         B     VDCON200                                                         
*                                                                               
VDCON105 XC    OUTCOD,OUTCOD                                                    
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTCOD(0),12(R4)                                                 
*                                                                               
         L     R6,ANODBLK                                                       
         USING NODBLKD,R6                                                       
         GOTO1 FINDOUT,PARAS,OUTCOD,NDIOA                                       
         BNE   INVOUTL                                                          
*                                                                               
         BAS   RE,TRACE                                                         
         ZIC   R1,NDLEV                                                         
         SH    R1,=H'3'                                                         
         STC   R1,OUTLEV                                                        
         L     R3,NDIOA            A(OUTLINE RECORD)                            
         DROP  R6                                                               
*                                                                               
         USING BURECD,R3                                                        
         OC    OUTCOD,BLANKS                                                    
         MVI   ELCODE,BUOUTELQ     OUTLINE DESCRIPTION ELEMENT                  
         LA    R3,BUFRSTEL                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                OUTLINE DESCRIPTION MUST BE THERE            
*                                                                               
         USING BUOUTD,R3                                                        
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         CLI   1(R4),0             TEST LEVEL IS GIVEN                          
         BE    INVLEV                                                           
         TM    3(R4),X'80'         TEST NUMERIC DATA                            
         BZ    INVLEV                                                           
         ICM   R1,15,8(R4)         NUMERIC VALUE                                
         LA    RF,MAXOUTS                                                       
         CR    R1,RF               TEST VALID OUTLINE LEVEL NUMBER              
         BH    INVLEV                                                           
         LTR   R1,R1               IF LEVEL NUMBER IS ZERO, OK                  
         BZ    *+12                                                             
         CLM   R1,1,OUTLEV         OTHERWISE, CHECK VALID LEVEL NUMBER          
         BL    INVLEV                                                           
         STC   R1,LEVELNUM         HANG ON TO LEVEL NUMBER                      
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VDCON200                                                         
*                                                                               
         L     R5,AOUTTAB          OUTLINE TABLE FROM BUILDER                   
VDCON110 OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VDCON200            YES -- TRY NEXT SCANNER ENTRY                
         CLC   OUTCODE,OUTCOD      LOOK FOR FIRST ENTRY OF THIS OUTLINE         
         BE    *+12                GOT IT                                       
         LA    R5,OUTTABL(R5)                                                   
         B     VDCON110                                                         
*                                                                               
         CLI   LEVELNUM,0          TEST LEVEL 0 (DON'T PRINT)                   
         BNE   VDCON120                                                         
*                                                                               
VDCON115 OI    OUTIND1,BUOUTNPR    DON'T PRINT LINE                             
         LA    R5,OUTTABL(R5)      ADVANCE TO NEXT TABLE ENTRY                  
         OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VDCON200            YES -- TRY NEXT SCANNER ENTRY                
         CLC   OUTLVLN,OUTLEV      TEST STILL WITHIN THIS OUTLINE               
         BH    VDCON115            YES                                          
         B     VDCON200            NO -- NEXT SCANNER ENTRY                     
*                                                                               
VDCON120 CLC   OUTLVLN,LEVELNUM    TEST THIS LINE IS PRINTED                    
         BL    VDCON150            LEVEL IS LOW -- PRINT IT                     
         BE    *+12                                                             
         OI    OUTIND1,BUOUTNPR    LEVEL IS HIGH -- DON'T PRINT LINE            
         B     VDCON150                                                         
*                                                                               
         OI    OUTIND2,OUTIDET     LEVEL IS EQUAL -- MARK IT AS DETAIL          
         MVI   BYTE,OUTITOT                                                     
         XI    BYTE,X'FF'                                                       
         NC    OUTIND2,BYTE        TURN OFF TOTAL INDICATOR BIT                 
*                                                                               
VDCON150 LA    R5,OUTTABL(R5)      ADVANCE TO NEXT TABLE ENTRY                  
         OC    OUTSEQN,OUTSEQN     TEST END OF OUTLINE TABLE                    
         BZ    VDCON200            YES -- TRY NEXT SCANNER ENTRY                
         CLC   OUTLVLN,OUTLEV      TEST STILL WITHIN THIS OUTLINE               
         BH    VDCON120            YES                                          
*                                                                               
VDCON200 LA    R4,64(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)            INCREMENT OPTION NUMBER                      
         STC   R1,OPTNUM                                                        
         BCT   R0,VDCON100                                                      
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
* VALIDATE TITLE                                                                
*                                                                               
VALTITLE LA    R2,WRITITLH         TITLE FIELD                                  
*                                                                               
         CLI   5(R2),0             TITLE IS REQUIRED                            
         BNE   *+12                WE'RE ALL DONE                               
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         EJECT                                                                  
* GENERATE ABC REPORT                                                           
*                                                                               
         LA    R1,DTYPETAB         TEST ANY DATA TYPES FOUND                    
         L     R0,ADTYPE                                                        
         CR    R0,R1                                                            
         BE    GENX                NO                                           
*                                                                               
         L     R6,AGLOBALS                                                      
         USING GLOBALD,R6                                                       
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         MVC   ADRIVER,DMCB                                                     
         GOTO1 CALLOV,DMCB,0,X'D9000A61'  LOAD T00A61 (ABC DRIVER)              
         MVC   GLASYSDR,DMCB                                                    
*                                                                               
         L     R1,=A(BUFFER)       A(DPG PROGRAM ELEMENTS)                      
         A     R1,RELO                                                          
         ST    R1,GLAPROG                                                       
         LA    R2,HDHK             A(HEAD HOOK)                                 
         ST    R2,GLAHOOK                                                       
*                                                                               
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
*                                                                               
         LA    R2,BUILDPL          A(BUILDER BLOCKS)                            
         USING BUPARMD,R2                                                       
         ST    R2,GLABCBPL         PASSED TO DRIVER                             
         MVI   GLABCSW,1           AND TURN ON ABC SWITCH                       
         ZIC   R0,REPWIDTH         SET PAPER WIDTH                              
         ST    R0,GLWPAPER                                                      
*                                                                               
         L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         ZIC   R0,RCSUBPRG                                                      
         DROP  RF                                                               
         L     RF,AMYBOX                                                        
         USING BOXD,RF                                                          
         BCTR  R0,0                                                             
         STC   R0,BOXFONT          CHOOSE CHARACTER SET                         
         DROP  RF                                                               
*                                                                               
         GOTO1 ADRIVER,DMCB,(R6)   GENERATE REPORT                              
*                                                                               
         LA    R3,ACCDEF           A(ACCUMULATOR DEFINITION BLOCK)              
         USING ACCDEFD,R3                                                       
*                                                                               
         L     R1,BUPAOUTT         FREE OUTLINE TABLE STORAGE                   
         SR    R0,R0                                                            
         ICM   R0,7,ACCOUTL                                                     
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R1,BUPAACCS         FREE ACCUMULATOR TABLE STORAGE               
         SR    R0,R0                                                            
         ICM   R0,7,ACCACCL                                                     
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         XC    MCUSRDMP,MCUSRDMP                                                
         DROP  RE                                                               
*                                                                               
GENX     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* ROUTINE TO SET UP A NODIO KEY                                                 
*                                                                               
*        ON EXIT, NODKEY AND NODKEYSV CONTAIN THE KEY                           
*                                                                               
SETKEY   NTR1                                                                   
*                                                                               
         L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
*                                                                               
         XC    NODKEY,NODKEY                                                    
         LA    R2,NODKEY           R2=STRING POINTER                            
         LR    R0,R2               R0=A(STRING START)                           
         MVC   0(L'CLTCODE,R2),CLTCODE                                          
         LA    R2,L'CLTCODE+1(R2)                                               
         MVC   0(L'PRDCODE,R2),PRDCODE                                          
         LA    R2,L'PRDCODE+1(R2)                                               
         MVC   0(L'PLANCODE,R2),PLANCODE                                        
         LA    R2,L'PLANCODE(R2)                                                
         SR    R2,R0               R2=L'STRING                                  
         OC    NODKEY,BLANKS       SPACE FILL STRING AREA                       
         GOTO1 SQUASHER,DMCB,NODKEY,(NDDELIM,(R2))                              
*                                                                               
SETKEYX  MVC   NODKEYSV,NODKEY     SAVE THE KEY                                 
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* NODERR - TRANSLATE NODIO ERROR MESSAGE TO SYSTEM ERROR MESSAGE                
*                                                                               
* AT ENTRY - R2 POINTS TO ERROR FIELD                                           
*                                                                               
NODERR   L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
         ZIC   RF,NDERR                                                         
         SH    RF,=Y(NDERRFST)     RF HAS INDEX INTO TABLE                      
         IC    R0,NODERRTB(RF)                                                  
         STC   R0,ERROR                                                         
         B     TRAPERR                                                          
         DROP  R5                                                               
*                                                                               
NODERRTB DS    0X                                                               
         DC    AL1(LEVERR)         NDLEVERR                                     
         DC    AL1(0)              NDCDLERR                                     
         DC    AL1(NOTFOUND)       NDRNFERR                                     
         DC    AL1(DUPLICAT)       NDRENERR                                     
         DC    AL1(0)              NDPMDERR                                     
         DC    AL1(TOOLONG)        NDOVFERR                                     
         DC    AL1(0)                                                           
         DC    AL1(0)              NDRESERR                                     
         DC    AL1(0)              NDLIBERR                                     
         DC    AL1(0)              NDANFERR                                     
         DC    AL1(0)              NDAMXERR                                     
         DS    0H                                                               
         SPACE 5                                                                
* TRACE - DO A NODIO TRACE AND READ OF A RECORD POINTED                         
* TO BY NDIOA.  ON EXIT NODKEY AND NODKEYSV SET TO NODAL KEY                    
*                                                                               
TRACE    NTR1                                                                   
*                                                                               
         L     R5,ANODBLK                                                       
         USING NODBLKD,R5                                                       
*                                                                               
         XC    NODKEY,NODKEY                                                    
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'TRACE',NODKEY                             
         OC    NODKEY,BLANKS                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R5                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE OUTLINE CODE AND TO READ THE OUTLINE RECORD           
*                                                                               
*        AT ENTRY, R1 = A(OUTLINE  CODE)                                        
*        ON EXIT, CC=EQ FOR OK, CC=NEQ FOR ERROR AND ERROR SET                  
*                                                                               
FINDOUT  NTR1                                                                   
*                                                                               
         MVI   ERROR,0                                                          
         LM    R2,R3,0(R1)         R2=A(OUTLINE CODE), R3=A(I/O AREA)           
         LA    R4,KEY                                                           
         USING BUCRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BUCSYS,C'B'                                                      
         MVC   BUCAGY,AGENCY                                                    
         MVI   BUCRTYP,BUCRTYPQ                                                 
         MVC   BUCCLT,CLTCODE                                                   
         MVC   BUCPRD,PRDCODE                                                   
         MVC   BUCPLAN,PLANCODE                                                 
         MVC   BUCCODE,0(R2)       EXTRACT OUTLINE CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUCKEY),KEYSAVE TEST FOR HIT ON CODE                       
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     FINDOUTX                                                         
*                                                                               
         MVC   FULL,AIO                                                         
         ST    R3,AIO              SET USER I/O AREA                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING BURECD,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  SET KEY = RECORD KEY                         
         MVC   AIO,FULL            RESTORE I/O AREA                             
*                                                                               
FINDOUTX CLI   ERROR,0                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR REPORT                                                  
*                                                                               
HDHK     NTR1                                                                   
*                                                                               
         CLI   USEHDHK,C'Y'                 TEST OK TO DO HEADHOOK              
         BNE   HDHKX                                                            
*                                                                               
         L     RF,AMYBOX                                                        
         USING BOXD,RF                                                          
         L     RF,BOXAWIDE                  A(WIDE DSECT)                       
         USING WIDED,RF                                                         
*                                                                               
         MVI   XHEAD2+1,HO                  UNDERLINE                           
         MVC   XHEAD2+2(26),XHEAD2+1                                            
         MVI   XHEAD3,0                                                         
*                                                                               
         SR    RE,RE                                                            
         CLI   WRIOUTLH+5,3                 TEST FLOAT OF NAMES                 
         BNH   *+12                                                             
         IC    RE,WRIOUTLH+5                                                    
         SH    RE,=H'3'                                                         
*                                                                               
         MVC   XHEAD4+9(L'WRICLI),WRICLI    CLIENT                              
         LA    R1,XHEAD4+14(RE)                                                 
         MVC   0(L'CLTNAM,R1),CLTNAM                                            
         MVC   XHEAD5+9(L'WRIPRD),WRIPRD    PRODUCT                             
         LA    R1,XHEAD5+14(RE)                                                 
         MVC   0(L'PRDNAM,R1),PRDNAM                                            
         MVC   XHEAD6+9(L'WRIPLAN),WRIPLAN  PLAN                                
         LA    R1,XHEAD6+14(RE)                                                 
         MVC   0(L'PLANNAM,R1),PLANNAM                                          
*                                                                               
         CLI   WRIOUTLH+5,0                 OUTLINE (IF PRESENT)                
         BE    HDHK5                                                            
         MVC   XHEAD7+1(7),=C'OUTLINE'                                          
         MVC   XHEAD7+9(L'WRIOUTL),WRIOUTL                                      
         LA    R1,XHEAD7+14(RE)                                                 
         MVC   0(L'WRIOUTD,R1),WRIOUTD                                          
*                                                                               
HDHK5    OC    PERIOD,PERIOD                PERIOD (IF PRESENT)                 
         BZ    HDHK20                                                           
         CLI   REPWIDTH,132                 RIGHT JUSTIFY THESE FIELDS          
         BNE   HDHK10                                                           
         MVC   XHEAD6+97(6),=C'PERIOD'                                          
         MVC   XHEAD6+104(L'PEREXP),PEREXP                                      
         B     HDHK20                                                           
*                                                                               
HDHK10   MVC   XHEAD6+130(6),=C'PERIOD'     WIDTH = 165                         
         MVC   XHEAD6+137(L'PEREXP),PEREXP                                      
*                                                                               
HDHK20   CLI   WRITITLH+5,0                 TITLE                               
         BNE   *+6                                                              
         DC    H'0'                         MUST BE THERE                       
         ZIC   R0,WRITITLH+5                                                    
         SRL   R0,1                                                             
         ZIC   R1,REPWIDTH                                                      
         SRL   R1,1                         DISP TO CENTER OF PAGE              
         SR    R1,R0                                                            
         LA    R2,XHEAD1(R1)                                                    
         ZIC   R1,WRITITLH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WRITITL                                                  
         LA    R2,198(R2)                                                       
         MVI   0(R2),HO                     UNDERLINE                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R2)                                                    
*                                                                               
         CLI   WRISTITH+5,0                 SUB-TITLE (IF PRESENT)              
         BE    HDHKX                                                            
         ZIC   R0,WRISTITH+5                                                    
         SRL   R0,1                                                             
         ZIC   R1,REPWIDTH                                                      
         SRL   R1,1                         DISP TO CENTER OF PAGE              
         SR    R1,R0                                                            
         LA    R2,XHEAD4(R1)                                                    
         ZIC   R1,WRISTITH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WRISTIT                                                  
*                                                                               
HDHKX    B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
*                                                                               
         SPROG 1                   FOR 132 CHAR WIDTH                           
*&&US*&& WSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& WSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         WSPEC H1,98,AGYNAME                                                    
         WSPEC H2,98,AGYADD                                                     
         WSPEC H4,2,C'CLIENT'                                                   
         WSPEC H4,98,RUN                                                        
         WSPEC H5,2,C'PRODUCT'                                                  
         WSPEC H5,98,PAGE                                                       
         WSPEC H5,109,REQUESTOR                                                 
         WSPEC H6,2,C'PLAN'                                                     
*                                                                               
         SPROG 2                   FOR 165 CHAR WIDTH                           
*&&US*&& WSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& WSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         WSPEC H1,131,AGYNAME                                                   
         WSPEC H2,131,AGYADD                                                    
         WSPEC H4,2,C'CLIENT'                                                   
         WSPEC H4,131,RUN                                                       
         WSPEC H5,2,C'PRODUCT'                                                  
         WSPEC H5,131,PAGE                                                      
         WSPEC H5,142,REQUESTOR                                                 
         WSPEC H6,2,C'PLAN'                                                     
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 3                                                                
INVOPT   MVC   CONHEAD,=CL60'* ERROR * OPTION INVALID *'                        
         CLI   MODE,PRINTREP                                                    
         BE    MYERRORP                                                         
         GOTO1 VSCANERR,DMCB,(42,SCANBLK),(2,OPTNUM)                            
*                                                                               
INVMON   MVC   CONHEAD,=CL60'* ERROR * INVALID MONTH EXPRESSION *'              
         B     MYERROR                                                          
*                                                                               
INVPER   MVC   CONHEAD,=CL60'* ERROR * INVALID PERIOD EXPRESSION *'             
         B     MYERROR                                                          
*                                                                               
INVWOSTK MVC   CONHEAD,=CL60'* ERROR * CAN ONLY USE WITH STACK *'               
         B     MYERROR                                                          
*                                                                               
INVHEAD  MVC   CONHEAD,=CL60'* ERROR * INVALID HEADING EXPRESSION *'            
         B     MYERROR                                                          
*                                                                               
TEXTERR  MVC   CONHEAD,=CL60'* ERROR * MAXIMUM OF 4 TEXT COLUMNS *'             
         B     MYERROR                                                          
*                                                                               
REFERR   MVC   CONHEAD,=CL60'* ERROR * ONLY ONE REFNUM OR REFLET COLUMN+        
                ALLOWED *'                                                      
         B     MYERROR                                                          
*                                                                               
TOOWIDE  MVC   CONHEAD,=CL60'* ERROR * REPORT WIDTH IS EXCEEDED *'              
         B     MYERROR                                                          
*                                                                               
INVDCON  MVC   CONHEAD,=CL60'* ERROR * ''PLAN'' OPTION MUST BE FIRST *'         
         B     MYERROR                                                          
*                                                                               
TOOSTACK MVC   CONHEAD,=CL60'* ERROR * ONLY 4 STACKED DTYPES ALLOWED *'         
         CLI   MODE,PRINTREP                                                    
         BE    MYERRORP                                                         
         GOTO1 VSCANERR,DMCB,(42,SCANBLK),(2,OPTNUM)                            
*                                                                               
NOSTACK  MVC   CONHEAD,=CL60'* ERROR * DTYPE NO. X NOT FOUND *'                 
         MVC   CONHEAD+20(1),SUBOPT                                             
         OI    CONHEAD+20,X'F0'                                                 
         CLI   MODE,PRINTREP                                                    
         BE    MYERRORP                                                         
         GOTO1 VSCANERR,DMCB,(42,SCANBLK),(2,OPTNUM)                            
*                                                                               
INVLEV   MVC   CONHEAD,=CL60'* ERROR * INVALID LEVEL *'                         
         CLI   MODE,PRINTREP                                                    
         BE    MYERRORP                                                         
         GOTO1 VSCANERR,DMCB,(42,SCANBLK),(2,OPTNUM)                            
*                                                                               
INVOUTL  MVC   CONHEAD,=CL60'* ERROR * OUTLINE NOT FOUND *'                     
         CLI   MODE,PRINTREP                                                    
         BE    MYERRORP                                                         
         GOTO1 VSCANERR,DMCB,(42,SCANBLK),(2,OPTNUM)                            
*                                                                               
NODATACL MVC   CONHEAD,=CL60'* ERROR * NO DATA COLUMNS GENERATED *'             
         B     MYERROR                                                          
*                                                                               
         EJECT                                                                  
MYERROR  CLI   MODE,PRINTREP                                                    
         BNE   MYERRORX                                                         
MYERRORP MVI   USEHDHK,C'N'        DON'T USE HEADHOOK                           
         L     RF,AMYBOX                                                        
         USING BOXD,RF                                                          
         L     RF,BOXAWIDE         A(WIDE PRINT DSECT)                          
         USING WIDED,RF                                                         
         MVI   XP1,0                                                            
         MVC   XP2+1(L'CONHEAD),CONHEAD                                         
         L     R3,ASPOOLD                                                       
         GOTO1 SPOOL,DMCB,(R3)                                                  
         B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
MYERRORX GOTO1 ERREX2                                                           
         SPACE 3                                                                
TRAPERR  CLI   MODE,PRINTREP                                                    
         BE    TRAPERRX                                                         
         GOTO1 ERREX                                                            
         SPACE 3                                                                
TRAPSCAN CLI   MODE,PRINTREP                                                    
         BE    TRAPERRX                                                         
         GOTO1 VSCANERR,DMCB,(42,SCANBLK),(0,OPTNUM)                            
         SPACE 3                                                                
TRAPERRX MVI   USEHDHK,C'N'        DON'T USE HEADHOOK                           
         L     RF,AMYBOX                                                        
         USING BOXD,RF                                                          
         L     RF,BOXAWIDE         A(WIDE PRINT DSECT)                          
         USING WIDED,RF                                                         
         MVI   XP1,0                                                            
         MVC   XP2(29),=C' ****************************'                        
         MVC   XP3(29),=C' * ERROR * RECORD NOT FOUND *'                        
         MVC   XP4(29),=C' ****************************'                        
         L     R3,ASPOOLD                                                       
         GOTO1 SPOOL,DMCB,(R3)                                                  
         B     EXIT                                                             
         DROP  RF                                                               
         SPACE 3                                                                
RELO     DS    A                   RELOCATION FACTOR                            
*&&US                                                                           
HO       EQU   X'BF'               HORIZONTAL LINE CHARACTER                    
*&&                                                                             
*&&UK                                                                           
HO       EQU   X'1C'                                                            
*&&                                                                             
BLANKS   DC    CL132' '                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*DTYPES*'                                                      
DTYPETAB DS    1500X               DATA TYPE TABLE                              
*                                                                               
         DS    0D                                                               
         DC    C'*BUFFER*'                                                      
BUFFER   DS    4096X               DRIVER ELEMENT BUFFER                        
         SPACE 5                                                                
         PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE BUWRIWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE BUWRIF2D                                                       
         EJECT                                                                  
* SAVE AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
ADRONE   DS    A                   A(DRONE)                                     
ADRIVER  DS    A                   A(DRIVER)                                    
AGLOBALS DS    A                   A(DRIVER GLOBALS AREA)                       
ADTYPE   DS    A                   A(NEXT LOCATION IN DTYPETAB)                 
AOUTTAB  DS    A                   A(OUTLINE TABLE)                             
AMYBOX   DS    A                   A(BOX AREA)                                  
BUILDPL  DS    5A                  BUILDER PARAMETER LIST                       
DTYPEDA  DS    XL4                 D/A OF DATA TYPE RECORD                      
CURWIDTH DS    H                   WIDTH OF REPORT                              
LEVELNUM DS    X                   OUTLINE LEVEL NUMBER                         
DTYPECD  DS    CL8                 DATA CODE                                    
NEXTLEV  DS    X                   NEXT OUTLINE LEVEL                           
NEXTNODE DS    XL4                 NODE ESTABLISHED BY PARENT                   
PERIOD   DS    XL4                 PERIOD START/END (YEAR/MONTH)                
COLPERD  DS    XL4                 PERIOD USED FOR THIS COLUMN                  
COLPERD2 DS    XL4                 ALTERNATE PERIOD FOR COLUMN                  
COLASAT  DS    XL3                 AS AT DATE USED FOR THIS COLUMN              
PEREXP   DS    CL16                PERIOD EXPRESSION                            
PERHEAD  DS    CL16                HEADING USED IN PERIOD FIELD                 
OPTNUM   DS    X                   CURRENT SCANNER OPTION NUMBER                
SUBOPT   DS    X                   SCANNER SUB-OPTION NUMBER                    
TEXTCOLS DS    X                   COUNT NUMBER OF TEXT COLUMNS                 
TEXTFLAG DS    CL1                 Y=COLUMN IS TEXT/REFLET(NUM)                 
REFFOUND DS    C                   IF 'Y' THEN REF DATA WAS FOUND               
COLFOUND DS    C                   IF 'Y' THEN THERE IS COLUMN DATA             
DATAFND  DS    C                   IF 'Y' THEN DATA COLUMN FOUND                
USEHDHK  DS    C                   IF 'Y' THEN OK TO USE HEADHOOK               
REPWIDTH DS    X                   MAX WIDTH OF REPORT (132 OR 165)             
STKDWID  DS    X                   WIDEST DATA WIDTH IN STACK                   
STKHWID  DS    X                   WIDEST HEADING WIDTH IN STACK                
NOEJECT  DS    C                   IF 'Y' DON'T EJECT AFTER OUTLINES            
PLANKEY  DS    XL16                PLAN NODAL KEY                               
ACCDEF   DS    XL16                ACCUM DEFINITION LINE                        
SCANBLK  DS    512X                SCANNER BLOCK (ROOM FOR 8 ENTRIES)           
STKDTYPS DS    XL137               TABLE OF STACKED DATA TYPES (MAX 4)          
         SPACE 5                                                                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
SAVNKEY  DS    CL(L'NODKEY)        OUTLINE'S NODAL KEY                          
SVPARKEY DS    CL(L'NODKEY)        PARENT'S NODAL KEY                           
*                                                                               
SVCLTVAL DS    CL(L'CLTVALS)       CLIENT VALUES SAVE AREA                      
SVPRDVAL DS    CL(L'PRDVALS)       PRODUCT VALUES SAVE AREA                     
SVPLNVAL DS    CL(L'PLANVALS)      PLAN VALUES SAVE AREA                        
SVOUTVAL DS    CL(L'OUTVALS)       OUTLINE VALUES SAVE AREA                     
         EJECT                                                                  
       ++INCLUDE DRONEBLKD                                                      
         SPACE 3                                                                
       ++INCLUDE DRONEBLKHD                                                     
         EJECT                                                                  
       ++INCLUDE BUILDERD                                                       
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
STACKD   DSECT                                                                  
*                                                                               
STKDTYPE DS    CL8                 DATA TYPE CODE                               
STKDSKAD DS    XL4                 DISK ADDRESS                                 
STKPOSNO DS    X                   STACK POSITION NUMBER (1,2,3,4)              
STKINDS  DS    X                   INDICATORS                                   
STKIPER  EQU   X'80'               PERIOD OVERRIDE MAY BE INPUT                 
STKHEAD  DS    CL20                HEADLINE                                     
STACKLEN EQU   *-STACKD                                                         
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032BUWRI02   05/01/02'                                      
         END                                                                    
