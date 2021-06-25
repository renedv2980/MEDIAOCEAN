*          DATA SET CTSFM0D    AT LEVEL 030 AS OF 08/25/05                      
*PHASE TA0A0DA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*                                                                               
* DEMO FOR JUSTIN                                                               
* DEMO FOR JUSTIN                                                               
* HOORAY                                                                        
* UH-OH                                                                         
* INSERTED ON 8/25 FOR TEST                                                     
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM0D -- SCROLLER DATATYPE MAINTENANCE/LIST/REPORT *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMFD (MAINTENANCE)                        *         
*                        CTSFMED (LIST)                               *         
*                        CTSFMDD (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED DATATYPE RECORDS, LIST, OR REPORT.           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A0D - SCROLLER DATATYPE RECORD MAINT/LIST/REPORT'            
TA0A0D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A0D**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
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
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       CLI   ACTEQU,ACTREP       REPORT ACTION?                               
         BE    VK10                                                             
*                                                                               
         XC    SYSPROG,SYSPROG     SYSTEM/PROGRAM CODES                         
         LA    R2,DTMSYPGH                                                      
         CLI   DTMSYPGH+5,0        ANY DATA?                                    
         BNE   *+14                YES                                          
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         CLI   DTMSYPGH+5,5        MUST BE EXACTLY 5 CHARACTERS LONG            
         BE    *+14                YES                                          
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   SYSPROG,DTMSYPG     SAVE SYSTEM/PROGRAM                          
*                                                                               
         XC    DATATYPE,DATATYPE   DATATYPE NAME                                
         LA    R2,DTMDTYPH                                                      
         CLI   DTMDTYPH+5,0        ANY DATA?                                    
         BNE   *+22                                                             
         CLI   ACTEQU,ACTLIST      ON-SCREEN LIST ACTION?                       
         BE    VK20                YES                                          
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   GERROR,=AL2(RESERVQ)                                             
         CLC   =C'USER',WORK       DISALLOW RESERVED NAMES                      
         BE    VSFMERR                                                          
         CLC   =C'PF',WORK                                                      
         BE    VSFMERR                                                          
         CLC   =C'SEL',WORK                                                     
         BE    VSFMERR                                                          
         CLC   =C'KEY',WORK                                                     
         BE    VSFMERR                                                          
         CLC   =C'TEXT',WORK                                                    
         BE    VSFMERR                                                          
         CLC   =C'START  ',WORK                                                 
         BE    VSFMERR                                                          
         CLC   =C'END    ',WORK                                                 
         BE    VSFMERR                                                          
*                                                                               
         XC    GERROR,GERROR                                                    
         MVC   DATATYPE,WORK       SAVE DATATYPE NAME                           
         B     VK20                                                             
*                                                                               
VK10     XC    SYSPROG,SYSPROG     SYSTEM/PROGRAM CODES                         
         LA    R2,DTPSYPGH                                                      
         CLI   DTPSYPGH+5,0        ANY DATA?                                    
         BNE   *+14                YES                                          
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
         GOTO1 ANY                 GET KEY                                      
         CLC   =C'ALL     ',WORK   'ALL' SYSTEMS/PROGRAMS?                      
         BE    *+10                                                             
         MVC   SYSPROG,WORK                                                     
*                                                                               
         XC    DATATYPE,DATATYPE                                                
*                                                                               
VK20     LA    R4,KEY              BUILD KEY                                    
         USING DTYPKEYD,R4                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   DTYPSYS,DTYPSYSQ    SYSTEM                                       
         MVI   DTYPTYP,DTYPTYPQ    RECORD TYPE                                  
         MVC   DTYPSYPG,SYSPROG    SYSTEM/PROGRAM                               
         MVC   DTYPCODE,DATATYPE   DATATYPE NAME                                
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R4,AIO              BUILD RECORD                                 
         USING DTYPKEY,R4                                                       
         MVC   SYSPROG,DTYPSYPG    RECORD TYPE                                  
         MVC   DATATYPE,DTYPCODE                                                
*                                                                               
         MVI   ELCODE,DTNAMELQ     DESCRIPTION (NAME) ELEMENT                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING DTNAMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   DTNAMEL,DTNAMELQ                                                 
         MVI   DTNAMLN,DTNAMLNQ                                                 
         LA    R2,DTMDESCH                                                      
         GOTO1 ANY                                                              
         MVC   DTNAME,WORK         ACTUAL DESCRIPTION                           
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,DTGENELQ     GENERAL INFO ELEMENT                         
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING DTGEND,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   DTGENEL,DTGENELQ                                                 
         MVI   DTGENLN,DTGENLNQ                                                 
*                                                                               
         LA    R2,DTMIDH           ID NUMBER                                    
         CLI   DTMIDH+5,0                                                       
         BE    *+12                                                             
         BAS   RE,VNUM                                                          
         STC   R1,DTGENIDN                                                      
*                                                                               
         LA    R2,DTMCASEH         CASE                                         
         CLI   DTMCASEH+5,0        CASE GIVEN?                                  
         BNE   *+16                                                             
         MVI   DTMCASE,C'U'        NO -- DEFAULT TO UPPER ONLY                  
         MVI   DTMCASEH+5,1        FUDGE INPUT LENGTH                           
         OI    DTMCASEH+6,X'80'    XMIT                                         
         CLI   DTMCASE,C'U'        UPPER CASE ONLY?                             
         BE    *+22                YES                                          
         CLI   DTMCASE,C'L'        UPPER/LOWER CASE?                            
         BE    *+14                YES                                          
         MVC   GERROR,=AL2(BADCASEQ)                                            
         B     VSFMERR                                                          
         MVC   DTGENDUL,DTMCASE                                                 
*                                                                               
         LA    R2,DTMSLENH         ON-SCREEN LENGTH                             
         GOTO1 ANY                                                              
         BAS   RE,VNUM                                                          
         STC   R1,DTGENOSL                                                      
*                                                                               
         LA    R2,DTMDLENH         DATA TABLE LENGTH                            
         GOTO1 ANY                                                              
         BAS   RE,VNUM                                                          
         STC   R1,DTGENDTL                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,DTNMRELQ     NAME TEXT ELEMENT (ROW DISPLAY)              
         GOTO1 REMELEM                                                          
         LA    R2,DTMRHEDH                                                      
         CLI   DTMRHEDH+5,0                                                     
         BE    VR10                                                             
         LA    R6,ELEM                                                          
         USING DTNMRD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   DTNMREL,DTNMRELQ                                                 
         MVI   DTNMRLN,DTNMRLNQ                                                 
         MVC   DTNMRLEN,DTMRHEDH+5 NUMBER OF CHARS IN ROW NAME                  
         MVC   DTNMRTXT,DTMRHED    ACTUAL ROW NAME                              
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR10     MVI   ELCODE,DTNMCELQ     NAME TEXT ELEMENT (COLUMN DISPLAY)           
         GOTO1 REMELEM                                                          
         CLI   DTMCHD1H+5,0        ANY COLUMN HEADINGS?                         
         BNE   *+12                                                             
         CLI   DTMCHD2H+5,0                                                     
         BE    VR20                                                             
*                                                                               
         LA    R6,ELEM             YES                                          
         USING DTNMCD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   DTNMCEL,DTNMCELQ                                                 
         MVI   DTNMCLN,DTNMCLNQ                                                 
*                                                                               
         LA    R2,DTMCHD1H         TOP COLUMN HEADING                           
         CLI   DTMCHD1H+5,0                                                     
         BE    *+16                                                             
         MVC   DTNMCLN1,DTMCHD1H+5                                              
         MVC   DTNMCTX1,DTMCHD1                                                 
*                                                                               
         LA    R2,DTMCHD2H         BOTTOM COLUMN HEADING                        
         CLI   DTMCHD2H+5,0                                                     
         BE    *+16                                                             
         MVC   DTNMCLN2,DTMCHD2H+5                                              
         MVC   DTNMCTX2,DTMCHD2                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR20     B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE FIELD FOR NUMERIC                                                    
*                                                                               
VNUM     TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+14                                                             
         MVC   GERROR,=AL2(NOTNUM)                                              
         B     VSFMERR                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'1'                                                         
         BNL   *+14                                                             
         MVC   GERROR,=AL2(BADRANGQ)                                            
         B     VSFMERR                                                          
         CH    R1,=H'255'                                                       
         BNHR  RE                                                               
         MVC   GERROR,=AL2(BADRANGQ)                                            
         B     VSFMERR                                                          
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R0,DTMTAGH          LAST FIELD ON SCREEN                         
         LA    R2,DTMDESCH         FIRST FIELD HEADER                           
*                                                                               
DR10     ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R1,=H'17'           MINUS HEADER, EXTEN, AND 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DR20     ZIC   R1,0(R2)            RESTORE LENGTH                               
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BE    *+16                                                             
         TM    1(R2),X'20'         NO -- FIELD IS PROTECTED?                    
         BZ    DR10                NO -- CLEAR IT                               
         B     DR20                YES -- BUMP TO NEXT FIELD                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNAMELQ     DESCRIPTION                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DTNAMD,R6                                                        
         MVC   DTMDESC,DTNAME                                                   
         OI    DTMDESCH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTGENELQ     GENERAL INFORMATION                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DTGEND,R6                                                        
         EDIT  DTGENIDN,(3,DTMID),ALIGN=LEFT                                    
         OI    DTMIDH+6,X'80'                                                   
         MVC   DTMCASE,DTGENDUL                                                 
         OI    DTMCASEH+6,X'80'                                                 
         EDIT  DTGENOSL,(2,DTMSLEN),ALIGN=LEFT                                  
         OI    DTMDLENH+6,X'80'                                                 
         EDIT  DTGENDTL,(3,DTMDLEN),ALIGN=LEFT                                  
         OI    DTMDLENH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNMRELQ     NAME TEXT (ROW DISPLAY)                      
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         USING DTNMRD,R6                                                        
         MVC   DTMRHED,DTNMRTXT                                                 
         OI    DTMRHEDH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNMCELQ     NAME TEXT (COLUMN DISPLAY)                   
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING DTNMCD,R6                                                        
         MVC   DTMCHD1,DTNMCTX1    TOP                                          
         OI    DTMCHD1H+6,X'80'                                                 
         MVC   DTMCHD2,DTNMCTX2    BOTTOM                                       
         OI    DTMCHD2H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              SELECTED RECORD                              
         USING DTYPKEYD,R4                                                      
*                                                                               
         MVC   DTLSYPG,DTYPSYPG    SYSTEM/PROGRAM                               
         OI    DTLSYPGH+6,X'80'                                                 
         MVC   DTLDTYP,DTYPCODE    DATATYPE NAME                                
         OI    DTLDTYPH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* ON-SCREEN LIST                                                                
*                                                                               
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   DTYPSYS,DTYPSYSQ    SYSTEM                                       
         MVI   DTYPTYP,DTYPTYPQ    RECORD TYPE                                  
         MVC   DTYPSYPG,SYSPROG    SYSTEM/PROGRAM                               
         MVC   DTYPCODE,DATATYPE   DATATYPE NAME                                
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(17),SAVEKEY     SAME SYSTEM/PROGRAM?                         
         BNE   LRX                 NO MORE DATATYPES TO LIST                    
*                                                                               
         GOTO1 GETREC              GET THE SCROLLER DATATYPE RECORD             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTDTYPE,DTYPCODE   PUT DATATYPE NAME IN LIST LINE               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNAMELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DTNAMD,R6                                                        
         MVC   LSTDESC,DTNAME      PUT DESCRIPTION IN LIST LINE                 
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* OFFLINE PRINT                                                                 
*                                                                               
PR       LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    PR10                NO                                           
         USING BOXD,RF                                                          
         LA    R1,BXHOOK           BOX HOOK                                     
         ST    R1,BOXHOOK                                                       
         DROP  RF                                                               
*                                                                               
PR10     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   DTYPSYS,DTYPSYSQ    SYSTEM                                       
         MVI   DTYPTYP,DTYPTYPQ    RECORD TYPE                                  
         MVC   DTYPSYPG,SYSPROG    SYSTEM/PROGRAM                               
         MVC   DTYPCODE,DATATYPE   DATATYPE NAME                                
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      DTYPE RECORD?                                
         BNE   PRX                 NO MORE DATATYPES TO REPORT                  
*                                                                               
         OC    SYSPROG,SYSPROG     ANY SYSTEM/PROGRAM FILTER?                   
         BZ    *+14                                                             
         CLC   SYSPROG,DTYPSYPG    YES - MATCH ON SYSTEM/PROGRAM                
         BNE   PRX                 NO                                           
*                                                                               
         CLC   DTYPSYPG,(DTYPSYPG-DTYPKEY)+KEYSAVE                              
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       EJECT PAGE ON SYSTEM/PROGRAM CHANGE          
*                                                                               
         GOTO1 GETREC              GET THE SCROLLER DATATYPE RECORD             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         MVC   PRTDTYPE,DTYPCODE   PUT DATATYPE NAME IN PRINT LINE              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNAMELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DTNAMD,R6                                                        
         MVC   PRTDESC,DTNAME      PUT DESCRIPTION IN PRINT LINE                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTGENELQ     GENERAL INFORMATION                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DTGEND,R6                                                        
         EDIT  DTGENIDN,(3,PRTID)                                               
         MVC   PRTCASE,=C'U/L  '                                                
         CLI   DTGENDUL,C'U'                                                    
         BNE   *+10                                                             
         MVC   PRTCASE,=C'UPPER'                                                
         EDIT  DTGENOSL,(2,PRTSLEN)                                             
         EDIT  DTGENDTL,(3,PRTDLEN)                                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNMRELQ     NAME TEXT (ROW DISPLAY)                      
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         USING DTNMRD,R6                                                        
         MVC   PRTRHED,DTNMRTXT                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DTNMCELQ     NAME TEXT (COLUMN DISPLAY)                   
         BAS   RE,GETEL                                                         
         BNE   PR40                                                             
         USING DTNMCD,R6                                                        
         MVC   PRTCHD1,DTNMCTX1    TOP                                          
         MVC   PRTCHD2,DTNMCTX2    BOTTOM                                       
         DROP  R6                                                               
*                                                                               
PR40     GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
         B     PR20                NEXT RECORD                                  
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H1+55(19),=C'DTYPE RECORD REPORT'                                
         MVI   H2+55,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+56(18),H2+55                                                  
         MVC   H4+8(L'DTYPSYPG),DTYPSYPG   SYSTEM/PROGRAM                       
*                                                                               
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+33,C'C'                                                  
         MVI   BOXCOLS+37,C'C'                                                  
         MVI   BOXCOLS+43,C'C'                                                  
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+101,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
HDHOOKX  B     XIT                                                              
         SPACE 3                                                                
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H4,1,C'SYS.PRG'                                                  
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,2,C'DTYPE   DESCRIPTION'                                      
         SSPEC H7,35,C'ID# CASE  LENGTH'                                        
         SSPEC H7,74,C'NAMES'                                                   
         SSPEC H9,45,C'SC TAB'                                                  
         SSPEC H9,63,C'ROW             TOP COL     BOTTOM COL'                  
         DC    X'00'                                                            
         EJECT                                                                  
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
BADRANGQ EQU   227                                                              
RESERVQ  EQU   228                                                              
BADCASEQ EQU   229                                                              
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* BOX HOOK ROUTINE                                                              
*                                  P1, BYTE 0 = ROW CHARACTER                   
*                                  P1, BYTES 1-3 = A(PRINT LINE)                
*                                  P2, BYTE 0 = LINE NUMBER                     
*                                  P2, BYTES 1-3 = A(BOXD)                      
*                                                                               
BXHOOK   NMOD1 0,**BXHK**                                                       
*                                                                               
         CLI   4(R1),8             UP TO ROW 8?                                 
         BNE   BXHOOKX                                                          
*                                                                               
         L     RF,4(R1)            A(BOX AREA)                                  
         USING BOXD,RF                                                          
         MVI   BOXCOLS+46,C'C'     NOW WE ADD MORE COLUMNS                      
         MVI   BOXCOLS+75,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXREQ,C'N'         SET NEW CONFIGURATION FLAG                   
         DROP  RF                                                               
*                                                                               
BXHOOKX  XIT1                                                                   
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMFDD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMEDD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMDDD                                                       
         EJECT                                                                  
       ++INCLUDE CTGENDTYPE                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
SYSPROG  DS    0CL5                SYSTEM/PROGRAM                               
SYSCODE  DS    CL2                                                              
PROGCODE DS    CL3                                                              
DATATYPE DS    CL7                 DATATYPE NAME                                
SAVEKEY  DS    XL32                GENFILE KEY                                  
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTDTYPE DS    CL7                 DTYPE NAME                                   
         DS    CL3                                                              
LSTDESC  DS    CL24                DESCRIPTION                                  
         SPACE 5                                                                
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRTDTYPE DS    CL7                 DTYPE NAME                                   
         DS    C                                                                
PRTDESC  DS    CL24                DESCRIPTION                                  
         DS    C                                                                
PRTID    DS    CL3                 ID NUMBER                                    
         DS    C                                                                
PRTCASE  DS    CL5                 CASE                                         
         DS    C                                                                
PRTSLEN  DS    CL2                 ON-SCREEN LENGTH                             
         DS    C                                                                
PRTDLEN  DS    CL3                 DATA TABLE LENGTH                            
         DS    C                                                                
PRTRHED  DS    CL24                ROW NAME                                     
         DS    C                                                                
PRTCHD1  DS    CL12                TOP COLUMN NAME                              
         DS    C                                                                
PRTCHD2  DS    CL12                BOTTOM COLUMN NAME                           
         DS    C                   RIGHT BOX MARGIN                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030CTSFM0D   08/25/05'                                      
         END                                                                    
