*          DATA SET ACCAP15    AT LEVEL 024 AS OF 07/07/20                      
*PHASE T61D15B                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP15 -- ALLOCATION PROFILE REPORT                 *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS                  *         
*                                                                     *         
*  OUTPUTS:      REPORT ON PRINT Q                                    *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- RELO REGISTER                                  *         
*                R4 -- PRINT LINE                                     *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD - BASE SAVED STORAGE                      *         
*                RA -- GEND                                           *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
* DCUR L 013  TOOK OUT SAVED ADDR OF GETCAP (COR-RES NOW).  RELINKED  *         
*             TO PICK UP CHANGE TO ACCAPROTAB(OVH PROFILE NOW VALID   *         
*             DOWN TO LOWEST LEVEL).  ADDED CODE TO PRINT 2ND LINE OF *         
*             PROFILE SETTINGS.  MOVED COPFTABD & COPPD DSECTS TO A   *         
*             GLOBAL PANBOOK.                                                   
* **** ******* 014 BAL/R -> BAS/R CONVERSION                                    
* JFOS 13MAR06 015 <LO01-4584> CURED:VARIABLE N'DPS, NEW MCS CATEGS             
* TFRY 02JUL08 016 <LO01-7858> WHERE THERE IS A PROFILE FILTER SAVE             
*              MINIMUM LENGTH INSTEAD OF INPUT LENGTH SO THAT REPORT            
*              WILL INCLUDE DATA WHERE SHORT NAME IS ENTERED                    
* MPEN 29JUL09 <LO01-8967> ADD NEW PROFILES FOR EXP MODULE IN COST    *         
* MPEN 23SEP09 <LO10-9348> NEW PROFILE TO SET RECEIPT BY DEFAULT      *         
*      24SEP09 LO01-9351 TIME LOCK AND REMINDERS FOR GREY             *         
***********************************************************************         
         TITLE 'T61D15 - ALLOCATION PROFILE REPORT'                             
T61D15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D15**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(BASE SAVED STORAGE)                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*               VALIDATE KEY                                          *         
***********************************************************************         
         SPACE 1                                                                
VK       XC    SAVEKEY,SAVEKEY     INITIALIZE                                   
         XC    SAVEKEY2,SAVEKEY2                                                
         XC    PROFILT,PROFILT                                                  
         XC    GRPFILT,GRPFILT                                                  
         MVC   METHCODE,SPACES                                                  
         MVC   METHNUM,SPACES                                                   
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
         MVC   PERSON,SPACES                                                    
         MVC   ACCNT,SPACES                                                     
         MVI   LEVBIT,LEVDFLT      DEFAULT LEVEL                                
         MVI   OPTBIT,0                                                         
         MVI   HEDLNBIT,0                                                       
         MVI   METHBIT,0                                                        
*                                                                               
         MVC   HD1LINE,SPACES                                                   
         MVC   HD2LINE,SPACES                                                   
         MVC   HD3LINE,SPACES                                                   
         MVC   HD4LINE,SPACES                                                   
         MVC   HD5LINE,SPACES                                                   
         MVC   HD6LINE,SPACES                                                   
         MVC   HD7LINE,SPACES                                                   
         MVC   HD8LINE,SPACES                                                   
*                                                                               
         GOTO1 GETLDG,DMCB,C'1R'   GET APPROPRIATE FIELD LENGTHS                
         MVC   LEVELLN,ABCDLEN                                                  
         MVC   SVLEV1NM,LDGNM1                                                  
         MVC   SVLEV2NM,LDGNM2                                                  
         MVC   SVLEV3NM,LDGNM3                                                  
         MVC   SVLEV4NM,LDGNM4                                                  
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   OFFLINE?                                     
         BNZ   VK05                DO NOT TEST                                  
         TM    TRANSTAT,RACHANG    RECORD AND/OR ACTION CHANGED?                
         BNZ   VK02                YES-ENTER MESSAGE                            
         LA    R2,CONWHENH                                                      
         TM    4(R2),X'80'         PRINT OPT ENTERED THIS TIME                  
         BZ    VK05                                                             
*                                                                               
VK02     LA    R2,PRRMETHH         ENTER REPORT DATA AND PRESS ENTER            
         B     ERRENDT             TO PROCESS REQUEST                           
         OI    6(R2),X'40'                                                      
*                                                                               
***********************************************************************         
*              VALIDATE METHOD                                                  
***********************************************************************         
*                                  SEE IF METHOD WAS SPECIFIED                  
VK05     LA    R2,PRRMETHH                                                      
*                                                                               
VK06     CLI   5(R2),0             ANY DATA?                                    
         BNE   VK07                                                             
         OI    METHBIT,NOMETH                                                   
         B     VK40                                                             
*                                                                               
VK07     CLC   AC@ALLU,PRRMETH                                                  
         BNE   VK09                                                             
         OI    METHBIT,ALLMETH                                                  
         B     VK40                                                             
*                                                                               
VK09     TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   VK10                                                             
*                                                                               
*----------------------------------------------------------------------         
*                                  READ RECORD BY METHOD NUMBER                 
*                                                                               
         CLI   5(R2),1             ONLY 1-9                                     
         BNE   EINVMET                                                          
         MVC   METHNUM,8(R2)                                                    
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
         B     VK20                                                             
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                  READ RECORD BY METHOD CODE                   
*                                                                               
VK10     MVC   METHCODE,PRRMETH    SAVE METHOD CODE                             
         OC    METHCODE,SPACES                                                  
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
         DROP  R6                                                               
*                                                                               
*----------------------------------------------------------------------         
*                                  GET BOTH METHNUM AND METHCODE                
*                                                                               
VK20     DS    0H                                                               
         USING METELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   ERRINV                                                           
*                                                                               
         MVC   METHNUM,METNUM      SAVE BOTH NUMBER AND CODE                    
         MVC   METHCODE,METCODE                                                 
         OI    METHBIT,SPECMETH    A SPECIFIC METHOD FILTER                     
         MVC   PRRMETH,METCODE                                                  
         OI    PRRMETHH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*    VALIDATE OFFICE/DEPT/SUBDPT/PERSON - CAN ENTER UP TO ANY LEVEL             
*----------------------------------------------------------------------         
*                                                                               
VK40     LA    R3,ACCNT            BUILD ACCOUNT IN ACCNT TO READ 1R            
         LA    R2,PRROFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    VK45                                                             
*                                                                               
VK42     CLC   PRROFFH+5(1),LEVELLN         SHOULD = LEN LEVEL A ACC            
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),PRROFF    SAVE OFFICE                                  
         OI    LEVBIT,LEVOFF       OFFICE LEVEL                                 
         OI    LEVBIT,LEVGRP       OFFICE GROUP LEVEL                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PRROFF     BUILD ACCOUNT TO READ 1R                      
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
*                                                                               
VK45     LA    R2,PRRDEPTH         ANY DEPARTMENT                               
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*                                                                               
         TM    LEVBIT,LEVOFF       MUST HAVE OFFICE                             
         BZ    EMISHIGH                                                         
*                                                                               
VK47     CLC   5(1,R2),LEVELLN+1                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPT(0),8(R2)                                                    
         OI    LEVBIT,LEVDPT       DEPT LEVEL                                   
         EX    R1,*+8              BUILD ACCOUNT TO READ 1R                     
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       R3 POINTS TO RIGHT PLACE IN ACCNT            
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
*                                                                               
VK50     LA    R2,PRRSDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0                                                          
         BE    VK55                                                             
*                                                                               
         CLC   5(1,R2),LEVELLN+2                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUBDPT(0),8(R2)                                                  
         OI    LEVBIT,LEVSDPT      SUB DEPT LEVEL                               
         EX    R1,*+8              BUILD ACCOUNT TO READ 1R                     
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       R3 POINTS TO RIGHT PLACE IN ACCNT            
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
*                                                                               
VK55     LA    R2,PRRPERH          ANY PERSON                                   
         CLI   5(R2),0                                                          
         BE    VK58                                                             
*                                                                               
         TM    LEVBIT,LEVOFF+LEVDPT+LEVSDPT                                     
         BNO   EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+3                                                
         BH    ETOOLONG                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSON(0),8(R2)                                                  
         OI    LEVBIT,LEVPER       PERSON LEVEL                                 
         EX    R1,*+8              BUILD ACCOUNT TO READ 1R                     
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       R3 POINTS TO RIGHT PLACE IN ACCNT            
*                                                                               
VK58     LA    R2,PRROFFH          CURSOR AT OFFICE ON ERROR                    
         BAS   RE,VALACCNT                                                      
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*               VALIDATE PROFILE FILTER                                         
*----------------------------------------------------------------------         
*                                                                               
VK60     XC    CODISP1,CODISP1     DISPLACEMENT TO ENTRY IN COBLOCK             
*                                                                               
         LA    R2,PRRPFLTH         ANY PROFILE FILTER?                          
         CLI   5(R2),0                                                          
         BE    VK80                                                             
*                                                                               
         LA    R3,PROFTAB          CHECK FOR MATCH IN PROFILE TABLE             
         USING COPFTABD,R3                                                      
*                                                                               
VK65     L     R0,APDSLIST                                                      
         ZICM  R1,COSHORT,2        DISP TO SHORT NAME                           
         AR    R1,R0                                                            
         MVC   SHRTNAME,0(R1)                                                   
         ZICM  R1,COLONG,2         DISP TO LONG NAME                            
         AR    R1,R0                                                            
         MVC   LONGNAME,0(R1)                                                   
*                                                                               
         CLC   SHRTNAME,8(R2)      MATCH ON SHORT NAME?                         
         BE    VK70                YEP                                          
*                                                                               
         ZIC   R1,COMIN            MIN COMPARE FOR LONG NAME                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LONGNAME(0),8(R2)   MATCH ON LONG NAME                           
         BE    VK70                YEP                                          
*                                                                               
         LA    R3,COTBLEN(R3)      CHECK NEXT ENTRY                             
         CLI   0(R3),X'00'         END OF TABLE                                 
         BNE   VK65                                                             
         B     EINVPRO                                                          
*                                                                               
VK70     DS    0H                                                               
         MVC   PRRPFLT,LONGNAME                                                 
         OI    6(R2),X'80'         XMIT LONG NAME                               
         MVC   PROFILT,SHRTNAME    SAVE FILTER                                  
         MVC   5(1,R2),COMIN                                                    
         LA    R1,PROFTAB                                                       
         SR    R3,R1               SAVE DISPLACEMENT INTO PROFILE TABLE         
         STH   R3,PROFDISP                                                      
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*          VALIDATE OPTIONS                                                     
***********************************************************************         
*                                                                               
VK80     DS    0H                                                               
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
         BAS   RE,VALOPTS2         VALIDATE SHOWALL,ACTIVITY & DEFAULT          
*                                  OPTIONS                                      
         EJECT                                                                  
***********************************************************************         
* BUILD KEY                                                                     
***********************************************************************         
*                                                                               
         USING CAPRECD,R6                                                       
VK100    LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
*                                                                               
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PROFILE REC                                  
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKMTHD,METHNUM                                                 
         MVC   CAPKOFC,OFFICE                                                   
         MVC   CAPKDPT,DEPT                                                     
         MVC   CAPKSDT,SUBDPT                                                   
         MVC   CAPKPER,PERSON                                                   
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PR       DS    0H                                                               
         LA    R2,CONWHENH                                                      
         CLC   REMUSER,=C'***'     HAS USER PROVIDED AN ID?                     
         BH    *+12                YES                                          
         STCM  R2,15,ACURFORC      FORCE THE CURSOR ON PRINT FIELD              
         B     ERRINIT                                                          
*                                                                               
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         LA    RF,HEDSPEC                                                       
         ST    RF,SPECS            SET A(HEADLINE SPECS)                        
         TM    OPTBIT,OPTACT       IS ACTIVITY OPTION ON?                       
         BZ    *+12                NO                                           
         LA    RF,HEDSPEC2                                                      
         ST    RF,SPECS                                                         
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK         SET A(HEADLINE HOOK)                         
*                                                                               
         OC    PROFILT,PROFILT                                                  
         BZ    *+14                                                             
         MVC   BIGKEY,SAVEKEY                                                   
         B     PRHI                                                             
*                                                                               
**** FORCE ANY HIGHER LEVELS TO PRINT FIRST                                     
         BAS   RE,GOGETCAP                                                      
         BAS   RE,BLDNAME                                                       
         BAS   RE,GTMTHNME                                                      
         BAS   RE,GOPRINT                                                       
         MVI   FORCEHED,C'Y'       EJECT PAGE ON NEW RECORD                     
*                                                                               
PR05     MVC   BIGKEY,SAVEKEY   KEY FROM VALKEY                                 
*                                                                               
PRHI     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    PR10                                                             
         DC    H'0'                                                             
*                                                                               
PRSEQ    DS    0H                                                               
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR10                                                             
         DC    H'0'                                                             
*                                                                               
PR10     CLC   BIGKEY(3),SAVEKEY                                                
         BNE   PRX                 NO MORE TO LIST                              
*                                                                               
**** HAS THIS RECORD ALREADY BEEN HANDLED BY GETCAP?                            
PR11     CLC   BIGKEY(L'SAVEKEY2),SAVEKEY2                                      
         BE    PRSEQ                                                            
*                                                                               
PR15     LA    R6,BIGKEY           CHECK FOR FILTERS                            
         USING CAPRECD,R6                                                       
         TM    METHBIT,ALLMETH     SHOW FOR ALL METHODS?                        
         BO    PR20                YES, SKIP TO OFFICE                          
*                                                                               
         TM    METHBIT,NOMETH      SHOW FOR NO METHODS?                         
         BZ    *+18                NO, CHECK FOR A SPECIFIC METH                
         CLC   CAPKMTHD,SPACES                                                  
         BNE   PRSEQ                                                            
         B     PR20                                                             
*                                                                               
         TM    METHBIT,SPECMETH    SHOW FOR A SPECIFIC METHOD?                  
         BNO   PR20                NO                                           
         CLC   CAPKMTHD,METHNUM    MATCH                                        
         BNE   PRSEQ                                                            
*                                                                               
PR20     TM    LEVBIT,LEVOFF       ANY OFFICE SPECIFIED                         
         BNO   PR22                NO                                           
         CLC   CAPKOFC,OFFICE      MATCH                                        
         BNE   PRSEQ                                                            
*                                                                               
PR22     TM    LEVBIT,LEVDPT       ANY DEPT SPECIFIED                           
         BNO   PR24                NO                                           
         CLC   CAPKDPT,DEPT        MATCH                                        
         BNE   PRSEQ                                                            
*                                                                               
PR24     TM    LEVBIT,LEVSDPT      ANY SUB DEPT SPECIFIED                       
         BNO   PR30                NO                                           
         CLC   CAPKSDT,SUBDPT      MATCH                                        
         BNE   PRSEQ                                                            
*                                                                               
PR30     OC    PROFILT,PROFILT                                                  
         BZ    PR35                                                             
         LA    R3,PROFTAB                                                       
         AH    R3,PROFDISP                                                      
         BAS   RE,SHOWPROF                                                      
         BNE   PRSEQ                                                            
         BAS   RE,PROFSET                                                       
         BNE   PRSEQ                                                            
         MVC   HD6LINE,SPACES                                                   
         MVC   HD6TITLE,AC@PRFL                                                 
         MVC   HD6FIELD(L'LONGNAME),LONGNAME                                    
*                                                                               
PR35     BAS   RE,GOGETCAP                                                      
         BAS   RE,BLDNAME          BUILD NAME FOR 1R HEADLINES                  
         BAS   RE,GTMTHNME         SET UP METHOD HEADLINE                       
         BAS   RE,GOPRINT          SET TO PRINT                                 
         MVI   FORCEHED,C'Y'       EJECT PAGE ON NEW RECORD                     
         B     PRSEQ               CONTINUE                                     
*                                                                               
PRX      XC    CONSERV,CONSERV                                                  
         MVC   CONSERV(4),=C'$DQU'                                              
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
********************************************************************            
*  SEND FIELDS TO PRINT                                                         
********************************************************************            
         SPACE 1                                                                
GOPRINT  NTR1                                                                   
         USING COPFTABD,R3                                                      
         LA    R3,PROFTAB          R3 POINTER TO PROFILE TABLE                  
*                                                                               
GOPR10   DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         STCM  R3,15,DISCPADR      SAVE DISPL INTO PROFILE TABLE                
         MVC   SVCODRTN,CODRTN     SAVE DISPLAY ROUTINE #                       
         MVC   SVCOBLEN,COBLEN     SAVE MAX LEN OF COBLCK FIELD                 
         MVC   SVCOEXTR,COEXTRA    SAVE N'DECIMAL PLACES FOR EDITTING           
         MVC   SVCODISP,CODISP     DISP INTO COBLOCK                            
         TM    OPTBIT,OPTSHOW      SHOW ALL PROFILES                            
         BNO   GOPR15                                                           
         TM    COFLAG,NOSHOW       DO NOT SHOW THIS PROFILE IN SHOWALL?         
         BO    GOPR90              DO NOT SHOW                                  
         B     GOPR25                                                           
GOPR15   TM    OPTBIT,OPTGRP       SHOW PROFILE FOR A CERTAIN GROUP?            
         BZ    GOPR20              NO                                           
         CLI   GRPFILT,COTMESHT    IS IT THE 'TIME' GROUP?                      
         BE    GOPR20              IF SO, NO METHOD IS NEEDED                   
         CLI   GRPFILT,COMCSTSP    OR MCS TIME                                  
         BE    GOPR20              IF SO, NO METHOD IS NEEDED                   
         CLI   GRPFILT,COMCSTSC    OR MCS T/SHEET COLS                          
         BE    GOPR20              IF SO, NO METHOD IS NEEDED                   
         CLI   GRPFILT,COMCSEXP    OR MCS EXPENSES                              
         BE    GOPR20              IF SO, NO METHOD IS NEEDED                   
         LA    R2,PRRMETHH                                                      
         CLI   5(R2),0             IS A METHOD PROVIDED                         
         BNE   GOPR20                YES, SO CONTINUE                           
         B     ERRMISS               NO, PRINT ERROR MESSAGE                    
         OI    6(R2),X'40'                                                      
GOPR20   BAS   RE,SHOWPROF         SHOW PROFILE AT THIS LEVEL?                  
         BNE   GOPR90              NO, NEXT PROFILE                             
*                                                                               
GOPR25   DS    0H                                                               
         TM    OPTBIT,OPTGRP       ANY GROUP FILTER                             
         BNO   GOPR30                                                           
         CLC   GRPFILT,COFILT      SAME GROUP                                   
         BNE   GOPR90              NO, NEXT PROFILE                             
*                                                                               
GOPR30   CLI   COCNTRY,CTRYANY     ANY COUNTRY FILTER                           
         BE    GOPR40                                                           
         CLI   CTRYCODE,0                                                       
         BE    GOPR40                                                           
         TM    COCNTRY,CTRYNOT     DON'T SHOW IN COUNTRY                        
         BO    GOPR35                                                           
         CLC   COCNTRY,CTRYCODE    ONLY SHOW IN THIS COUNTRY                    
         BNE   GOPR90                                                           
         B     GOPR40                                                           
GOPR35   MVC   BYTE,COCNTRY                                                     
         NI    BYTE,X'FF'-CTRYNOT                                               
         CLC   BYTE,CTRYCODE       DON'T SHOW IN THIS COUNTRY                   
         BE    GOPR90                                                           
*                                                                               
GOPR40   L     R6,APDSLIST                                                      
         ZICM  RF,COSHORT,2        DISP TO SHORT DESC                           
         AR    RF,R6                                                            
         MVC   PSHORT,0(RF)      SHORT DESCRIPTION                              
         MVI   PHYPHEN,C'-'                                                     
         ZICM  RF,CODESCR,2        DISP TO LONG DESC                            
         AR    RF,R6                                                            
         MVC   PLONG,0(RF)       LONG DESCRIPTION                               
*                                                                               
         L     R2,COPDESD          ADDR OF PROFILE VALIDATION ENTRY             
         A     R2,RELO                                                          
         STCM  R2,15,DISVADR       SAVE VALIDATION ENTRY ADDR                   
         USING COPDD,R2                                                         
*                                                                               
         L     R6,APDSLIST                                                      
         ZICM  RF,COPDLIT,2                                                     
         AR    RF,R6                                                            
         MVC   PVALOPT,0(RF)       PROFILE OPTIONS                              
*                                                                               
         LA    R6,COPTIONS         OPTION BLOCK                                 
         USING COPTION,R6                                                       
*                                                                               
         AH    R6,CODISP                                                        
         MVC   LEVEL,COLEVEL     LEVEL TAKEN FROM                               
         LA    RF,FROMTAB                                                       
         USING FROMTABD,RF                                                      
GOPR50   CLC   LEVEL,FLEVEL       FORMAT FROM CODE                              
         BE    GOPR55                                                           
         LA    RF,FLEVLNQ(RF)                                                   
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         B     GOPR50                                                           
*                                                                               
GOPR55   TM    OPTBIT,OPTACT                                                    
         BZ    *+14                                                             
         MVC   PFROM2,FLEVNAME                                                  
         B     *+10                                                             
         MVC   PFROM,FLEVNAME                                                   
         DROP  RF                                                               
                                                                                
         TM    OPTBIT,OPTACT                                                    
         BZ    GOPR60                                                           
         MVC   PWHO,COWHO        WHO CHANGED IT                                 
         MVC   PRDATE,CODAT                                                     
         GOTO1 DATCON,DMCB,(1,PRDATE),(11,PDATE)   DATE LAST CHANGED            
*                                                                               
GOPR60   CLI   SVCODRTN,X'01'        SETTING IN VALIDATION TABLE                
         BNE   GOPR66                                                           
*                                                                               
         ZIC   R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R1,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R1                                                      
GOPR62   CLC   COPDCOB,COSET                                                    
         BE    GOPR64                                                           
         LA    R1,COPDMLEN(R1)     NEXT MINI ENTRY                              
         BCT   R0,GOPR62                                                        
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
GOPR64   L     RF,APUDSLST                                                      
         ZICM  R6,COPDOPT,2        DISP TO SETTING                              
         AR    R6,RF                                                            
         MVC   PSETT,0(R6)     OPTION SETTING                                   
         B     GOPR69                                                           
         DROP  R1                                                               
*                                                                               
         USING COPTION,R6                                                       
GOPR66   DS    0H                                                               
         CLI   SVCODRTN,CODNUMQ      SETTING IN VALIDATION TABLE                
         BNE   GOPR68                                                           
         CURED (P4,COSET),(8,PSETT),0,ALIGN=LEFT                                
         ORG   *-2                                                              
         MVC   CURPCDEC-CURPARMD(L'CURPCDEC,R1),SVCOEXTR  N'DPS                 
         NI    CURPCDEC-CURPARMD(R1),COVNDPS  SWITCH OFF OTHER BITS             
         BASR  RE,RF                                                            
         B     GOPR69                                                           
*                                                                               
GOPR68   ZIC   R1,SVCOBLEN           LENGTH OF SETTING                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSETT(0),COSET                                                   
*                                                                               
GOPR69   TM    OPTBIT,OPTDFLT      DEFAULT OPTION USED?                         
         BZ    GOPR85                                                           
*                                                                               
         DROP  R3                                                               
IO3      USING BLOCKSD,R3                                                       
         L     R3,AIO3             POINT TO BLOCK WITH DEFAULT SETTINGS         
*                                                                               
         LA    R6,IO3.COPTIONS         OPTION BLOCK                             
         USING COPTION,R6                                                       
         AH    R6,SVCODISP                                                      
*                                                                               
GOPR70   CLI   SVCODRTN,X'01'        SETTING IN VALIDATION TABLE                
         BNE   GOPR76                                                           
*                                                                               
         ZIC   R0,COPDNUM          NUMBER OF OPTIONS                            
         LA    R1,COPDENTY         MINI ENTRIES                                 
         USING COPDENTY,R1                                                      
GOPR72   CLC   COPDCOB,COSET                                                    
         BE    GOPR74                                                           
         LA    R1,COPDMLEN(R1)     NEXT MINI ENTRY                              
         BCT   R0,GOPR72                                                        
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
GOPR74   L     RF,APUDSLST                                                      
         ZICM  R6,COPDOPT,2        DISP TO SETTING                              
         AR    R6,RF                                                            
         TM    OPTBIT,OPTACT                                                    
         BO    *+14                                                             
         MVC   PDEF,0(R6)     OPTION SETTING                                    
         B     *+10                                                             
         MVC   PDEF2,0(R6)                                                      
         B     GOPR82                                                           
         DROP  R1,R2                                                            
*                                                                               
         USING COPTION,R6                                                       
GOPR76   DS    0H                                                               
         CLI   SVCODRTN,CODNUMQ      SETTING IN VALIDATION TABLE                
         BNE   GOPR78                                                           
         TM    OPTBIT,OPTACT                                                    
         BO    GOPR77                                                           
         CURED (P4,COSET),(8,PDEF),0,ALIGN=LEFT                                 
         ORG   *-2                                                              
         MVC   CURPCDEC-CURPARMD(L'CURPCDEC,R1),SVCOEXTR  N'DPS                 
         NI    CURPCDEC-CURPARMD(R1),COVNDPS  SWITCH OFF OTHER BITS             
         BASR  RE,RF                                                            
         B     GOPR82                                                           
GOPR77   CURED (P4,COSET),(8,PDEF2),0,ALIGN=LEFT                                
         ORG   *-2                                                              
         MVC   CURPCDEC-CURPARMD(L'CURPCDEC,R1),SVCOEXTR  N'DPS                 
         NI    CURPCDEC-CURPARMD(R1),COVNDPS  SWITCH OFF OTHER BITS             
         BASR  RE,RF                                                            
         B     GOPR82                                                           
*                                                                               
GOPR78   ZIC   R1,SVCOBLEN           LENGTH OF SETTING                          
         BCTR  R1,0                                                             
         TM    OPTBIT,OPTACT                                                    
         BO    GOPR79                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEF(0),COSET                                                    
         B     GOPR82                                                           
GOPR79   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEF2(0),COSET                                                   
*                                                                               
GOPR82   DS    0H                                                               
         DROP  IO3                                                              
         ICM   R3,15,DISCPADR                                                   
         USING COPFTABD,R3                                                      
*                                                                               
GOPR85   TM    OPTBIT,OPTDFLT                                                   
         BZ    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    COFLAG,TWOLINEP     IS THIS A TWO LINE PROFILE?                  
         BZ    GOPR90              NO THAN GO TO NEXT PROFILE                   
         TM    OPTBIT,OPTACT       IS THE ACTIVITY OPTION ON?                   
         BO    GOPR90              YES-DON'T SHOW 2ND LINE                      
         MVC   PSHORT,SPACES                                                    
         MVC   PHYPHEN,SPACES                                                   
         MVC   PLONG,SPACES                                                     
         MVC   PSETT,SPACES                                                     
         ICM   R1,15,DISVADR       ADDR OF PROFILE VALIDATION ENTRY             
         USING COPDD,R1                                                         
*                                                                               
         L     R6,APDSLIST                                                      
         ZICM  RF,COPDLIT2,2                                                    
         AR    RF,R6                                                            
         MVC   PVALOPT,0(RF)       JUST FILL IN 2ND LINE OF SETTINGS            
         TM    OPTBIT,OPTDFLT                                                   
         BZ    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
GOPR90   LA    R3,COTBLEN(R3)      CHECK NEXT PROFILE                           
         CLI   0(R3),X'00'         END OF TABLE                                 
         BNE   GOPR10                                                           
*                                                                               
GOPRX    DS    0H                                                               
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*    CALLS GETCAP FROM KEY IN BIGKEY                                            
*  IF DEFAULT OPTION IS CHOSEN-COBLOCK WILL CONTAIN PROFILES WITH THE           
*  REAL SETTING----AIO3 WILL CONTAIN THE PROFILES WITH THE DEFAULT              
*  SETTING                                                                      
***********************************************************************         
*                                                                               
GOGETCAP NTR1                                                                   
*                                                                               
         LA    R0,COBLOCK          CLEAR COBLOCK                                
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,BIGKEY           FILL IN BLOCK KEY                            
         USING CAPRECD,R6                                                       
*                                                                               
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CAPKCPY                                                   
         MVC   COKMTHD,CAPKMTHD                                                 
         MVC   COKOFC(L'CAPKOFC),CAPKOFC                                        
         MVC   COKDPT(L'CAPKDPT),CAPKDPT                                        
         MVC   COKSDT(L'CAPKSDT),CAPKSDT                                        
         MVC   COKPER(L'CAPKPER),CAPKPER                                        
*                                                                               
*                                                                               
         GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,X'00'      ANY ERRORS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    OPTBIT,OPTSHOW      IF OPTION SHOWALL                            
         BNO   GC40                                                             
         CLC   CAPKMTHD,SPACES     AND METHOD IS SPECIFIED                      
         BE    GC40                                                             
*                                                                               
         L     R0,AIO3             CLEAR AIO3                                   
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
IO3      USING BLOCKSD,R2                                                       
         L     R2,AIO3             USE AIO3                                     
         MVC   IO3.COADM,DATAMGR       PASS A(DATA MANAGER)                     
         MVC   IO3.COBKEY(COBKEYLN),SPACES                                      
         MVC   IO3.COKCPY,CAPKCPY                                               
         MVC   IO3.COKMTHD,SPACES      *** GET NO METHOD PROFILES               
         MVC   IO3.COKOFC(L'CAPKOFC),CAPKOFC                                    
         MVC   IO3.COKDPT(L'CAPKDPT),CAPKDPT                                    
         MVC   IO3.COKSDT(L'CAPKSDT),CAPKSDT                                    
         MVC   IO3.COKPER(L'CAPKPER),CAPKPER                                    
*                                                                               
*                                                                               
GC20     GOTO1 VGETCAP,DMCB,(R2)   USE AIO3                                     
         CLI   IO3.COSTATUS,X'00'      ANY ERRORS                               
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         USING COPFTABD,R6                                                      
         LA    R6,PROFTAB          BUMP THRU PROFILE TABLE                      
         B     GC30                                                             
*                                                                               
GC30NX   LA    R6,COTBLEN(R6)                                                   
GC30     CLI   0(R6),0                                                          
         BE    GC40                                                             
*                                                                               
         TM    COSHOW,COLNOMTH     PROFILE VALID FOR NO METHOD                  
         BNO   GC30NX                                                           
*                                                                               
         ZICM  RF,COBLEN,1         LENGTH OF SETTING                            
         LA    R0,COHEDLN          LENGTH OF HEADER                             
         AR    RF,R0                                                            
         SH    RF,=H'1'                                                         
         BM    GC30NX                                                           
         LA    R1,IO3.COPTIONS                                                  
         AH    R1,CODISP           R1=DISP INTO AIO3 OF PROFILE                 
         LA    R3,COPTIONS                                                      
         AH    R3,CODISP           R3=DISP INTO COBLOCK OF PROFILE              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)     MOVE FROM AIO3 TO COBLOCK                      
         B     GC30NX                                                           
         DROP  R6                                                               
*                                                                               
*                                                                               
GC40     TM    OPTBIT,OPTDFLT      DISP DEFAULT LEVELS?                         
         BNO   GCX                                                              
*                                                                               
***CALL GETCAP A 2ND TIME TO GET DEFAULT PROFILE SETTINGS-USE AIO3              
         L     R0,AIO3             CLEAR AIO3                                   
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,BIGKEY           FILL IN BLOCK KEY                            
         USING CAPRECD,R6                                                       
*                                                                               
IO3      USING BLOCKSD,R2                                                       
         L     R2,AIO3             USE AIO3                                     
         MVC   IO3.COADM,DATAMGR       PASS A(DATA MANAGER)                     
         MVC   IO3.COBKEY(COBKEYLN),SPACES                                      
         MVC   IO3.COKCPY,CAPKCPY                                               
         MVC   IO3.COKMTHD,CAPKMTHD                                             
         MVC   IO3.COKOFC(L'CAPKOFC),CAPKOFC                                    
         MVC   IO3.COKDPT(L'CAPKDPT),CAPKDPT                                    
         MVC   IO3.COKSDT(L'CAPKSDT),CAPKSDT                                    
         MVC   IO3.COKPER(L'CAPKPER),CAPKPER                                    
         MVI   IO3.COSELLEV,COSLVDFT                                            
*                                                                               
         GOTO1 VGETCAP,DMCB,(R2)   USE AIO3                                     
         CLI   IO3.COSTATUS,X'00'      ANY ERRORS                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    OPTBIT,OPTSHOW      IF OPTION SHOWALL                            
         BNO   GCX                                                              
         CLC   CAPKMTHD,SPACES     AND METHOD IS SPECIFIED                      
         BE    GCX                                                              
*                                                                               
         L     R0,AIO2             CLEAR AIO2                                   
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,BIGKEY           FILL IN BLOCK KEY                            
         USING CAPRECD,R6                                                       
*                                                                               
IO2      USING BLOCKSD,R2                                                       
         L     R2,AIO2             USE AIO2                                     
         MVC   IO2.COADM,DATAMGR       PASS A(DATA MANAGER)                     
         MVC   IO2.COBKEY(COBKEYLN),SPACES                                      
         MVC   IO2.COKCPY,CAPKCPY                                               
         MVC   IO2.COKMTHD,SPACES      ****GET NO METHOD PROFILES               
         MVC   IO2.COKOFC(L'CAPKOFC),CAPKOFC                                    
         MVC   IO2.COKDPT(L'CAPKDPT),CAPKDPT                                    
         MVC   IO2.COKSDT(L'CAPKSDT),CAPKSDT                                    
         MVC   IO2.COKPER(L'CAPKPER),CAPKPER                                    
         MVI   IO2.COSELLEV,COSLVDFT                                            
*                                                                               
         GOTO1 VGETCAP,DMCB,(R2)   USE AIO2                                     
         CLI   IO3.COSTATUS,X'00'      ANY ERRORS                               
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         USING COPFTABD,R6                                                      
         LA    R6,PROFTAB          BUMP THRU PROFILE TABLE                      
         B     GC50                                                             
*                                                                               
GC50NX   LA    R6,COTBLEN(R6)                                                   
GC50     CLI   0(R6),0                                                          
         BE    GCX                                                              
*                                                                               
         TM    COSHOW,COLNOMTH     PROFILE VALID FOR NO METHOD                  
         BNO   GC50NX                                                           
*                                                                               
         ZICM  RF,COBLEN,1         LENGTH OF SETTING                            
         LA    R0,COHEDLN          LENGTH OF HEADER                             
         AR    RF,R0                                                            
         SH    RF,=H'1'                                                         
         BM    GC50NX                                                           
         LA    R1,IO2.COPTIONS                                                  
         AH    R1,CODISP           R1=DISP INTO AIO2 OF PROFILE                 
         LA    R3,IO3.COPTIONS                                                  
         AH    R3,CODISP           R3=DISP INTO AIO3 OF PROFILE                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)     MOVE FROM AIO2 TO AIO3                         
         B     GC50NX                                                           
*                                                                               
*                                                                               
GCX      MVC   SAVEKEY2,BIGKEY                                                  
         B     XIT                                                              
         DROP  R6,IO2                                                           
         EJECT                                                                  
***********************************************************************         
*    CHECKS IF PROFILE SHOULD BE SHOWN                                          
*    GETS LEVEL FROM KEY IN BIGKEY                                              
*    R3 SHOULD BE POINTING TO ENTRY IN PROFILE TABLE                            
***********************************************************************         
*                                                                               
SHOWPROF NTR1                                                                   
*                                                                               
         USING COPFTABD,R3                                                      
*                                                                               
         MVC   PROFNUM,CONUM       PROFILE NUMBER                               
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CAPRECD,R6                                                       
*                                                                               
         TM    COFLAG,HIGHLEV      IS THE PROFILE FROM A HIGHER LEVEL           
*                                  THAN METHOD?                                 
         BZ    SP01                                                             
         TM    OPTBIT,OPTGRP       IS A GROUP OPTION ON?                        
         BO    XYES                                                             
SP01     CLC   CAPKMTHD,SPACES                                                  
         BNE   SP05                                                             
         TM    COSHOW,COLNOMTH     NO METHOD                                    
         BNO   XNO                                                              
         CLC   CAPKOFC,SPACES      ANY OFFICE                                   
         BNE   SP10                                                             
         B     XYES                                                             
*                                                                               
SP05     TM    COSHOW,COLNOMTH     THERE IS A METHOD SO                         
         BO    XNO                 SKIP IF FOR NO METHOD                        
         CLC   CAPKOFC,SPACES      ANY OFFICE                                   
         BNE   SP10                                                             
         TM    COSHOW,COLMETH      NO, THEN MUST BE AT METHOD LEVEL             
         BNO   XNO                                                              
         BO    XYES                                                             
*                                                                               
SP10     CLC   CAPKDPT,SPACES      ANY DEPT                                     
         BNE   SP20                                                             
         TM    COSHOW,COLOFFC      NO, THEN OFFICE LEVEL                        
         BNO   XNO                                                              
         BO    XYES                                                             
*                                                                               
SP20     CLC   CAPKSDT,SPACES      ANY SUB DEPT                                 
         BNE   SP30                                                             
         TM    COSHOW,COLDPT       NO, THEN DEPT LEVEL                          
         BNO   XNO                                                              
         BO    XYES                                                             
*                                                                               
SP30     CLC   CAPKPER,SPACES      ANY PERSON                                   
         BNE   SP40                                                             
         TM    COSHOW,COLSDT       NO, THEN SUB DEPT LEVEL                      
         BNO   XNO                                                              
         BO    XYES                                                             
*                                                                               
SP40     TM    COSHOW,COLPER       NO, THEN PERSON LEVEL                        
         BNO   XNO                                                              
         BO    XYES                                                             
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*    CHECKS IF PROFILE IS SET TO SOMETHING (NOT THE DEFAULT)                    
*    PROFNUM HAS PROFILE NUMBER TO BE CHECKED                                   
***********************************************************************         
*                                                                               
PROFSET  NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING OPDELD,R6                                                        
         MVI   ELCODE,OPDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    PS20                                                             
         B     XNO                 NO PROFILE SETTING                           
*                                                                               
PS10     BAS   RE,NEXTEL                                                        
         BNE   XNO                                                              
*                                                                               
PS20     CLC   PROFNUM,OPDNUM                                                   
         BNE   PS10                                                             
         B     XYES                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS - ONLY ONE OPT VALID                       *          
***********************************************************************         
*                                                                               
VALOPTS  NTR1                                                                   
*                                                                               
         MVC   HD8LINE,SPACES                                                   
         LA    R3,OPTSTAB                                                       
         USING VALOPTD,R3                                                       
*                                                                               
VAL10    LA    R2,PRRCATGH         ENTER CATEGORY OPTION FIELD                  
         CLI   5(R2),0             NO OPTION?                                   
         BE    VALX                                                             
*                                                                               
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
VAL20    EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   VOPTINIT(0),8(R2)                                                
         BE    VAL30                                                            
         LA    R3,VOPTLEN(R3)                                                   
         CLI   0(R3),X'00'         EOT                                          
         BNE   VAL20                                                            
         LA    R2,PRRCATGH                                                      
         B     ERRINV                                                           
         OI    6(R2),X'40'                                                      
*                                                                               
VAL30    OC    OPTBIT,VOPTBIT      TURN ON BIT                                  
         TM    VOPTBIT,OPTGRP      IS THERE A GROUP FILTER                      
         BNO   *+10                                                             
         MVC   GRPFILT,VOPTGRP                                                  
*                                                                               
         MVC   HD7TITLE,AC@OPT                                                  
         MVC   HD7FIELD(L'VOPTNAME),VOPTNAME                                    
*                                                                               
VALX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*         VALIDATE SHOWALL, DEFAULT AND ACTIVITY OPTIONS-                       
*         ABLE TO BE COMBINED WITH AN OTHER OPTION                              
***********************************************************************         
         SPACE 1                                                                
VALOPTS2 NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         LA    R2,PRRDEFH          DEFAULT                                      
         CLI   5(R2),0                                                          
         BE    VALS20                                                           
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),AC@YESU                                                  
         BNE   *+12                                                             
         OI    OPTBIT,OPTDFLT                                                   
         B     VALS05                                                           
         CLC   8(0,R2),AC@NOU                                                   
         BE    VALS20                                                           
         B     ERRINV                                                           
         OI    6(R2),X'40'                                                      
*                                                                               
VALS05   MVC   HD8TITLE,AC@OPT                                                  
         OI    HEDLNBIT,PROPTWRD                                                
         LA    R1,WORK                                                          
VALS06   CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     VALS06                                                           
*                                                                               
         MVC   0(L'AC@INCDF,R1),AC@INCDF                                        
*                                                                               
VALS20   LA    R2,PRRALLH          SHOWALL                                      
         CLI   5(R2),0                                                          
         BE    VALS40                                                           
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),AC@YESU                                                  
         BNE   *+12                                                             
         OI    OPTBIT,OPTSHOW                                                   
         B     VALS25                                                           
         CLC   8(0,R2),AC@NOU                                                   
         BE    VALS40                                                           
         B     ERRINV                                                           
         OI    6(R2),X'40'                                                      
*                                                                               
VALS25   TM    HEDLNBIT,PROPTWRD                                                
         BO    *+18                                                             
         MVC   HD8TITLE,AC@OPT                                                  
         OI    HEDLNBIT,PROPTWRD                                                
         B     *+8                                                              
         OI    HEDLNBIT,INSCOMMA                                                
*                                                                               
         LA    R1,WORK             POINT R1 TO RIGHT PLACE IN LINE              
         TM    OPTBIT,OPTDFLT                                                   
         BZ    *+8                                                              
         LA    R1,L'AC@INCDF(R1)                                                
*                                                                               
         TM    HEDLNBIT,INSCOMMA                                                
         BZ    *+12                                                             
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'AC@SALL,R1),AC@SALL                                          
*                                                                               
VALS40   LA    R2,PRRACTH          ACTIVITY                                     
         CLI   5(R2),0                                                          
         BE    VALSX                                                            
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),AC@YESU                                                  
         BNE   *+12                                                             
         OI    OPTBIT,OPTACT                                                    
         B     VALS45                                                           
         CLC   8(0,R2),AC@NOU                                                   
         BE    VALSX                                                            
         B     ERRINV                                                           
         OI    6(R2),X'40'                                                      
*                                                                               
VALS45   TM    HEDLNBIT,PROPTWRD                                                
         BO    *+18                                                             
         MVC   HD8TITLE,AC@OPT                                                  
         OI    HEDLNBIT,PROPTWRD                                                
         B     *+8                                                              
         OI    HEDLNBIT,INSCOMMA                                                
*                                                                               
         LA    R1,WORK             POINT R1 TO RIGHT PLACE IN WORK              
         TM    OPTBIT,OPTDFLT                                                   
         BZ    *+8                                                              
         LA    R1,L'AC@INCDF(R1)                                                
         TM    OPTBIT,OPTSHOW                                                   
         BZ    *+12                                                             
         LA    R1,1(R1)            ADD 1 FOR COMMA                              
         LA    R1,L'AC@SALL(R1)                                                 
*                                                                               
         TM    HEDLNBIT,INSCOMMA                                                
         BZ    *+12                                                             
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'AC@ACTY,R1),AC@ACTY                                          
*                                                                               
VALSX    MVC   HD8FIELD,WORK                                                    
         OC    HD8FIELD,SPACES                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*         VALIDATE OFFICE,DEPT,SUBDPT,PERSON IN 1R                   *          
***********************************************************************         
*                                                                               
VALACCNT NTR1                      VALIDATE LOCATION ENTERED                    
         LA    R6,KEY2             BUILD KEY                                    
         USING ACTRECD,R6                                                       
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,ACCNT                                                    
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY2,AIO2                    
         CLC   KEY2,0(R6)                                                       
         BNE   EINVACC             INVALID ACCOUNT                              
*                                                                               
VAX      DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET METHOD NAME                                                               
* AIO POINTS TO RECORD                                                          
***********************************************************************         
         SPACE 1                                                                
GTMTHNME NTR1                                                                   
*                                                                               
*                                                                               
         MVC   HD1LINE,SPACES                                                   
         LA    R6,BIGKEY                                                        
         USING CAPRECD,R6                                                       
*                                                                               
         CLC   CAPKMTHD,SPACES                                                  
         BE    GTMX                                                             
*                                                                               
         XC    KEY2,KEY2                                                        
         LA    R3,KEY2                                                          
         USING CAHRECD,R3                                                       
*                                                                               
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ                                                 
         MVI   CAHKSUB,CAHKSUBQ                                                 
         MVC   CAHKCPY,CMPY                                                     
         MVC   CAHKMTHD,CAPKMTHD                                                
         XC    CAHKOFC,CAHKOFC                                                  
*                                                                               
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY2,KEY2,0                   
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   EINVMET             INVALID METHOD                               
         MVC   SVDSKADR,CAHKDA                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',SVDSKADR,AIO2,WORK2           
         GOTO1 GETNME,DMCB,AIO,0                                                
*                                                                               
         MVC   HD1LINE,SPACES                                                   
         MVC   HD1DESC,WORK                                                     
         MVC   HD1TITLE,AC@METH                                                 
         DROP  R6                                                               
*                                                                               
         USING METELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   ERRINV                                                           
                                                                                
         MVC   HD1FIELD,METCODE                                                 
         OC    HD1LINE,SPACES                                                   
*                                                                               
GTMX     B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD 1R AT EACH LEVEL TO GET NAME                                            
***********************************************************************         
         SPACE 1                                                                
BLDNAME  NTR1                                                                   
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CAPRECD,R6                                                       
*                                                                               
         MVC   HD2LINE,SPACES                                                   
         MVC   HD3LINE,SPACES                                                   
         MVC   HD4LINE,SPACES                                                   
         MVC   HD5LINE,SPACES                                                   
*                                                                               
BLD10    MVC   ACCNT,SPACES                                                     
         LA    R2,ACCNT            BUILD ACCOUNT IN ACCNT TO READ 1R            
         CLC   CAPKOFC,SPACES                                                   
         BE    BLD20                                                            
                                                                                
         ZIC   RF,LEVELLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CAPKOFC   BUILD ACCOUNT TO READ 1R                       
         BAS   RE,GOGETNME                                                      
         MVC   HD2TITLE,SVLEV1NM                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   HD2FIELD(0),CAPKOFC                                              
         MVC   HD2DESC,WORK                                                     
         AR    R2,RF                                                            
         LA    R2,1(R2)                                                         
*                                                                               
BLD20    CLC   CAPKDPT,SPACES                                                   
         BE    BLD30                                                            
         ZIC   RF,LEVELLN+1                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CAPKDPT    R2 POINTS TO RIGHT PLACE IN ACCNT             
         BAS   RE,GOGETNME                                                      
         MVC   HD3TITLE(L'SVLEV2NM),SVLEV2NM                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   HD3FIELD(0),CAPKDPT                                              
         MVC   HD3DESC,WORK                                                     
         AR    R2,RF                                                            
         LA    R2,1(R2)                                                         
*                                                                               
BLD30    CLC   CAPKSDT,SPACES                                                   
         BE    BLD40                                                            
         ZIC   RF,LEVELLN+2                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CAPKSDT     R2 POINTS TO RIGHT PLACE IN ACCNT            
         BAS   RE,GOGETNME                                                      
         MVC   HD4TITLE,SVLEV3NM                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   HD4FIELD(0),CAPKSDT                                              
         MVC   HD4DESC,WORK                                                     
         AR    R2,RF                                                            
         LA    R2,1(R2)                                                         
*                                                                               
BLD40    CLC   CAPKPER,SPACES                                                   
         BE    BLDX                                                             
         ZIC   RF,LEVELLN+3                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CAPKPER     R2 POINTS TO RIGHT PLACE IN ACCNT            
         BAS   RE,GOGETNME                                                      
         MVC   HD5TITLE,SVLEV4NM                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   HD5FIELD(0),CAPKPER                                              
         MVC   HD5DESC,WORK                                                     
*                                                                               
BLDX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*            CALL GETNME FOR EACH LEVEL OF 1R                                   
***********************************************************************         
         SPACE 1                                                                
GOGETNME NTR1                                                                   
*                                                                               
         XC    KEY2,KEY2                                                        
         LA    R3,KEY2                                                          
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'1'                                                     
         MVI   ACTKLDG,C'R'                                                     
         MVC   ACTKACT,ACCNT                                                    
*                                                                               
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY2,KEY2,0                   
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   EINVACC             INVALID ACCOUNT                              
         MVC   SVDSKADR,ACTKDA                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',SVDSKADR,AIO2,WORK2           
         GOTO1 GETNME,DMCB,AIO,0                                                
*                                                                               
GOX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                        SETUP                                                  
***********************************************************************         
         SPACE 1                                                                
SETUP    NTR1                                                                   
*                                                                               
*                                                                               
         L     R1,=A(PUDCLIST)                                                  
         A     R1,RELO                                                          
         ST    R1,APUDCLST                                                      
         L     R1,=A(PDCLIST)                                                   
         A     R1,RELO                                                          
         ST    R1,APDCLIST                                                      
         L     R1,=A(PUDSLIST)                                                  
         A     R1,RELO                                                          
         ST    R1,APUDSLST                                                      
         L     R1,=A(PDSLIST)                                                   
         A     R1,RELO                                                          
         ST    R1,APDSLIST                                                      
*                                                                               
         L     R2,APDCLIST                                                      
         L     R3,APDSLIST                                                      
         GOTO1 DICTATE,DMCB,C'LL  ',(R2),(R3)                                   
         L     R2,APUDCLST                                                      
         L     R3,APUDSLST                                                      
         GOTO1 DICTATE,DMCB,C'LU  ',(R2),(R3)                                   
*                                                                               
         USING VALOPTD,R3                                                       
         LA    R3,OPTSTAB          TRANSLATE TABLE                              
         CLI   VOPTINIT,X'40'      ALREADY DONE                                 
         BH    S60                                                              
S50      GOTO1 DICTATE,DMCB,C'SU  ',VOPTINIT,0                                  
         GOTO1 DICTATE,DMCB,C'SU  ',VOPTNAME,0                                  
         LA    R3,VOPTLEN(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   S50                                                              
         DROP  R3                                                               
*                                                                               
         USING FROMTABD,R3                                                      
S60      LA    R3,FROMTAB          TRANSLATE TABLE                              
         CLI   FLEVEL,C' '         ALREADY DONE?                                
         BNL   S80                                                              
S70      GOTO1 DICTATE,DMCB,C'SU  ',FLEVNAME,0                                  
         LA    R3,FLEVLNQ(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   S70                                                              
         DROP  R3                                                               
*                                                                               
S80      LA    R2,PROLISTI         FOR THE ENTRIES NOT ALREADY                  
         LA    R3,PROLISTU         TRANSLATED IN THE BASE                       
         GOTO1 DICTATE,DMCB,C'LL  ',(R2),(R3)                                   
*                                                                               
SX       B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HEAD HOOK                                                           *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H1(L'HD1LINE),HD1LINE                                            
         MVC   H2(L'HD2LINE),HD2LINE                                            
         MVC   H3(L'HD3LINE),HD3LINE                                            
         MVC   H4(L'HD4LINE),HD4LINE                                            
         MVC   H5(L'HD5LINE),HD5LINE                                            
         MVC   H6(L'HD6LINE),HD6LINE                                            
         MVC   H7(L'HD7LINE),HD7LINE                                            
         MVC   H8(L'HD8LINE),HD8LINE                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RANDOM STUFF                                                        *         
***********************************************************************         
         SPACE 1                                                                
ENTCHNG  DS    0H                                                               
         MVI   GERROR1,X'04'       REC DISP - ENTER CHANGES                     
         MVI   GMSGTYPE,C'I'       INFO                                         
         B     ERRX                                                             
ERRMISS  DS    0H                                                               
         MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   DS    0H                                                               
         MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
*                                                                               
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
EINVACC  MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)    MISSING HIGHER LEVELS                    
         B     ACCERRX                                                          
EINVMET  MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
ENOMETH  MVC   GERROR,=AL2(ACENOMET)                                            
         B     ACCERRX                                                          
EINVPRO  MVC   GERROR,=AL2(ACEIVPRO)                                            
         B     ACCERRX                                                          
ERRINIT  MVC   GERROR,=AL2(ACEINIRP)                                            
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
ERRONLY1 MVC   GERROR,=AL2(ACEOOENT)                                            
         B     ACCERRX                                                          
ERRENDT  MVC   GERROR,=AL2(ACIEREPD)                                            
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
*                                                                               
ACCERRX  DS    0H                                                               
         MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HEADLINE SPECS                                                      *         
***********************************************************************         
         SPACE 1                                                                
HEDSPEC  DS    0H                                                               
         SPROG 1,THRU,2                                                         
         SSPEC H1,78,RUN                                                        
         SSPEC H2,78,REQUESTOR                                                  
         SSPEC H3,78,REPORT                                                     
         SSPEC H4,78,PAGE                                                       
         SPACE 1                                                                
         SSPEC H14,1,AC#CPDES,19                                                
         SSPEC H14,30,AC#SETTG,7                                                
         SSPEC H14,39,AC#VALOP,13                                               
         SSPEC H14,77,AC#FROM,4                                                 
         SPROG 2                                                                
         SSPEC H14,85,AC#DEF,7                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
*                                                                               
HEDSPEC2 DS    0H                  FOR ACTIVITY OPTION                          
         SPROG 1,THRU,2                                                         
         SSPEC H1,78,RUN                                                        
         SSPEC H2,78,REQUESTOR                                                  
         SSPEC H3,78,REPORT                                                     
         SSPEC H4,78,PAGE                                                       
         SPACE 1                                                                
         SSPEC H14,1,AC#CPDES,19                                                
         SSPEC H14,30,AC#SETTG,7                                                
         SSPEC H14,39,AC#VALOP,13                                               
         SSPEC H14,76,AC#LAST,4                                                 
         SSPEC H14,85,AC#ACTY,8                                                 
         SSPEC H14,95,AC#FROM,4                                                 
         SPROG 2                                                                
         SSPEC H14,106,AC#DEF,7                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE DSECTS                                                                  
***********************************************************************         
*                                                                               
*------- FROM EQUIVALENTS TABLE                                                 
*                                                                               
         DS    0F                                                               
FROMTAB  DC    C'F'                                                             
         DCDD  AC#DEF,10           DEFAULT LEVEL                                
         DC    C'A'                                                             
         DCDD  AC#AGY,10           AGENCY                                       
         DC    C'M'                                                             
         DCDD  AC#METH,10          METHOD                                       
         DC    C'O'                                                             
         DCDD  AC#ANL,10           OFFICE                                       
         DC    C'D'                                                             
         DCDD  AC#DPT,10           DEPARTMENT                                   
         DC    C'S'                                                             
         DCDD  AC#SUBDP,10         SUB-DEPT                                     
         DC    C'P'                                                             
         DCDD  AC#PRSN,10          PERSON                                       
         DC    X'00'                                                            
*                                                                               
OPTSTAB  DS    0C                                                               
         DCDD  AC#OPTTM,3                                                       
         DCDD  AC#TIME,8                                                        
         DC    AL1(OPTGRP),AL1(COTMESHT)                                        
*                                                                               
         DCDD  AC#OPTRN,3                                                       
         DCDD  AC#RUN,8                                                         
         DC    AL1(OPTGRP),AL1(CORUNF)                                          
*                                                                               
         DCDD  AC#OPTDR,3                                                       
         DCDD  AC#DIR,8                                                         
         DC    AL1(OPTGRP),AL1(CODIRECT)                                        
*                                                                               
         DCDD  AC#OPTID,3                                                       
         DCDD  AC#IDR,8                                                         
         DC    AL1(OPTGRP),AL1(COINDIR)                                         
*                                                                               
         DCDD  AC#OPTOV,3                                                       
         DCDD  AC#COVER,8                                                       
         DC    AL1(OPTGRP),AL1(COOVHED)                                         
*                                                                               
         DCDD  AC#OPTSL,3                                                       
         DCDD  AC#SLRY,8                                                        
         DC    AL1(OPTGRP),AL1(COINDTYP)                                        
*                                                                               
         DCDD  AC#OPTNB,3                                                       
         DCDD  AC#NEWBZ,8                                                       
         DC    AL1(OPTGRP),AL1(CONEWBIZ)                                        
*                                                                               
         DCDD  AC#OPTPB,3                                                       
         DCDD  AC#PBONO,8                                                       
         DC    AL1(OPTGRP),AL1(COPROBON)                                        
*                                                                               
         DCDD  AC#OPTHO,3                                                       
         DCDD  AC#HOUSE,8                                                       
         DC    AL1(OPTGRP),AL1(COHOUSEC)                                        
*                                                                               
         DCDD  AC#OPTMT,3                                                       
         DCDD  AC#BRTSH,8                                                       
         DC    AL1(OPTGRP),AL1(COMCSTSP)                                        
*                                                                               
         DCDD  AC#OPTMC,3                                                       
         DCDD  AC#BRTSC,8                                                       
         DC    AL1(OPTGRP),AL1(COMCSTSC)                                        
*                                                                               
         DCDD  AC#OPTXP,3                                                       
         DCDD  AC#BREXP,8                                                       
         DC    AL1(OPTGRP),AL1(COMCSEXP)                                        
*                                                                               
         DC    X'00'                                                            
*                                                                               
***INPUT/OUTPUT FOR DICTATE CALL                                                
PROLISTI DS    0C                  DATA DICTIONARY INPUT                        
         DCDDL AC#DEF,7            DEFAULT                                      
         DCDDL AC#SALL,7           SHOWALL                                      
         DCDDL AC#FROM,4           FROM                                         
         DCDDL AC#SETTG,7          SETTING                                      
         DCDDL AC#CPDES,19         PROFILE DESCRIPTIONS                         
         DCDDL AC#INCDF,15         INCLUDE DEFAULT                              
PROLSTIX DC    X'00'                                                            
*                                                                               
PROLISTU DS    0C                  DATA DICTIONARY OUTPUT                       
AC@DEF   DS    CL7                                                              
AC@SALL  DS    CL7                                                              
AC@FROM  DS    CL4                                                              
AC@SETTG DS    CL7                                                              
AC@CPDES DS    CL19                                                             
AC@INCDF DS    CL15                                                             
PROLSTUX EQU   *                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACCAPROTAB                                                     
         EJECT                                                                  
       ++INCLUDE ACCAPDDICT                                                     
         EJECT                                                                  
*                                                                               
BLOCKSD  DSECT                        BLOCKS PUT IN SYSSPARE                    
       ++INCLUDE ACCAPBLOCK         COBLOCK                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACCAPPROFD        DSECTS FOR ACCAPROTAB                        
*                                                                               
* VALID OPTIONS TABLE DSECT                                                     
*                                                                               
VALOPTD  DSECT                                                                  
VOPTINIT DS    CL3                 INPUT LETTER FOR OPTION                      
VOPTNAME DS    CL8                                                              
VOPTBIT  DS    X                   OPTBIT SETTING                               
VOPTGRP  DS    X                   SETTING FOR GROUP FILTER                     
VOPTLEN  EQU   *-VALOPTD                                                        
*                                                                               
* OPTION TABLE DSECT FOR ACTIVITY, DEFAULT AND SHOWALL                          
*                                                                               
VALOPT2D DSECT                                                                  
VOPTBIT2 DS    X                   OPTBIT SETTING                               
VOPTNME2 DS    CL8                                                              
VOPTQLEN EQU   *-VALOPT2D                                                       
*                                                                               
* FROM LEVEL TABLE DSECT                                                        
*                                                                               
FROMTABD DSECT                                                                  
FLEVEL   DS    CL1                 LEVEL SETTING BIT                            
FLEVNAME DS    CL10                DATA DICT EQUATE                             
FLEVLNQ  EQU   *-FROMTABD                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCUREDITD                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SCREENS                                                             *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAPFFD       BASE SCREEN                                     
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE3D                                                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION AND GLOBAL STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
STARTWRK DS    0A                  IN TWA                                       
RELO     DS    A                                                                
APUDCLST DS    A                                                                
APDCLIST DS    A                                                                
APUDSLST DS    A                                                                
APDSLIST DS    A                                                                
PROFDISP DS    H                   DISP INTO PROFILE TABLE                      
CODISP1  DS    H                   DISP TO ENTRY IN COBLOCK                     
WORK2    DS    CL64                                                             
SVDSKADR DS    XL4                 SAVE DISK ADDRESS                            
METHCODE DS    CL3                 METHOD CODE                                  
METHNUM  DS    CL1                 METHOD NUMBER                                
OFFICE   DS    CL2                 OFFICE CODE                                  
DEPT     DS    CL6                 DEPARTMENT CODE                              
SUBDPT   DS    CL6                 SUB DEPARTMENT CODE                          
PERSON   DS    CL8                 PERSON CODE                                  
PROFILT  DS    CL3                 PROFILE LIST FILTER                          
GRPFILT  DS    XL1                 GROUP FILTER                                 
PROFNUM  DS    XL1                 PROFILE NUMBER                               
LEVEL    DS    CL1                                                              
SHRTNAME DS    CL3                                                              
LONGNAME DS    CL8                                                              
DISVADR  DS    XL4                 A(VALIDATION TABLE ENTRY)                    
DISCPADR DS    XL4                 A(COSTING PROFILE ENTRY)                     
PRDATE   DS    XL3                                                              
SVCODRTN DS    X                   SAVED DISPLAY ROUTINE #                      
SVCOBLEN DS    X                   SAVED MAX LEN OF COBLK FIELD                 
SVCODISP DS    Y                   SAVED DISP INTO COBLOCK                      
SVCOEXTR DS    X                   SAVED N'DECIMAL PLACES FOR EDITTING          
*                                                                               
*                                                                               
SVOPTBIT DS    XL1                 SAVED AT LAST DISPLAY OPTION BIT             
OPTBIT   DS    XL1                 OPTION BIT                                   
OPTACT   EQU   X'80'               ACTIVITY OPTION                              
OPTSHOW  EQU   X'40'               SHOW ALL PROFILES                            
OPTGRP   EQU   X'20'               GROUP FILTER                                 
OPTDFLT  EQU   X'10'               DEFAULT OPTION                               
*                                                                               
HEDLNBIT DS    XL1                 FOR HEALINE PRINTING                         
PROPTWRD EQU   X'80'               ALREADY PRINTED 'OPTION' LITERAL             
INSCOMMA EQU   X'40'               MORE THAN ONE OPT CHOSEN-USE COMMAS          
*                                                                               
LEVBIT   DS    XL1                                                              
LEVDFLT  EQU   X'80'               DEFAULT LEVEL                                
LEVMETH  EQU   X'40'               METHOD LEVEL                                 
LEVGRP   EQU   X'20'               OFFICE GROUP LEVEL                           
LEVOFF   EQU   X'10'               OFFICE LEVEL                                 
LEVDPT   EQU   X'08'               DEPARTMENT LEVEL                             
LEVSDPT  EQU   X'04'               SUB DEPT LEVEL                               
LEVPER   EQU   X'02'               PERSON LEVEL                                 
*                                                                               
METHBIT  DS    XL1                                                              
NOMETH   EQU   X'80'               SHOW PROFS WITH NO METHOD                    
ALLMETH  EQU   X'40'               SHOW PROFS FOR ALL METHODS                   
SPECMETH EQU   X'20'               FOR A SPECIFIC METHOD ONLY                   
*                                                                               
LEVELLN  DS    CL4                 LENGTHS OF ALL LEVELS                        
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
SVLEV1NM DS    XL(L'ACLVDESC)      LEVEL 1 DESCRIPTION                          
SVLEV2NM DS    XL(L'ACLVDESC)      LEVEL 2 DESCRIPTION                          
SVLEV3NM DS    XL(L'ACLVDESC)      LEVEL 3 DESCRIPTION                          
SVLEV4NM DS    XL(L'ACLVDESC)      LEVEL 4 DESCRIPTION                          
*                                                                               
SAVEKEY  DS    XL42                ACCFILE KEY                                  
SAVEKEY2 DS    XL42                                                             
KEY2     DS    XL42                ACCFILE KEY                                  
         EJECT                                                                  
*                                                                               
HD1LINE  DS    0CL60               METHOD                                       
HD1TITLE DS    CL6                                                              
         DS    CL2                                                              
HD1FIELD DS    CL3                                                              
         DS    CL7                                                              
HD1DESC  DS    CL42                                                             
*                                                                               
HD2LINE  DS    0CL60               OFFICE                                       
HD2TITLE DS    CL6                                                              
         DS    CL2                                                              
HD2FIELD DS    CL8                                                              
         DS    CL2                                                              
HD2DESC  DS    CL42                                                             
*                                                                               
HD3LINE  DS    0CL60               DEPT                                         
HD3TITLE DS    CL6                                                              
         DS    CL2                                                              
HD3FIELD DS    CL8                                                              
         DS    CL2                                                              
HD3DESC  DS    CL42                                                             
*                                                                               
HD4LINE  DS    0CL60               SUBDEPT                                      
HD4TITLE DS    CL6                                                              
         DS    CL2                                                              
HD4FIELD DS    CL8                                                              
         DS    CL2                                                              
HD4DESC  DS    CL42                                                             
*                                                                               
HD5LINE  DS    0CL60               PERSON                                       
HD5TITLE DS    CL6                                                              
         DS    CL2                                                              
HD5FIELD DS    CL8                                                              
         DS    CL2                                                              
HD5DESC  DS    CL42                                                             
*                                                                               
HD6LINE  DS    0CL60               PROFILE                                      
HD6TITLE DS    CL7                                                              
         DS    CL1                                                              
HD6FIELD DS    CL52                                                             
*                                                                               
HD7LINE  DS    0CL60               OPTION (PROBONO,HOUSE,TIME, ETC.)            
HD7TITLE DS    CL6                                                              
         DS    CL2                                                              
HD7FIELD DS    CL52                                                             
*                                                                               
HD8LINE  DS    0CL60               OPTION (DEFAULT,SHOWALL,ACTIVITY)            
HD8TITLE DS    CL6                                                              
         DS    CL2                                                              
HD8FIELD DS    CL52                                                             
         EJECT                                                                  
***********************************************************************         
* APPLICATION AND GLOBAL STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
       EJECT                                                                    
       ++INCLUDE DDSPOOLD                                                       
       EJECT                                                                    
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PSHORT   DS    CL3                                                              
PHYPHEN  DS    CL1                                                              
PLONG    DS    CL24                                                             
         DS    CL1                                                              
PSETT    DS    CL8                                                              
         DS    CL1                                                              
PVALOPT  DS    CL37                                                             
         DS    CL1                                                              
PFROM    DS    CL10                                                             
         DS    CL1                                                              
PDEF     DS    CL8                                                              
         ORG   PFROM                                                            
PWHO     DS    CL8                 FOR ACTIVITY OPTION PRINTING                 
         DS    CL1                                                              
PDATE    DS    CL8                                                              
         DS    CL2                                                              
PFROM2   DS    CL10                                                             
         DS    CL1                                                              
PDEF2    DS    CL8                                                              
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACCAP15   07/07/20'                                      
         END                                                                    
