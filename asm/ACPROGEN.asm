*          DATA SET ACPROGEN   AT LEVEL 079 AS OF 07/10/20                      
*PHASE T00A83A                                                                  
*INCLUDE ACDISOPT                                                               
*INCLUDE ACVALOPT                                                               
*INCLUDE ACPROCPT                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE ACJOBMNT                                                               
*INCLUDE PRORATA                                                                
*INCLUDE ACRAPPER                                                               
         TITLE 'T00A83 - PRODUCTION GENERAL ROUTINES'                           
*GHOA 14FEB18 077 DSRD-18081 NEW OPT/MAIN 'EMAIL IF PERSN ASSIGN 2 JOB'         
*VGUP 07AUG19 078 DSRD-22480 RELINK FOR ACJOBMNT                                
*ASAX 26MAY20 079 DSRD-25987 NEW OPT/MAIN 'DATE EST IN LAST FISCAL YR'          
         PRINT NOGEN                                                            
T00A83   CSECT                                                                  
         REQUS                                                                  
         USING *,RF                                                             
GEN      NTR1                                                                   
         DROP  RF                                                               
         LR    RB,RF                                                            
         USING T00A83,RB,R7,R8                                                  
         B     *+12                                                             
         DC    CL8'*PROGEN*'                                                    
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING T60BFFD,RA                                                       
         SPACE 1                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VSETHEIR                                                         
         B     VSETCOMP                                                         
         B     VSETADDS                                                         
         B     VSETOG                                                           
         B     VVALOG                                                           
         B     VVALOFF                                                          
         B     VSETOFF                                                          
         B     VVALCLI                                                          
         B     VSETCLI                                                          
         B     VVALPROD                                                         
         B     VSETPROD                                                         
         B     VVALMG                                                           
         B     VVALMED                                                          
         B     VSETMED                                                          
         B     VVALJOB                                                          
         B     VSETJOB                                                          
         B     VVALWG                                                           
         B     VVALWORK                                                         
         B     VSETWORK                                                         
         B     VANYNAME                                                         
         B     VVALAC12                                                         
         B     VVALAC14                                                         
         B     VCANWEDL                                                         
         B     VSETNAME                                                         
         B     VVACTIVE                                                         
         B     VVALLOC                                                          
         B     VPERSIN                                                          
         B     VPERSOUT                                                         
         B     VNAMEIN                                                          
         B     VNAMEOUT                                                         
         B     VADDRIN                                                          
         B     VADDROUT                                                         
         B     CALL                                                             
         B     RETURN                                                           
         B     CLEARF                                                           
         B     CALL0                                                            
         B     DISOPT                                                           
         B     VALOPT                                                           
         B     ERRCUR                                                           
         B     VVALSCH                                                          
         B     VVALCAT                                                          
         B     VVALPAN                                                          
         B     BLDMED                                                           
         B     BLDWC                                                            
         B     BLDOFF                                                           
         B     CHKACC                                                           
         B     VVALPROG                                                         
         B     SAVPTRS                                                          
         B     CHGPTRS                                                          
         B     SETNEW                                                           
         B     SETEMU                                                           
         B     MODPTRS                                                          
         B     VJOBDEL                                                          
         B     VJOBRES                                                          
         B     VJOBOPN                                                          
         B     VJOBCLS                                                          
         DS    4X'00'              RESERVED FOR JOBLOK                          
         B     VNUMERIC                                                         
         B     VSETFLTS                                                         
         B     VVALFLTS                                                         
         B     VINFOXIT                                                         
         B     VXTRAXIT                                                         
         B     VPERSIN2                                                         
         B     VPERSOU2                                                         
         B     VCKCARUL                                                         
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              COMPANY RECORD AND VALUES                                        
         SPACE 3                                                                
VSETCOMP LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,BLANKS                                                       
         MVC   ACKEYACC(1),CUL                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         SPACE 1                                                                
         MVI   ELCODE,CPYELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CPYELD,R6                                                        
         MVC   COMPSTA1,CPYSTAT1   SAVE STATUS BYTES                            
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN1Q                                                    
         BL    VSETC2                                                           
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN2Q                                                    
         BL    VSETC2                                                           
         MVC   COMPSTA9,CPYSTAT9                                                
         MVC   COMPSTAA,CPYSTATA                                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    VSETC2                                                           
         MVC   COMPSTAB,CPYSTATB                                                
         MVC   COMPSTAC,CPYSTATC                                                
*                                                                               
VSETC2   MVC   CUL+1(2),CPYPROD    PRODUCTION UNIT/LEDGER                       
         MVI   OFCLNGTH,1                                                       
         TM    COMPSTA4,X'01'      NEW OFFICES?                                 
         BNO   *+8                                                              
         MVI   OFCLNGTH,2                                                       
*                                                                               
         MVI   DPTLNGTH,2          DEPARTMENT LENGTH DEFAULT IS 2               
         CLI   CPYDEPTL,0                                                       
         BE    *+10                                                             
         MVC   DPTLNGTH,CPYDEPTL   LENGTH OF DEPT. CODES                        
*                                                                               
         MVC   COMPNAME,BLANKS     GET COMPANY NAME                             
         MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=Y(ACNMNAME-ACNAMED+1)                                        
         BM    XIT                                                              
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   COMPNAME(0),ACNMNAME                                             
         DROP  R6                                                               
         SPACE 2                                                                
VSETADDS LH    R1,=Y(RECACTS-T00A83)                                            
         LA    R1,T00A83(R1)                                                    
         ST    R1,ARECACT                                                       
         MVI   LRECACT,L'RECACTS   OVERRIDE L'RECORD/ACTION ENTRIES             
         LH    R1,=Y(OPTTAB-T00A83)                                             
         LA    R1,T00A83(R1)                                                    
         ST    R1,AOPTTAB          SET OPTION TABLE ADDRESS                     
         LH    R1,=Y(RECSEC-T00A83)                                             
         LA    R1,T00A83(R1)                                                    
         ST    R1,ARECSEC          SET SECURITY TABLE ADDRESS                   
         B     XIT                                                              
         EJECT                                                                  
*              LEDGER HEIRARCHY                                                 
         SPACE 3                                                                
VSETHEIR LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY(42),BLANKS                                                   
         MVC   KEY(3),CUL                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,INVLDGR       NO PRODUCTION LEDGER                         
         B     VEXIT                                                            
*                                                                               
         CLI   AUTHOR,0            ANY NEW PROD SECURITY ?                      
         BE    SETHIER             NO, ALWAYS DO SECCHECK THEN                  
         CLI   OPTION2,C'N'        YES, SKIP THE SECURITY CHECK ?               
         BE    *+8                 YES                                          
*                                                                               
SETHIER  BAS   RE,SECCHECK         (SECURITY CHECK)                             
         SPACE 1                                                                
         MVI   ELCODE,ACLTELQ      LEDGER ELEMENT                               
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R6                                                       
         MVC   LEDGTOFF,ACLTOFF                                                 
         DROP  R6                                                               
         SPACE 1                                                                
         MVI   ELCODE,ACHRELQ      GET HEIRARCHY ELEMENT                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R6                                                       
         MVC   LLEVA,ACHRLEVA       AND SET LENGTHS                             
         MVC   LLEVAB,ACHRLEVB                                                  
         MVC   LLEVABC,ACHRLEVC                                                 
         MVC   LLEVABCD,ACHRLEVD                                                
         ZIC   R1,LCLIPRO                                                       
         ZIC   R0,LCLI                                                          
         SR    R1,R0                                                            
         STC   R1,LPRO                                                          
         IC    R1,LCLIJOB                                                       
         IC    R0,LCLIPRO                                                       
         SR    R1,R0                                                            
         STC   R1,LJOB                                                          
         IC    R1,LCLIJOB                                                       
         IC    R0,LCLI                                                          
         SR    R1,R0                                                            
         STC   R1,LPROJOB                                                       
         B     XIT                                                              
         EJECT                                                                  
*              OFFICE ROUTINES                                                  
         SPACE 3                                                                
VSETOG   MVI   EFFOFG,0            SET EFFOFG FROM EFFOFFC                      
         CLI   EFFOFFC,X'41'                                                    
         BL    XIT                                                              
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGOFC,EFFOFFC                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACOGKEY,KEYSAVE     WAS THERE AN OFFICE RECORD?                  
         BNE   XIT                                                              
         GOTO1 SETOFF              THEN LOOK UP THE OFFICE GROUP                
         B     XIT                                                              
         SPACE 1                                                                
VVALOG   GOTO1 ANY                 VALIDATE OFFICE GROUP                        
         MVC   EFFOFG,WORK                                                      
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOG                                                  
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGCODE,EFFOFG                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         SPACE 1                                                                
VVALOFF  GOTO1 ANY                 VALIDATE OFFICE                              
         TM    COMPSTA4,X'01'      TEST ON NEW OFFICES                          
         BO    *+12                                                             
         CLI   5(R2),1             TEST INPUT LENGTH                            
         BH    INVEND                                                           
         MVC   EFFOFF,WORK                                                      
         MVC   EFFOFFC,WORK                                                     
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGOFC,EFFOFFC                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETOFF                                                           
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         SPACE 1                                                                
VSETOFF  MVI   EFFOFG,0            PRESET OFFICE GROUP TO ZERO                  
         MVI   ELCODE,ACGPELQ      GET GROUP FROM GROUP ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING ACGPD,R6                                                         
         MVC   EFFOFG,ACGPCODE                                                  
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
*              CLIENT ROUTINES                                                  
         SPACE 3                                                                
VVALCLI  GOTO1 SETHEIR                                                          
         GOTO1 ANY                                                              
         MVC   CLICODE,WORK                                                     
         MVI   ERROR,TOOLNG                                                     
         CLC   5(1,R2),LCLI                                                     
         BH    VEXIT                                                            
         MVC   KEY(42),BLANKS                                                   
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 SETCLI                                                           
         CLI   RECNUM,RECNCLI      IF WE ARE AT A CLIENT RECORD                 
         BE    VVALCLI1            MUST DO SECURITY NOW                         
         CLI   TWAACCS,C'*'        TEST FOR OFFICE SECURITY                     
         BE    VVALCLI2            YES-DO NOT CHECK AT CLIENT LEVEL             
         CLI   TWAACCS,C'$'                                                     
         BE    VVALCLI2                                                         
         SPACE 1                                                                
VVALCLI1 BAS   RE,ACCCHECK         CHECK ANYTHING ELSE                          
         SPACE 1                                                                
VVALCLI2 GOTO1 ANYNAME                                                          
         GOTO1 SETOG               GET OFFICE GROUP IF AVAILABLE                
         B     XIT                                                              
         SPACE 1                                                                
VSETCLI  L     R1,AIO                                                           
         MVC   CLICODE,3(R1)                                                    
         MVI   ELCODE,RSTELQ       SET DATA FROM STATUS ELEMENT                 
         BAS   RE,GETELIO                                                       
         USING RSTELD,R6                                                        
         MVC   CLIF1,RSTFILT1                                                   
         MVC   CLIF2,RSTFILT2                                                   
         MVC   CLIF4,RSTFILT4                                                   
*                                                                               
         GOTO1 GETFILTS,CLFLTS     LOAD CLFLTS FROM STATUS ELEMENT              
         BAS   RE,CLREFFS          CLEAR EFFECTIVE FILTER VALUES                
         GOTO1 SETEFFS,CLFLTS      SET EFLTS FROM CLIENT FILTERS                
*                                                                               
         MVI   CLIOFF,0                                                         
         XC    CLIOFFC,CLIOFFC                                                  
         MVI   ELCODE,ACPRELQ      SET OFFICE FROM PROFILE ELEMENT              
         BAS   RE,GETELIO                                                       
         BNE   VSETCLI2            (IF AROUND)                                  
         USING ACPROFD,R6                                                       
         MVC   CLIOFF,ACPROFFC                                                  
         MVC   CLIOFFC,ACPROFFC                                                 
         SPACE 1                                                                
VSETCLI2 MVC   EFFOFF,CLIOFF                                                    
         MVC   EFFOFFC,CLIOFFC                                                  
         MVC   EFFF1,CLIF1                                                      
         MVC   EFFF2,CLIF2                                                      
         MVC   EFFF4,CLIF4                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCT ROUTINES                                                 
         SPACE 3                                                                
VVALPROD GOTO1 ANY                                                              
         MVC   PRODCODE,WORK                                                    
         MVI   ERROR,TOOLNG                                                     
         CLC   5(1,R2),LPRO                                                     
         BH    VEXIT                                                            
         MVC   KEY(42),BLANKS                                                   
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 SETPROD                                                          
         BAS   RE,ACCCHECK         (ACCESS CHECK)                               
         GOTO1 ANYNAME                                                          
         GOTO1 SETOG               GET OFFICE GROUP IF AVAILABLE                
         B     XIT                                                              
         SPACE 1                                                                
VSETPROD ZIC   R1,LCLI                                                          
         A     R1,AIO                                                           
         LA    R1,3(R1)                                                         
         MVC   PRODCODE,0(R1)                                                   
         MVI   ELCODE,RSTELQ       SET DATA FROM STATUS ELEMENT                 
         BAS   RE,GETELIO                                                       
*                                                                               
         USING RSTELD,R6                                                        
         MVC   PRODF1,RSTFILT1                                                  
         MVC   PRODF2,RSTFILT2                                                  
         MVC   PRODF4,RSTFILT4                                                  
         GOTO1 GETFILTS,PRFLTS                                                  
         BAS   RE,CLREFFS          CLEAR EFFECTIVE FILTER VALUES                
         GOTO1 SETEFFS,CLFLTS      SET EFLTS FROM CLIENT FILTERS                
         GOTO1 SETEFFS,PRFLTS      SET EFLTS FROM PRODUCT FILTERS               
*                                                                               
         MVI   PRODOFF,0                                                        
         XC    PRODOFFC,PRODOFFC                                                
         MVI   ELCODE,ACPRELQ      SET OFFICE FROM PROFILE ELEMENT              
         BAS   RE,GETELIO                                                       
         BNE   VSETPRO2            (IF AROUND)                                  
         USING ACPROFD,R6                                                       
         MVC   PRODOFF,ACPROFFC                                                 
         MVC   PRODOFFC,ACPROFFC                                                
*                                                                               
VSETPRO2 MVC   EFFOFF,CLIOFF       RE-ESTABLISH EFFECTIVE FROM CLI              
         MVC   EFFOFFC,CLIOFFC                                                  
         MVC   EFFF1,CLIF1                                                      
         MVC   EFFF2,CLIF2                                                      
         MVC   EFFF4,CLIF4                                                      
         CLI   PRODOFF,X'41'       AND PRODUCT STATUS                           
         BL    *+10                                                             
         MVC   EFFOFF,PRODOFF                                                   
         CLI   PRODOFFC,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFOFFC,PRODOFFC                                                 
         CLI   PRODF1,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF1,PRODF1                                                     
         CLI   PRODF2,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF2,PRODF2                                                     
         CLI   PRODF4,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF4,PRODF4                                                     
         B     XIT                                                              
         EJECT                                                                  
*              MEDIA ROUTINES                                                   
         SPACE 3                                                                
VVALMG   GOTO1 ANY                 VALIDATE MEDIA GROUP                         
         MVC   MGROUP,WORK                                                      
         LA    R4,KEY                                                           
         USING ACMGKEY,R4                                                       
         XC    ACMGKEY,ACMGKEY                                                  
         MVI   ACMGRTYP,ACMGEQU                                                 
         MVI   ACMGSREC,ACMGSEQU                                                
         MVC   ACMGCUL,CUL                                                      
         MVC   ACMGCODE,MGROUP                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         SPACE 1                                                                
VVALMED  GOTO1 ANY                                                              
         MVC   MEDIA,WORK                                                       
         MVC   KEY,BLANKS                                                       
         MVI   KEY,X'09'           TRY AND READ THE MEDIA RECORD                
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(1),MEDIA                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   ERROR,BADMEDIA                                                   
         CLC   KEY(3),KEYSAVE                                                   
         BNE   VEXIT                                                            
         GOTO1 SETMED                                                           
         B     XIT                                                              
         SPACE 1                                                                
VSETMED  MVI   ELCODE,ACMDELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R6                                                      
         MVC   MEDNAME,ACMDDESC                                                 
         MVC   MGROUP,ACMDGRP                                                   
         B     XIT                                                              
         EJECT                                                                  
*              JOB ROUTINES                                                     
         SPACE 3                                                                
VVALJOB  GOTO1 VALMED                                                           
         GOTO1 ANY                                                              
         MVC   JOBNUM,WORK                                                      
         MVI   ERROR,TOOLNG                                                     
         CLC   5(1,R2),LJOB                                                     
         BH    VEXIT                                                            
         MVC   KEY(42),BLANKS                                                   
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),JOBNUM                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 SETJOB                                                           
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         SPACE 1                                                                
VSETJOB  ZIC   R1,LCLIPRO                                                       
         A     R1,AIO                                                           
         LA    R1,3(R1)                                                         
         MVC   JOBNUM,0(R1)                                                     
         MVI   ELCODE,RSTELQ       SET DATA FROM STATUS ELEMENT                 
         BAS   RE,GETELIO                                                       
*                                                                               
         USING RSTELD,R6                                                        
         MVC   JOBF1,RSTFILT1                                                   
         MVC   JOBF2,RSTFILT2                                                   
         MVC   JOBF4,RSTFILT4                                                   
         MVC   JOBSTAT,RSTSTAT     EXTRACT STATUS BYTE                          
         GOTO1 GETFILTS,JBFLTS                                                  
         BAS   RE,CLREFFS          CLEAR EFFECTIVE FILTER VALUES                
         GOTO1 SETEFFS,CLFLTS      SET EFLTS FROM CLIENT FILTERS                
         GOTO1 SETEFFS,PRFLTS      SET EFLTS FROM PRODUCT FILTERS               
         GOTO1 SETEFFS,JBFLTS      SET EFLTS FROM JOB FILTERS                   
*                                  RE-ESTABLISH EFFECTIVE FROM CLI              
         MVC   EFFF1,CLIF1                                                      
         MVC   EFFF2,CLIF2                                                      
         MVC   EFFF4,CLIF4                                                      
         CLI   PRODF1,X'41'        AND FROM PRODUCT                             
         BL    *+10                                                             
         MVC   EFFF1,PRODF1                                                     
         CLI   PRODF2,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF2,PRODF2                                                     
         CLI   PRODF4,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF4,PRODF4                                                     
         CLI   JOBF1,X'41'         AND FROM JOB                                 
         BL    *+10                                                             
         MVC   EFFF1,JOBF1                                                      
         CLI   JOBF2,X'41'                                                      
         BL    *+10                                                             
         MVC   EFFF2,JOBF2                                                      
         CLI   JOBF4,X'41'                                                      
         BL    *+10                                                             
         MVC   EFFF4,JOBF4                                                      
*                                                                               
         MVI   ELCODE,ACJBELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACJOBD,R6                                                        
         MVC   JOBCLOSE,ACJBCLOS                                                
         MVC   JOBJSTAT,ACJBSTAT   EXTRACT JOB ELEMENT STATUS BYTE              
         MVC   JOBJSTA2,ACJBSTA2                                                
         B     XIT                                                              
         EJECT                                                                  
*              SECURITY AND ACCESS CHECKING                                     
         SPACE 3                                                                
SECCHECK NTR1                      CHECK SECURITY LEVEL AGAINST TWA             
         CLI   TWAAUTH+1,0                                                      
         BE    XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSTELD,R6                                                        
         CLC   RSTSECY+1(1),TWAAUTH+1                                           
         BNH   XIT                                                              
         MVI   ERROR,SECLKOUT      RECORD HAS HIGHER SECURITY THAN TWA          
         B     VEXIT                                                            
         SPACE 1                                                                
ACCCHECK NTR1                      CHECK FOR LIMIT ACCESS                       
         CLC   TWAACCS,BLANKS      TEST FOR ANY LIMIT ACCESS                    
         BNH   XIT                 NO                                           
         BAS   RE,CHKSEC                                                        
         BE    XIT                 BYPASS SECURITY CHECKING                     
*&&DO                                                                           
         CLC   TWAACCS(2),BLANKS   TEST FOR ANY OLD ACCESS                      
         BNH   OFFCHECK                                                         
         CLI   TWAACCS,C'*'        TEST FOR SINGLE OFFICE CONTROL               
         BE    OFFCHECK                                                         
         CLI   TWAACCS,C'$'        TEST FOR LIST CONTROL                        
         BE    OFFCHECK                                                         
         CLC   TWAACCS(2),CLICODE  APPLY CLIENT CODE SECURITY                   
         BE    XIT                                                              
         B     NOACCEX                                                          
*&&                                                                             
         SPACE 1                                                                
OFFCHECK L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,EFFOFFC                                                 
         MVC   OFFAOPOS,LEDGTOFF   LEDGER OFFICE POSITION                       
         MVC   OFFAREC,AIO                                                      
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    XIT                 SECURITY IS OK                               
         SPACE 1                                                                
NOACCEX  MVI   ERROR,NOACCESS                                                   
         B     VEXIT                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************                     
* CHKSEC - CHECK IF LIMITED ACCESS/OFFCICE SECURITY       *                     
*          IS REQUIRED                                    *                     
* ON EXIT,  CC=EQ DO NOT TEST, CC=NEQ DO SECURITY TEST    *                     
***********************************************************                     
         SPACE 1                                                                
CHKSEC   L     R4,ARECSEC          R4=A(RECORD/ACTION TABLE)                    
         LA    R3,L'RECSEC         R3=L'RECORD/ACTION TABLE ENTRY               
*                                                                               
CHKSEC2  CLI   0(R4),X'FF'         TEST FOR EOT                                 
         BE    CHKSECN                                                          
         CLC   0(1,R4),RECNUM      FIND CORRECT RECORD/ACTION                   
         BNE   CHKSEC4             NO                                           
         CLC   1(1,R4),ACTNUM      MATCH ON RECORD/ACTION                       
         BE    CHKSECY                                                          
*                                                                               
CHKSEC4  LA    R4,0(R3,R4)         NEXT TABLE ENTRY                             
         B     CHKSEC2                                                          
*                                                                               
CHKSECY  CR    RB,RB               SET CC=EQ FOR 'YES'                          
         BR    RE                                                               
*                                                                               
CHKSECN  LTR   RB,RB               SET CC=NEQ FOR 'NO'                          
         BR    RE                                                               
         EJECT                                                                  
*              WORK CODE ROUTINES                                               
         SPACE 3                                                                
VVALWG   GOTO1 ANY                 VALIDATE WORK GROUP                          
         MVC   WGROUP,WORK                                                      
         LA    R4,KEY                                                           
         USING ACWGKEY,R4                                                       
         XC    ACWGKEY,ACWGKEY                                                  
         MVI   ACWGRTYP,ACWGEQU                                                 
         MVI   ACWGSREC,ACWGSEQU                                                
         MVC   ACWGCUL,CUL                                                      
         MVC   ACWGCODE,WGROUP                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         B     XIT                                                              
         SPACE 1                                                                
VVALWORK GOTO1 ANY                                                              
         MVC   WORKCODE,WORK                                                    
         MVC   KEY,BLANKS                                                       
         MVI   KEY,X'0A'           TRY AND READ THE WORK RECORD                 
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),WORKCODE                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   ERROR,BADWORK                                                    
         CLC   KEY(6),KEYSAVE                                                   
         BNE   VEXIT                                                            
         GOTO1 SETWORK                                                          
         B     XIT                                                              
         SPACE 1                                                                
VSETWORK MVI   ELCODE,ACANELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACANALD,R6                                                       
         MVC   WORKNAME,ACANDESC                                                
         MVC   WGROUP,ACANGRUP                                                  
         B     XIT                                                              
         EJECT                                                                  
* SCHEME/CATEGORY ROUTINES                                                      
*                                                                               
VVALSCH  GOTO1 ANY                                                              
         MVC   SCHEME,WORK                                                      
         LA    R4,KEY                                                           
         USING ACSHKEY,R4                                                       
         XC    ACSHKEY,ACSHKEY                                                  
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,SCHEME     SCHEME CODE                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACSHKEY,KEYSAVE     TEST SCHEME FOUND                            
         BE    VVALSCH2            YES                                          
         MVI   ERROR,BADSCH                                                     
         B     VEXIT                                                            
         SPACE 1                                                                
VVALSCH2 MVI   ELCODE,ACSDELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSDD,R6                                                         
         MVC   SCHMNAME,ACSDNAME                                                
         B     XIT                                                              
         SPACE 2                                                                
VVALCAT  GOTO1 ANY                                                              
         MVC   CATEGORY,WORK                                                    
         LA    R4,KEY                                                           
         USING ACCTKEY,R4                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,SCHEME      SCHEME CODE                                  
         MVC   ACCTCODE,CATEGORY   CATEGORY CODE                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACCTKEY,KEYSAVE                                                  
         BE    VVALCAT2                                                         
         MVI   ERROR,BADCAT                                                     
         B     VEXIT                                                            
         SPACE 1                                                                
VVALCAT2 MVI   ELCODE,ACCDELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACCDD,R6                                                         
         MVC   CATGNAME,ACCDNAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE PANEL                                                   
*                                                                               
VVALPAN  GOTO1 ANY                                                              
         MVC   PANEL,WORK                                                       
         CLC   PANEL,DEFPANEL                                                   
         BE    VVALPANX                                                         
         LA    R4,KEY                                                           
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,PANEL                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACPNKEY,KEYSAVE     TEST IF PANEL FOUND                          
         BE    VVALPAN2            YES                                          
         MVI   ERROR,BADPAN                                                     
         B     VEXIT                                                            
*                                                                               
VVALPAN2 MVI   ELCODE,ACPHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPHD,R6                                                         
         MVC   PANLNAME,ACPHNAME                                                
*                                                                               
VVALPANX B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                  VALIDATE TAX LOCALITY CODE                   
VVALLOC  GOTO1 ANY                                                              
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   LOCCODE,WORK        SAVE CODE TO PASS BACK                       
         MVC   ACUTKEY,BLANKS                                                   
         MVI   ACUTTYPE,ACUTEQU    TRY AND READ THE LOCALITY RECORD             
         MVI   ACUTSREC,ACUTSEQU                                                
         MVC   ACUTCMP,CUL                                                      
         MVC   ACUTLOC,LOCCODE                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   ERROR,INVALID                                                    
         CLC   KEY(11),KEYSAVE                                                  
         BNE   VEXIT                                                            
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO VALIDATE AN ESTIMATE REPORT PROGRAM CODE         *             
* ON ENTRY, P1=A(FIELD HEADER), P2=A(IO AREA), P3=A(OUTPUT AREA)  *             
* ON EXIT, CC=EQ AND ERROR=0 IF OK, CC=NEQ AND ERROR NEQ 0 IF BAD *             
*          CODE                                                   *             
*******************************************************************             
         SPACE 1                                                                
VVALPROG L     R2,0(R1)            R2=A(FIELD HEADER)                           
         LM    R5,R6,4(R1)         R5=A(IO AREA), R6=A(OUTPUT)                  
         MVI   ERROR,0                                                          
         GOTO1 ANY                                                              
         MVC   0(L'CT01NAME,R6),WORK  SET NAME IN OUTPUT                        
         CLI   5(R2),L'CT01NAME    TEST IF CODE IS TOO LONG                     
         BNH   *+12                                                             
         MVI   ERROR,INP2LONG                                                   
         B     VVALPRGX                                                         
*                                                                               
         L     R3,AIO              SAVE IO AREA POINTER                         
         LA    R4,KEY                                                           
         USING CT01RECD,R4                                                      
         XC    CT01KEY,CT01KEY                                                  
         MVI   CT01TYPE,CT01TYPQ                                                
         MVC   CT01AGID,AGENCY                                                  
         MVI   CT01SYS,6           SYSTEM=ACC                                   
         MVI   CT01PRG,X'0B'       PROGRAM=PROD                                 
         MVI   CT01PHAS,X'39'      PHASE=ESTIMATE EDIT                          
         MVC   CT01NAME,WORK                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'CTFILE'                                            
         ST    R5,AIO                                                           
         GOTO1 HIGH                                                             
         CLC   CT01KEY,KEYSAVE                                                  
         BE    *+8                                                              
         MVI   ERROR,NOTFOUND                                                   
         ST    R3,AIO              RESTORE IO POINTER                           
         XC    FILENAME,FILENAME   CLEAR OVERRIDE FILE NAME                     
*                                                                               
VVALPRGX CLI   ERROR,0             SET CC                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE ACCOUNT                                                 
         SPACE 3                                                                
VVALAC12 GOTO1 ANY                 12 CHARACTERS GIVEN                          
         MVC   KEY,BLANKS                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+1(12),WORK                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         SPACE 1                                                                
VVALAC14 GOTO1 ANY                 14 CHARACTERS GIVEN                          
         MVC   KEY,BLANKS                                                       
         MVC   KEY(1),CUL                                                       
         MVC   KEY+1(14),WORK                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CHECK IF OK TO DELETE RECORD                                     
         SPACE 3                                                                
VCANWEDL CLI   MODE,RECDEL         CHECK JUST BEFORE THE DELETE                 
         BNE   XIT                                                              
         CLC   TWAALIAS,=C'OK2DELET'   LOOK FOR SPECIAL ALIAS                   
         BE    XIT                     WHEN DELETE IS ALLOWED                   
         LA    R2,CONACTH                                                       
         MVI   ERROR,CANTDEL                                                    
         B     VEXIT               OTHERWISE WE'RE OUT OF LUCK                  
         EJECT                                                                  
*              SUBSIDIARY INPUT ROUTINES                                        
         SPACE 3                                                                
VANYNAME CLI   OPTION,C'Y'         IF OPTION IS SET TO C'Y'                     
         BNE   XIT                                                              
         BAS   RE,BUMP                                                          
         GOTO1 NAMEOUT                                                          
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO EXTRACT THE NAME FROM A RECORD AND PLACE IN OUTPUT             
* AREA OF AT LEAST 36 BYTES                                                     
*                                                                               
* AT ENTRY, P1=A(RECORD), P2=A(OUTPUT AREA)                                     
*                                                                               
VSETNAME LM    R2,R3,0(R1)                                                      
         MVC   0(36,R3),BLANKS     CLEAR OUTPUT AREA                            
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACNMELQ',(R2)),0                      
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   XIT                                                              
         L     R6,12(R1)                                                        
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=Y(ACNMNAME-ACNAMED+1)                                        
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),ACNMNAME                                                 
         DROP  R6                                                               
         EJECT                                                                  
*              ACTIVITY ELEMENT ROUTINE                                         
         SPACE 3                                                                
         USING ACTRECD,R2                                                       
VVACTIVE DS    0H                                                               
         L     R2,AIO              GET RECORD TYPE                              
         TM    COMPSTA6,CPYSRAPP   ONLY FOR AGENCIES THAT USE ACTIVITY          
         BZ    VADA6                                                            
*                                                                               
         GOTO1 =V(ACRECTYP),DMCB,(0,(R2)),RR=YES                                
         MVC   WORK(1),0(R1)                                                    
*                                                                               
         LA    R4,RAPKROFF         OFFICE RECORDS                               
         LA    R5,L'RAPKOFF-1                                                   
         CLI   WORK,ACRTOGRO                                                    
         BE    VADA4                                                            
         LA    R4,RAPKROGR         OFFICE GROUP RECORDS                         
         LA    R5,L'RAPKOGR-1                                                   
         CLI   WORK,ACRTOGRG                                                    
         BE    VADA4                                                            
         LA    R4,RAPKRMED         MEDIA RECORDS                                
         LA    R5,L'RAPKMED-1                                                   
         CLI   WORK,ACRTPMD                                                     
         BE    VADA4                                                            
         LA    R4,RAPKRMGR         MEDIA GROUP RECORDS                          
         LA    R5,L'RAPKMGR-1                                                   
         CLI   WORK,ACRTMGR                                                     
         BE    VADA4                                                            
*                                                                               
         LA    R4,RAPKRWGR                                                      
         LA    R5,L'RAPKWGR-1                                                   
         CLI   WORK,ACRTWGR        WORK GROUP RECORDS                           
         BE    VADA4                                                            
         LA    R4,RAPKRWRK         WORK CODE RECORDS                            
         LA    R5,L'RAPKWRK-1                                                   
         CLI   WORK,ACRTWCO                                                     
         BE    VADA4                                                            
         LA    R4,RAPKRSCH         SCHEME RECORDS                               
         LA    R5,L'RAPKSCH-1                                                   
         CLI   WORK,ACRTSCH                                                     
         BE    VADA4                                                            
         LA    R4,RAPKRCAT         CATEGORY RECORDS                             
         LA    R5,L'RAPKCAT-1                                                   
         CLI   WORK,ACRTCAT                                                     
         BE    VADA4                                                            
*                                                                               
         LA    R4,RAPKROPT                                                      
         LA    R5,L'RAPKOPT-1                                                   
         CLI   WORK,ACRTPOP        OPTION RECORDS                               
         BE    VADA4                                                            
*                                                                               
         LA    R4,RAPKRPRL         PRICE LIST RECORDS                           
         LA    R5,L'RAPKPRL-1                                                   
         CLI   WORK,ACRTPRL                                                     
         BE    VADA4                                                            
*                                                                               
         LA    R4,RAPKRUSR         USER FIELD SELECT RECORDS                    
         LA    R5,L'RAPKUSR-1                                                   
         CLI   WORK,ACRTUFS                                                     
         BE    VADA4                                                            
*                                                                               
         LA    R4,RAPKRSUP         SUPPLIER (VENDOR) RECORDS                    
         LA    R5,L'RAPKACT-1                                                   
         CLC   =C'SV',ACTKUNT                                                   
         BNE   VADA2                                                            
         CLI   WORK,ACRTACTL                                                    
         BNE   VADA6                                                            
         B     VADA4                                                            
*                                                                               
VADA2    CLC   =C'SJ',ACTKUNT      PRODUCTION LEDGER...                         
         BNE   VADA6                                                            
         LA    R4,RAPKRJOB         JOB RECORDS                                  
         CLI   WORK,ACRTACTL                                                    
         BE    VADA4                                                            
         LA    R4,RAPKRCLI         CLIENT RECORDS                               
         ZIC   R1,LLEVA                                                         
         LA    R1,ACTKACT(R1)                                                   
         CLI   0(R1),C' '                                                       
         BE    VADA4                                                            
         LA    R4,RAPKRPRO         PRODUCT RECORDS                              
*                                                                               
VADA4    MVI   ELCODE,PTRELQ       BUILD FIRST PART OF POINTER ELEMENT          
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING PTRELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   PTREL,PTRELQ                                                     
         MVI   PTRLN,PTRLN1Q+L'ACCKEY                                           
         MVI   PTRTYPE,PTRTRAP                                                  
*                                                                               
         LA    R6,PTRCODE          ADD POINTER TO POINTER ELEMENT               
         USING RAPRECD,R6                                                       
         MVI   RAPKTYP,RAPKTYPQ                                                 
         MVC   RAPKCPY,CUL                                                      
         GOTO1 DATCON,DMCB,(1,TODAYP),(2,RAPKDATE)                              
         GOTO1 GETFACT,DMCB,(1,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   RAPKTIME,FATIME+1   TIME IN BINARY SECONDS                       
         STC   R4,RAPKRTYP                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RAPKKEY(0),ACTKEY                                                
         GOTO1 ADDELEM             ADD POINTER ELEMENT                          
         B     VADAX                                                            
*                                                                               
VADA6    TM    COMPSTAA,CPYSACCT   MAKE POINTERS FOR ACCENT?                    
         BZ    VADAX               NO                                           
         LA    R4,RAPKRPAL         YES, COST ONLY ONE FOR NOW                   
         LA    R5,L'RAPKACT-1                                                   
         CLC   =C'1C',ACTKUNT                                                   
         BE    VADA4                                                            
*                                                                               
VADAX    B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*              PERSONAL ID ELEMENT ROUTINES                                     
         SPACE 3                                                                
VPERSIN  CLI   TWAALIAS,X'41'      ADD A PERSON ID ELEMENT                      
         BL    VPIN10              IF AN ALIAS IS AVAILABLE                     
         MVI   ELCODE,PACELQ       TO WORK/WORK+10/WORK+20                      
         GOTO1 REMELEM                                                          
NEW      USING PACELD,ELEMENT                                                   
         MVI   NEW.PACEL,PACELQ        RESET IN CASE NOT FOUND                  
         MVI   NEW.PACLN,PACLNQ2                                                
         MVC   NEW.PACPERS,TWAALIAS    ADD NEW DATA                             
         MVC   NEW.PACDATE,TODAYP      OLD DATA STILL IN ELEMENT                
         GOTO1 ADDELEM                                                          
         DROP  NEW                                                              
*                                                                               
VPIN10   GOTO1 VACTIVE             ADD ACTIVITY ELEMENT                         
         B     XIT                                                              
         SPACE 1                                                                
VPERSOUT MVC   WORK,BLANKS         EXTRACT PERSON/DATE/PERSON ON DATE           
         MVI   ELCODE,ACPIELQ      TO WORK/WORK+10/WORK+20                      
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING ACPID,R6                                                         
         MVC   WORK(8),ACPIPERS                                                 
         GOTO1 DATCON,DMCB,(1,ACPIDATE),(8,WORK+10)                             
         DROP  R6                                                               
         MVC   WORK+20(8),WORK                                                  
         MVC   WORK+29(2),=C'ON'                                                
         MVC   WORK+32(8),WORK+10                                               
         GOTO1 SQUASHER,DMCB,WORK+20,20                                         
         B     XIT                                                              
         EJECT                                                                  
*              NAME ELEMENT (X'20') ROUTINES                                    
         SPACE 3                                                                
VNAMEIN  MVI   ELCODE,ACNMELQ      ADD A NAME ELEMENT                           
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING ACNAMED,R6                                                       
         MVI   ACNMEL,ACNMELQ                                                   
         ZIC   R1,5(R2)            (INPUT NAME LENGTH)                          
         LA    R1,2(R1)                                                         
         STC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNMNAME(0),8(R2)                                                
         DROP  R6                                                               
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         SPACE 1                                                                
VNAMEOUT MVI   ELCODE,ACNMELQ      NAME TO SCREEN HEADER                        
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING ACNAMED,R6                                                       
         ZIC   R3,0(R2)            GET FIELD LENGTH                             
         SH    R3,=H'8'             MINUS HEADER                                
         TM    1(R2),X'02'          IF EXTENEDED HEADER, SUBTRACT IT            
         BNO   *+8                                                              
         SH    R3,=H'8'                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLANKS                                                   
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         CR    R1,R3                                                            
         BNH   *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACNMNAME                                                 
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO HANDLE ADDRESS ELEMENTS (X'22')                      
         SPACE 3                                                                
VADDRIN  MVI   ELCODE,ACADELQ      ADD AN ADDRESS ELEMENT                       
         GOTO1 REMELEM                                                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         LA    R6,ELEMENT                                                       
         USING ACADDD,R6                                                        
         MVI   ACADEL,ACADELQ                                                   
         LA    R3,ACADADD                                                       
         SR    R4,R4                                                            
         LA    R0,5                                                             
         SPACE 1                                                                
VADIN2   CLI   5(R2),0                                                          
         BE    VADIN4                                                           
         MVC   0(26,R3),BLANKS                                                  
         ZIC   R1,5(R2)            (INPUT NAME LENGTH)                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         BAS   RE,BUMPTOUN                                                      
         LA    R3,26(R3)                                                        
         LA    R4,1(R4)                                                         
         BCT   R0,VADIN2                                                        
         SPACE 1                                                                
VADIN4   STC   R4,ACADLNES                                                      
         MH    R4,=H'26'                                                        
         LA    R4,3(R4)                                                         
         STC   R4,ACADLEN                                                       
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         B     XIT                                                              
         SPACE 1                                                                
VADDROUT LR    R3,R2               ADDRESS TO SCREEN HEADERS                    
         LA    R0,5                                                             
         SPACE 1                                                                
VAOUT2   MVC   8(26,R2),BLANKS     FIRST PRE-CLEAR 4 FIELDS                     
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,VAOUT2                                                        
         SPACE 1                                                                
         LR    R2,R3               RESTORE R2 TO FIRST HEADER                   
         MVI   ELCODE,ACADELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING ACADDD,R6                                                        
         ZICM  R0,ACADLNES,1                                                    
         BZ    XIT                                                              
         LA    R3,ACADADD                                                       
         DROP  R6                                                               
         SPACE 1                                                                
VAOUT4   MVC   8(26,R2),0(R3)                                                   
         LA    R3,26(R3)                                                        
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,VAOUT4                                                        
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO BUILD A MEDIA BUFFER                                               
*                                                                               
* AT ENTRY, P1  BYTE 0 = TYPE OF BUFFER                                         
*                        0 = MEDIA ELEMENT BUFFER                               
*                        1 = MEDIA CODE/MEDIA GROUP BUFFER (2 BYTES)            
*               BYTES 1 -3 = A(MEDIA BUFFER)                                    
*                                                                               
BLDMED   L     R2,0(R1)                                                         
         ZIC   R3,0(R1)            GET TYPE OF BUFFER                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),CUL                                                     
         GOTO1 HIGH                                                             
         B     BLDMED2                                                          
*                                                                               
BLDMED1  GOTO1 SEQ                                                              
*                                                                               
BLDMED2  CLC   KEY(2),KEYSAVE      TEST SAME COMPANY                            
         BNE   BLDMEDX                                                          
*                                                                               
         MVI   ELCODE,ACMDELQ      GET MEDIA ELEMENT                            
         BAS   RE,GETELIO                                                       
         USING ACMEDIAD,R6                                                      
         LTR   R3,R3               TEST TYPE OF BUFFER                          
         BP    BLDMED4             CODE/GROUP STYLE                             
*                                                                               
         ZIC   R1,ACMDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACMEDIAD                                                 
         LA    R2,1(R1,R2)         NEXT ELEMENT POSITION                        
*                                                                               
         MVI   ELCODE,ACMNELQ      LOOK FOR MEDIA NAME ELEMENTS                 
         BAS   RE,GETELIO                                                       
         USING ACMND,R6                                                         
BLDMED3  BNE   BLDMED1             NONE                                         
         ZIC   R1,ACMNLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACMND                                                    
         LA    R2,1(R1,R2)                                                      
         BAS   RE,NEXTEL           ANY MORE ELEMENTS                            
         B     BLDMED3                                                          
*                                                                               
         USING ACMEDIAD,R6                                                      
BLDMED4  MVC   0(1,R2),ACMDCODE                                                 
         MVC   1(1,R2),ACMDGRP                                                  
         LA    R2,2(R2)            NEXT ENTRY                                   
         B     BLDMED1                                                          
*                                                                               
BLDMEDX  MVI   0(R2),0                                                          
         B     XIT                                                              
         SPACE 2                                                                
* ROUTINE TO BUILD A WORKCODE BUFFER                                            
*                                                                               
* AT ENTRY, P1=A(WORKCODE BUFFER)                                               
* ON EXIT, WORKCODE ENTRIES ARE UNIT/LEDGER/WORKCODE ELEMENT                    
*                                                                               
BLDWC    L     R2,0(R1)                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         GOTO1 HIGH                                                             
         B     BLDWC2                                                           
*                                                                               
BLDWC1   GOTO1 SEQ                                                              
*                                                                               
BLDWC2   CLC   KEY(4),KEYSAVE      TEST SAME COMPANY                            
         BNE   BLDWCX                                                           
*                                                                               
         MVI   ELCODE,ACANELQ      ANALYSIS ELEMENT                             
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING ACANALD,R6                                                       
         MVI   ACANLEN,ACANLENQ    FORCE ALL TO NEW LENGTH                      
         MVC   0(2,R2),CUL+1       EXTRACT UNIT/LEDGER                          
         LA    R1,ACANLENQ-1       SET EXECUTE ELENGTH                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R2),ACANALD                                                  
         LA    R2,3(R1,R2)                                                      
*                                                                               
         MVI   ELCODE,ACNMELQ      SEE IF THERE IS A LONG NAME                  
         BAS   RE,GETELIO                                                       
         BNE   BLDWC1              NO LONG NAME                                 
*                                                                               
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN          SAVE LONG NAME ELEMENT IN TABLE              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACNAMED                                                  
         LA    R2,1(R1,R2)                                                      
*                                                                               
         B     BLDWC1                                                           
*                                                                               
BLDWCX   MVI   0(R2),0                                                          
         B     XIT                                                              
         SPACE 2                                                                
* ROUTINE TO BUILD AN OFFICE BUFFER                                             
*                                                                               
* AT ENTRY, P1=A(OFFICE BUFFER)                                                 
* ON EXIT, BUFFER CONTAINS N 3 BYTE ENTRIES-OFFICE CODE/OFFICE GROUP            
*                                                                               
BLDOFF   L     R2,0(R1)            R2=A(OFFICE BUFFER)                          
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF    OFFICE RECORDS                               
         MVC   ACOGCUL,CUL                                                      
         GOTO1 HIGH                                                             
         B     BLDOFF2                                                          
*                                                                               
BLDOFF1  GOTO1 SEQ                                                              
*                                                                               
BLDOFF2  CLC   ACOGKEY(ACOGOFC-ACOGKEY),KEYSAVE                                 
         BNE   BLDOFFX                                                          
*                                                                               
         MVI   ELCODE,ACGPELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACGPD,R6                                                         
         MVC   0(2,R2),ACOGOFC           EXTRACT OFFICE CODE                    
         MVC   2(1,R2),ACGPCODE          EXTRACT OFFICE GROUP                   
         LA    R2,3(R2)                                                         
         B     BLDOFF1                                                          
*                                                                               
BLDOFFX  MVI   0(R2),0             SET END OF BUFFER                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE FACILITATES THE ACTION OF SWITCHING, WITHIN A SINGLE             
* TRANSACTION, BETWEEN ONE OVERLAY AND ANOTHER BY CHANGING THE RECORD,          
* ACTION, AND KEY FIELDS ON THE SCREEN AND CALLING GENCON AGAIN.  FIRST         
* IT SAVES THE CURRENT TWA IN TEMPSTR AND PUSHES THE CURRENT OVERLAY            
* NUMBER ONTO A STACK.  THEN IT CHANGES THE RECORD, ACTION AND KEY              
* FIELDS TO THE DATA SPECIFIED IN THE PARAMETER LIST.  FINALLY, IT SETS         
* THE FLAG 'GOAGAIN' TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON            
* WHICH RETURNS BACK TO THE CONTROLLER.  THE CONTROLLER THEN RECOGNIZES         
* THE FLAG AND CALLS GENCON AGAIN.  THE PARAMTER LIST LOOKS AS FOLLOWS:         
*                                                                               
*       RECORD,ACTION,KEY1,KEY2,KEY3,KEY4,...,0                                 
*                                                                               
* 9/6/94 - SAVE/RESTORE 3584 BYTE SCREENS (X'E00', GENCON MAX)                  
*                                                                               
*                                                                               
CALL0    LR    R4,R1               ENTRY POINT FOR VTRANSF                      
         B     CALL6                                                            
*                                                                               
CALL     LR    R4,R1               SAVE POINTER TO PARMS                        
         ZIC   R3,CALLSP           GET STACK POINTER                            
         LR    R2,R3               R2=ORIGINAL STACK POINTER VALUE              
*                                                                               
CALL1    LA    RF,CALLSTK(R3)      RF=A(NEXT POSITION)                          
         MVC   0(1,RF),OVERLAY     SLOT IN OVERLAY NUMBER                       
         LA    R3,1(R3)                                                         
         STC   R3,CALLSP                                                        
         CLI   CALLSP,3            MORE THAN 3 NESTS ?                          
         BNH   CALL2               NO                                           
         LA    R3,0                YES, START OVER                              
         B     CALL1                                                            
*                                                                               
CALL2    SRL   R2,1                DIVIDE ORIGINAL LEVEL BY TWO                 
         LA    R2,3(R2)            ADD BACK THREE TWA PAGES                     
         SLL   R2,32-8             MOVE TO HIGH ORDER BYTE                      
         ICM   R2,3,TERM                                                        
         BAS   RE,SETP6LEN      SET P6 TO L=BB, WHERE BB IS BIN LEN             
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CLEARP6                                                       
*                                                                               
CALL4    STC   R3,BYTE             SAVE STACK LEVEL                             
         L     RE,ATIA             RE=DESTINATION                               
         LA    RF,3584             MOVE/CLEAR HALF A TWA                        
         LA    R0,CONRECH          START AT RECORD HEADER                       
         LA    R1,SAVAREA-CONRECH  MOVE RECORD HEADER UNTIL TWA0 SAVE           
         TM    BYTE,X'01'          TEST FOR ODD NUMBER                          
         BO    *+6                 YES                                          
         AR    RE,RF               NO-MOVE TO SECOND HALF OF TWA                
         MVCL  RE,R0                                                            
         MVC   0(L'CONHEAD,RE),CONHEAD                                          
         MVC   L'CONHEAD(2,RE),CONSERV+6                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R2),ATIA,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CALL6    XC    CONREC,CONREC       SET NEW RECORD TYPE                          
         OI    CONRECH+6,X'80'                                                  
         OI    CONRECH+4,X'80'     INPUT THIS TIME                              
         NI    CONRECH+4,X'DF'                                                  
         CLC   0(4,R4),=F'255'     TEST IF EQUATE PASSED                        
         BH    CALL7               NO                                           
         GOTO1 LOOKUP,1                                                         
*                                                                               
CALL7    L     RF,0(R4)                                                         
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
         CLC   0(4,R4),=F'255'     TEST IF EQUATE PASSED                        
         BH    CALL9               NO                                           
         GOTO1 LOOKUP,2                                                         
*                                                                               
CALL9    L     RF,0(R4)                                                         
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
         OC    0(0,R2),BLANKS      SPACE PAD THE FIELD                          
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
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)                                                         
         USING TIOBD,RE                                                         
         MVI   TIOBINDS,0                                                       
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO LOOK UP A RECORD OR ACTION EQUATE IN THE RECORD/               
* ACTION TABLE AND TO CHANGE THE VCALL PARAMETER LIST.                          
*                                                                               
* AT ENTRY, R1=1 FOR RECORD LOOKUP,2 FOR ACTION LOOKUP                          
*                                                                               
LOOKUP   NTR1  ,                                                                
         L     R2,0(R4)            GET EQUATE VALUE                             
         STC   R1,BYTE             SAVE ENTRY TYPE                              
         ZIC   R1,LRECACT          R1=L'RECORD/ACTION TABLE ENTRY               
         L     R3,ARECACT          R3=A(RECORD/ACTION TABLE)                    
*                                                                               
LOOKUP2  CLC   BYTE,0(R3)          MATCH ON ENTRY TYPE                          
         BE    LOOKUP4             YES                                          
         CLI   BYTE,X'01'          TEST FOR RECORD LOOKUP                       
         BNE   LOOKUP3             NO-BUMP TO NEXT ENTRY                        
         CLI   0(R3),X'04'         TEST FOR PROGRAM RECORD                      
         BE    LOOKUP4             YES                                          
*                                                                               
LOOKUP3  LA    R3,0(R1,R3)                                                      
         CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BNE   LOOKUP2                                                          
         DC    H'0'                YES-MUST BE BAD PARAMETER                    
*                                                                               
LOOKUP4  CLM   R2,1,9(R3)          MATCH ON EQUATE VALUE                        
         BNE   LOOKUP3             NO                                           
*                                                                               
LOOKUP6  LA    R5,1(R3)            R5=A(CHARACTER CONSTANT)                     
         TM    GENSTAT3,USEDICT    TEST USING LANGUAGE DICTIONARY               
         BNO   LOOKUP8             NO                                           
*                                                                               
         MVC   DUB,1(R3)           EXTRACT DICTATE CONSTANT                     
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
         LA    R5,DUB              R5=A(TRANSLATED CONSTANT)                    
*                                                                               
LOOKUP8  ST    R5,0(R4)            REPLACE A(CHARACTER STRING) IN PARM          
         LA    R0,8                COMPUTE CHARACTER STRING LENGTH              
         LA    R5,7(R5)                                                         
         CLI   0(R5),C' '          TEST FOR SIGNIFICANT CHARACTER               
         BH    *+10                YES                                          
         BCTR  R5,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,0(R4)            SET LENGTH IN PARM                           
*                                                                               
LOOKUPX  B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER/SCREEN TO                    
* THE VALUE ON TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN'            
* FLAG TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN                   
* THE CONTROLLER REGAINS CONTROL, IT CALLS GENCON AGAIN WITH THE                
* RESTORED SCREEN.                                                              
*                                                                               
RETURN   ZIC   R3,CALLSP           GET STACK POINTER                            
         BCTR  R3,0                DECREMENT POINTER TO POP STACK               
         STC   R3,CALLSP                                                        
         LA    RE,CALLSTK(R3)                                                   
         MVC   RETURNED,LASTOV     NOTE OVERLAY RETURNING FROM                  
         MVC   OVERLAY,0(RE)       EXTRACT NEW OVERLAY NUMBER                   
*                                                                               
RETURN2  LR    R2,R3                                                            
         SRL   R2,1                DIVIDE LEVEL BY TWO                          
         LA    R2,3(R2)            START AT TWA PAGE 3                          
         SLL   R2,32-8             MOVE PAGE TO HOB                             
         ICM   R2,3,TERM                                                        
         BAS   RE,SETP6LEN      SET P6 TO L=BB, WHERE BB IS BIN LEN             
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CLEARP6                                                       
*                                                                               
RETURN4  LA    RE,CONRECH                                                       
         LA    RF,SAVAREA-CONRECH  MOVE RECORD HEADER UP TO SAVE AREA           
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         LA    R3,1(R3)            RESTORE ORIGINAL LEVEL                       
         STC   R3,BYTE                                                          
         TM    BYTE,X'01'          TEST FOR ODD LEVEL                           
         BO    *+8                 YES                                          
         A     R0,=F'3584'         NO-MUST BE IN SECOND HALF OF PAGE            
         MVCL  RE,R0                                                            
*                                                                               
RETURN6  LA    R2,CONRECH                                                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
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
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         MVC   TIOBCNT(1),TWASCR   SET SCREEN# IN TIO BLOCK                     
         OI    TIOBINDS,TIOBSCRN                                                
         DROP  RF                                                               
*                                                                               
RETURN10 MVI   GOAGAIN,C'Y'        SET TO CALL GENCON AGAIN                     
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
*                                                                               
* SET PARM 6 OF DMCB TO LEN FOR TEMPSTR READ                                    
*                                                                               
SETP6LEN EQU  *                                                                 
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=H'7168'                                              
         BR    RE                                                               
*                                                                               
CLEARP6  EQU  *                                                                 
         XC    DMCB+20(4),DMCB+20                                               
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
OVSCRTAB DS    0CL2                                                             
         DC    X'03',X'F3'         JOB MAINT                                    
         DC    X'11',X'E1'         CLIENT LIST                                  
         DC    X'12',X'E2'         PRODUCT LIST                                 
         DC    X'13',X'E3'         JOB LIST                                     
         DC    X'14',X'E4'         OPTION LIST                                  
         DC    X'1A',X'EA'         JOB NUMBER LIST                              
         DC    X'25',X'B2'         LINK LIST                                    
         DC    X'45',X'B5'         LINK MAINT                                   
         DC    X'40',X'B0'         SCHEME LIST                                  
         DC    X'31',X'C1'         CATEGORY DETAIL                              
         DC    X'41',X'B1'         CATEGORY MAINT                               
         DC    X'32',X'C2'         JOB ESTIMATE                                 
         DC    X'33',X'C3'         JOB ELIST                                    
         DC    X'34',X'C4'         PANEL MAINT                                  
         DC    X'38',X'C8'         TEXT MAINT                                   
         DC    X'3A',X'CA'         SESSION MAINT                                
         DC    X'3B',X'CB'         SESSION LIST                                 
         DC    X'44',X'B4'         PANEL LIST                                   
         DC    X'48',X'B8'         TEXT LIST                                    
         DC    X'47',X'B7'         JOB SUMMARY                                  
         DC    X'01',X'F1'         CLI2                                         
         DC    X'02',X'F2'         PRO2                                         
         DC    X'03',X'F3'         JOB2                                         
         DC    X'59',X'AD'         ADVERTISER LIST                              
         DC    X'61',X'AF'         ACCOUNT GROUP LIST                           
         DC    X'64',X'BE'         AUTHORIZATION LIST                           
         DC    X'65',X'BF'         AUTHORIZATION LIST                           
         DC    X'66',X'B6'         FUNDING MAINTENANCE                          
         DC    X'67',X'BB'         AUTHORIZATION HISTORY                        
         DC    X'70',X'00'         AUTHORIZATION HISTORY                        
         DC    X'71',X'F3'         JOB@@@@                                      
         DC    X'FF'               EOT                                          
         EJECT                                                                  
* CLEARF - CLEAR AND FOUT FIELDS                                                
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLEARF   LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),BLANKS                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
* DISOPT  -  DISPLAY OPTION VALUE                                               
*                                                                               
* AT ENTRY,  P1  BYTE 0    = 0                                                  
*                BYTES 1-3 = A(OPTION ELEMENT)                                  
*         OR P1  BYTE 0    = OPTION NUMBER                                      
*                BYTES 1-3 = A(GOBLOCK)                                         
*            P2  BYTES 1-3 = A(FIELD HEADER)                                    
*                                                                               
DISOPT   MVC   BYTE,0(R1)                                                       
         LM    R2,R3,0(R1)                                                      
         XC    DOBLOCK(DOBLOCKX-DOBLOCK),DOBLOCK                                
         CLI   BYTE,0              TEST IF OPTION NUMBER PASSED                 
         BNE   DISOPT2             YES                                          
         ST    R2,DOAOPTEL                                                      
         B     DISOPT4                                                          
*                                                                               
DISOPT2  MVC   DOOPTNUM,BYTE                                                    
         ST    R2,DOAGOBLK                                                      
*                                                                               
DISOPT4  ST    R3,DOAFLDH          A(OUTPUT FIELD HEADER)                       
         MVC   DOAOPTAB,AOPTTAB    A(OPTION TABLE)                              
         MVC   DOACOM,ACOMFACS                                                  
         MVC   DOOFCLN,OFCLNGTH                                                 
         MVC   DODPTLN,DPTLNGTH                                                 
         GOTO1 =V(ACDISOPT),DMCB,DOBLOCK,RR=YES                                 
         MVC   AOPTENT,DOAOPTEN                                                 
         B     XIT                                                              
         EJECT                                                                  
* VALOPT  -  VALIDATE OPTION VALUE                                              
*                                                                               
* AT ENTRY,  P1 = A(FIELD HEADER)                                               
*            P2 = A(OPTION ELEMENT) - OPTION NUMBER SET IN ELEMENT              
*                                                                               
* ON EXIT, CC=EQ IF OK, CC=NEQ IF ERROR FOUND IN INPUT                          
*                                                                               
*                                                                               
VALOPT   LM    R2,R3,0(R1)                                                      
         GOTO1 =V(ACVALOPT),DMCB,(R2),(R3),(RC),RR=YES                          
         CLI   DMCB+4,0                                                         
         BE    VOX                 GOOD EXIT                                    
         CLI   DMCB+4,X'FF'                                                     
         BE    ERRCUR                                                           
*                                                                               
VOERR    MVI   ERROR,INVALID                                                    
         LTR   RB,RB               SET CC=NEQ                                   
         B     XIT                                                              
*                                                                               
VOX      CR    RB,RB               SET CC=EQ                                    
         B     XIT                                                              
         EJECT                                                                  
*********************************************************                       
* ERRCUR - ERROR EXIT WITH CURSOR POSITIONING           *                       
*                                                       *                       
* AT ENTRY, R2=A(FIELD HEADER) AND ERRNDX=INDEX INTO    *                       
*           FIELD FOR CURSOR (OPTIONAL)                 *                       
*********************************************************                       
         SPACE 1                                                                
ERRCUR   OI    6(R2),X'80'                                                      
         CLI   OFFLINE,C'Y'        TEST IF OFFLINE                              
         BE    VERRXIT             YES-ALL DONE NOW                             
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         LR    RF,R2                                                            
         S     RF,ATWA             RF=DISP. TO ERROR FIELD HEADER               
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,ERRNDX                                                  
         OI    TIOBINDS,TIOBSETC                                                
         B     VERRXIT                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************                     
* CHKACC - CHECK ACCESS TO RECORD/ACTION PAIRS            *                     
* AT ENTRY, DUB(2)=RECORD NUMBER/ACTION NUMBER            *                     
* ON EXIT,  CC=EQ FOR ACCESS, CC=NEQ FOR NO ACCESS        *                     
***********************************************************                     
         SPACE 1                                                                
CHKACC   L     R4,ARECACT          R4=A(RECORD/ACTION TABLE)                    
         ZIC   R3,LRECACT          R3=L'RECORD/ACTION TABLE ENTRY               
*                                                                               
CHKACC2  CLI   0(R4),X'FF'         TEST FOR EOT                                 
         BNE   *+6                                                              
         DC    H'0'                DUMP IF NOT IN TABLE                         
         CLI   0(R4),X'03'         TEST FOR RECORD/ACTION ENTRY                 
         BNE   CHKACC3             NO                                           
         CLC   DUB(2),1(R4)        MATCH ON RECORD/ACTION                       
         BE    CHKACC4                                                          
*                                                                               
CHKACC3  LA    R4,0(R3,R4)         NEXT TABLE ENTRY                             
         B     CHKACC2                                                          
*                                                                               
CHKACC4  OC    SECMASKS,SECMASKS   TEST FOR ACCESS CONTROL                      
         BZ    CHKACCY             NO-RETURN 'YES'                              
         CLC   12(4,R4),=F'0'      TEST ANY SECURITY FOR RECORD/ACTION          
         BE    CHKACCY                                                          
         MVC   FULL,SECMASKS                                                    
         NC    FULL,12(R4)         TEST IF ANY BITS IN COMMON                   
         BZ    CHKACCN             NO-NO ACCESS                                 
*                                                                               
CHKACCY  CR    RB,RB               SET CC=EQ FOR 'YES'                          
         B     XIT                                                              
*                                                                               
CHKACCN  LTR   RB,RB               SET CC=NEQ FOR 'NO'                          
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE HANDLES PASSIVE POINTER MAINTENANCE FOR ALL STRAIGHT             
* MAINTENANCE GENCON OVERLAYS.  THE OVERLAY WILL CALL THIS ROUTINE              
* FOR EVERY MODE, AND THE ROUTINE WILL CALL EITHER SAVPTRS OR CHGPTRS           
* WHEN APPROPRIATE.                                                             
*                                                                               
* PARAMETER 1 - BYTE 0     X'80' IF AIO IS EMULATED RECORD                      
*             - BYTES 1-3  A(SAVED POINTERS BLOCK)                              
*                                                                               
MODPTRS  DS    0H                                                               
         MVC   BYTE,0(R1)          BYTE = EMULATED RECORD INDICATOR             
         L     R2,0(R1)            R2 = A(SAVED POINTERS BLOCK)                 
*                                                                               
         CLI   MODE,RECREST        IF MODE RECREST OR VALREC FOR                
         BE    MP10                    ACTION ADD                               
         CLI   MODE,VALREC                                                      
         BNE   MP20                                                             
         CLI   ACTNUM,ACTADD                                                    
         BNE   MP20                                                             
*                                  THEN CLEAR SAVED POINTERS BLOCK              
MP10     GOTO1 VSAVPTRS,DMCB,(BYTE,0),(R2)                                      
         B     MPX                                                              
*                                                                               
MP20     CLI   MODE,RECDEL         ELSE IF MODE RECDEL OR VALREC FOR            
         BE    MP30                    ACTION CHANGE                            
         CLI   MODE,VALREC                                                      
         BNE   MP40                                                             
*                                  THEN SET SAVED PASSIVE POINTERS              
MP30     GOTO1 VSAVPTRS,DMCB,(BYTE,AIO),(R2)                                    
         B     MPX                                                              
*                                                                               
MP40     CLI   MODE,XRECADD        ELSE IF MODE XRECADD, XRECPUT,               
         BE    MP50                    OR XRECREST                              
         CLI   MODE,XRECPUT                                                     
         BE    MP50                                                             
         CLI   MODE,XRECREST                                                    
         BNE   MP60                                                             
*                                  THEN CHANGE PASSIVE POINTERS                 
MP50     GOTO1 VCHGPTRS,DMCB,(BYTE,AIO),(R2)                                    
         B     MPX                                                              
*                                                                               
MP60     CLI   MODE,XRECDEL        ELSE IF MODE XRECDEL                         
         BNE   MPX                                                              
*                                  THEN DELETE SAVED POINTERS                   
         GOTO1 VCHGPTRS,DMCB,(BYTE,0),(R2)                                      
*                                                                               
MPX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SAVES THE BLOCK OF POINTERS THAT ARE DERIVED FROM THE            
* RECORD PASSED.                                                                
*                                                                               
* PARAMETER 1  -  BYTE 0     X'80' IF RECORD IS EMULATED                        
*              -  BYTES 1-3  A(RECORD) OR ZERO TO CLEAR BLK FOR AN ADD          
* PARAMETER 2  -  A(BLOCK TO SAVE POINTERS)                                     
*                                                                               
SAVPTRS  DS    0H                                                               
         CLI   EMULATE,C'Y'        DO NOTHING IF AGENCY DOESN'T EMULATE         
         BNE   SPX                                                              
*                                                                               
         LM    R2,R3,0(R1)         SET R2 AND R3 FROM PARMS                     
*                                                                               
         MVI   0(R3),X'FF'         CLEAR BLOCK FOR ADD IF CALLER WANTS          
         OC    1(3,R1),1(R1)                                                    
         BZ    SPX                                                              
*                                                                               
         MVI   0(R3),0             TELL ACPROCPT TO BUILD ACTIVE PTR            
*                                                                               
         LA    R5,PARAS            BUILD ACPROCPT PARAMETER BLOCK               
         USING CPTRD,R5                                                         
         XC    CPBLOCK,CPBLOCK                                                  
         MVI   CPEMUFLG,CPEMUYES   THIS FLAG TELLS ACPROCPT WHETHER             
         TM    0(R1),X'80'            THE RECORD IS EMULATED OR NOT             
         BO    *+8                                                              
         MVI   CPEMUFLG,CPEMUNO                                                 
         GOTO1 =V(ACPROCPT),DMCB,(0,(R2)),(R3),(R5),RR=YES                      
         DROP  R5                                                               
SPX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SETS THE POINTERS ON THE DIRECTORY FOR THE RECORD                
* PASSED.  IT USES THE OLD BLOCK CONTAINING THE POINTERS FOR THE                
* ORIGINAL RECORD TO DETERMINE WHICH POINTERS NEED TO CHANGE AND                
* HOW.                                                                          
*                                                                               
* PARAMETER 1  -  BYTE 0     X'80' IF RECORD IS EMULATED                        
*              -  BYTES 1-3  A(RECORD) OR ZERO TO DELETE OLD BLOCK              
* PARAMETER 2  -  A(OLD POINTER BLOCK)                                          
*                                                                               
CHGPTRS  DS    0H                                                               
         L     RF,AIO              SAVE AIO'S KEY                               
         MVC   PPKEYSV,0(RF)                                                    
*                                                                               
         CLI   EMULATE,C'Y'        DO NOTHING IF AGENCY DOESN'T EMULATE         
         BNE   CPX                                                              
*                                                                               
         LM    R2,R3,0(R1)         SET R2 AND R3 FROM PARMS                     
*                                                                               
         TM    0(R1),X'80'         IF EMULATED RECORD THEN SET FILE             
         BZ    CP2                    VARIABLES FOR NEW                         
         GOTO1 VSETNEW                                                          
*                                                                               
CP2      LA    R4,BLOCK            USE EMPTY BLOCK FOR DELETING                 
         MVI   0(R4),X'FF'                                                      
         OC    1(3,R1),1(R1)                                                    
         BZ    CP5                                                              
*                                                                               
         MVI   0(R4),0             TELL ACPROCPT TO BUILD ACTIVE PTR            
*                                                                               
         LA    R5,PARAS            BUILD ACPROCPT PARAMETER BLOCK               
         USING CPTRD,R5                                                         
         XC    CPBLOCK,CPBLOCK                                                  
         MVI   CPEMUFLG,CPEMUYES   THIS FLAG TELLS ACPROCPT WHETHER             
         TM    0(R1),X'80'            THE RECORD IS EMULATED OR NOT             
         BO    *+8                                                              
         MVI   CPEMUFLG,CPEMUNO                                                 
         GOTO1 =V(ACPROCPT),DMCB,(0,(R2)),(R4),(R5),RR=YES                      
         DROP  R5                                                               
*                                                                               
CP5      CLI   0(R4),X'FF'         IF NEW BLOCK HAS ACTIVE                      
         BE    CP7                                                              
         XC    BIGKEY,BIGKEY       THEN GET DISK ADDRESS OF RECORD              
         MVC   BIGKEY(L'ACCKEY),0(R4)                                           
         OI    DMINBTS,X'08'       PASS DELETES--OPTION RECORD                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACCKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DMDSKADD,BIGKEY+(ACCKDA-ACCKEY)                                  
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
CP7      CLI   0(R3),X'FF'         IF OLD POINTER BLOCK IS EMPTY                
         BNE   *+8                                                              
         MVI   0(R4),0             THEN SKIP ACTIVE IN NEW BLOCK                
*                                                                               
         CLI   0(R4),X'FF'         IF NEW POINTER BLOCK IS EMPTY                
         BNE   *+8                                                              
         MVI   0(R3),0             THEN SKIP ACTIVE IN OLD BLOCK                
*                                                                               
         LR    R6,R4               MARK DUPLICATE OLD AND NEW POINTERS          
         USING ACCRECD,R6              WITH ZERO                                
*                                                                               
CP10     CLI   0(R6),X'FF'         WHILE NOT END OF NEW POINTERS                
         BE    CP100                                                            
*                                                                               
         LR    R5,R3               R5 = A(OLD POINTERS)                         
*                                                                               
CP30     CLI   0(R5),X'FF'         WHILE NOT END OF OLD POINTERS                
         BE    CP90                                                             
*                                                                               
         CLC   ACCKEY,0(R5)        IF OLD AND NEW MATCH                         
         BNE   CP50                                                             
*                                                                               
         MVI   0(R6),0             THEN MARK BOTH WITH A ZERO                   
         MVI   0(R5),0                                                          
         B     CP90                                                             
*                                                                               
CP50     LA    R5,ACCKLEN(R5)      BUMP R5 TO NEXT OLD POINTER                  
         B     CP30                                                             
*                                                                               
CP90     LA    R6,ACCKLEN(R6)      BUMP R6 TO NEXT NEW POINTER                  
         B     CP10                                                             
*                                                                               
CP100    BAS   RE,DELPTRS          DELETE OUTDATED POINTERS                     
         BAS   RE,ADDPTRS          ADD NEW POINTERS                             
*                                                                               
         STCM  R2,8,BYTE           RESET TO EMULATION IF CALLER IS              
         TM    BYTE,X'80'             USING IT                                  
         BZ    CPX                                                              
         GOTO1 VSETEMU                                                          
*                                                                               
CPX      L     RF,AIO              RESTORE AIO'S KEY                            
         MVC   0(L'PPKEYSV,RF),PPKEYSV                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SCANS THE SAVED POINTER BLOCK AND DELETES ALL POINTERS           
* THAT ARE NOT MARKED WITH A ZERO.                                              
*                                                                               
DELPTRS  NTR1                                                                   
         LR    R5,R3               R5 = A(OLD POINTER BLOCK)                    
         LA    R6,BIGKEY           R6 = A(BIGKEY)                               
         USING ACCRECD,R6                                                       
*                                                                               
DP10     CLI   0(R5),X'FF'         IF END OF LIST THEN DONE DELETING            
         BE    DP100                                                            
         CLI   0(R5),0             IF MARKED WITH ZERO THEN SKIP                
         BE    DP90                                                             
*                                                                               
         MVC   ACCKEY,0(R5)        GET KEY FROM DIRECTORY                       
         OI    GENSTAT4,USEBIGKY                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   ACCKEY,KEYSAVE      DO NOTHING IF NOT FOUND                      
         BNE   DP90                                                             
*                                                                               
         OI    ACCKSTA,X'80'       MARK DELETED AND WRITE BACK                  
         GOTO1 WRITE                                                            
*                                                                               
DP90     LA    R5,ACCKLEN(R5)      BUMP TO NEXT KEY TO DELETE AND               
         B     DP10                    LOOP BACK                                
*                                                                               
DP100    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SCANS THE NEW POINTER BLOCK AND ADDS ALL POINTERS                
* THAT ARE NOT MARKED WITH A ZERO.                                              
*                                                                               
ADDPTRS  NTR1                                                                   
         LR    R5,R4               R5 = A(NEW POINTER BLOCK)                    
         LA    R6,BIGKEY           R6 = A(BIGKEY)                               
         USING ACCRECD,R6                                                       
*                                                                               
AP10     CLI   0(R5),X'FF'         IF END OF LIST THEN DONE ADDING              
         BE    AP100                                                            
         CLI   0(R5),0             IF MARKED WITH ZERO THEN SKIP                
         BE    AP90                                                             
*                                                                               
         MVC   ACCKEY,0(R5)        GET KEY FROM DIRECTORY                       
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         CLC   ACCKEY,KEYSAVE      IF KEY FOUND THEN WRITE BACK NEW             
         BNE   AP50                                                             
         MVC   ACCKEY(ACCKLEN),0(R5)                                            
         MVC   ACCKDA,DMDSKADD                                                  
         GOTO1 WRITE                                                            
         B     AP90                                                             
*                                  ELSE ADD NEW POINTER                         
AP50     MVC   ACCKEY(ACCKLEN),0(R5)                                            
         MVC   ACCKDA,DMDSKADD                                                  
         GOTO1 ADD                                                              
*                                                                               
AP90     LA    R5,ACCKLEN(R5)      BUMP TO NEXT KEY TO DELETE AND               
         B     AP10                    LOOP BACK                                
*                                                                               
AP100    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES A FIELD TO BE NUMERIC, PACKS IT AND SETS               
* CONDITION CODE TO INDICATE IF FIELD IS ZERO                                   
*                                                                               
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,3                                                          
         B     VEXIT                                                            
*                                                                               
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    XITR1                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
* THIS ROUTINE SETS THE NECESSARY GENCON VARIABLES TO ALLOW FOR THE             
* USE OF THE NEW ACC FILE.                                                      
*                                                                               
SETNEW   DS    0H                                                               
         MVC   SYSDIR,=C'ACCDIR  ' FORCE NEW DIR/FIL READS                      
         MVC   SYSFIL,=C'ACCMST  '                                              
         MVC   LSTATUS,=Y(L'ACCKSTA)                                            
         MVC   DATADISP,=Y(ACCRFST-ACCRECD)                                     
         OI    GENSTAT4,USEBIGKY                                                
         MVI   USEIO,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SETS THE NECESSARY GENCON VARIABLES TO ALLOW FOR THE             
* USE OF THE ACC EMULATION FILE.                                                
*                                                                               
SETEMU   DS    0H                                                               
         MVC   SYSDIR,=C'ACCFIL  ' FORCE ACCOUNT FILE READ                      
         MVC   SYSFIL,=C'ACCFIL  '                                              
         MVC   LSTATUS,=Y(L'ACSTATUS)                                           
         MVC   DATADISP,=Y(ACCORFST)                                            
         NI    GENSTAT4,X'FF'-USEBIGKY                                          
         MVI   USEIO,C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SET THE STRING OF FIELDS STARING AT 0(R2)) TO THE FILTER VALUES IN            
* THE RSTEL OF THE RECORD IN AIO                                                
*----------------------------------------------------------------------         
*                                                                               
VSETFLTS DS    0H                                                               
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   VSETFX                                                           
         LA    RE,FILTTAB                                                       
*                                                                               
VSETF30  XR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BZ    VSETFX              END OF TABLE                                 
         LA    R1,0(R6,R1)         ADDRESS FILTER VALUE                         
         MVC   8(1,R2),0(R1)       MOVE FILTER TO SCREEN.                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1               R2 = A(NEXT FLDHDR)                          
         LA    RE,1(RE)            NEXT FILTER OFFSET                           
         B     VSETF30                                                          
VSETFX   B     XIT                                                              
*                                                                               
*----------------------------------------------------------------------         
* SET THE STRING OF 1 BYTE FIELDS AT 0(R1)) TO THE FILTER VALUES IN             
* THE RSTEL AT 0(R6),                                                           
* USED BY SETCLI,SETPRO, SETJOB                                                 
*----------------------------------------------------------------------         
*                                                                               
GETFILTS NTR1                                                                   
         LA    RE,FILTTAB                                                       
*                                                                               
GETF30   XR    R2,R2                                                            
         ICM   R2,1,0(RE)                                                       
         BZ    GETFX               END OF TABLE                                 
         LA    R2,0(R2,R6)         ADDRESS FILTER VALUE                         
         MVC   0(1,R1),0(R2)       MOVE FILTER TO OUTPUT AREA                   
         LA    RE,1(RE)            NEXT FILTER OFFSET                           
         LA    R1,1(R1)            NEXT OUTPUT AREA                             
         B     GETF30                                                           
*                                                                               
GETFX    B     XIT                                                              
*                                                                               
CLREFFS  LA    RF,EFLTS            CLEAR EFFECTIVE FILTER VALS                  
         LA    R0,NFLTS                                                         
         XC    0(1,RF),0(RF)                                                    
         LA    RF,1(RF)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
* SET EFLTS FROM THE FILTER VALUES IN 0(R1)                                     
*                                                                               
SETEFFS  LA    RF,EFLTS                                                         
         LA    R0,NFLTS                                                         
SETEF30  CLI   0(R1),C' '          IS THIS FILTER VALUE DEFINED                 
         BNH   *+10                NO                                           
         MVC   0(1,RF),0(R1)       UPDATE EFFECIVE VALUE                        
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,SETEF30                                                       
         BR    RE                                                               
*                                                                               
*----------------------------------------------------------------------         
* UPDATE RSTEL WITH THE FILTER VALUES IN THE FIELDS STARTING AT 0(R2)           
* ERROR-XITS IF A FIELD IS INVALID, R6 IS ASSUMED TO POINT TO RSTEL             
* I/P PARM HOB OF R1=X'00' UPDATE ALL FILTER VALUES                             
*          HOB OF R1=X'20' UPDATE ONLY FILTER VALUES WHERE FIELD HAS            
*                          BEEN "MODIFIED THIS TIME"                            
*----------------------------------------------------------------------         
VVALFLTS DS    0H                                                               
         STCM  R1,8,BYTE           EXTRACT HOB OF R1                            
         MVI   ERRNDX,0                                                         
         LR    R4,R2               SAVE A(FIELD 1) IN R4                        
         LA    R0,NFLTS                                                         
         OI    6(R2),X'80'         XMIT ALL FIELDS UP FRONT                     
         BAS   RE,BUMP             INCASE OF ERROR                              
         BCT   R0,*-8                                                           
         LR    R2,R4               REST A(FIELD 1) FROM R4                      
*                                                                               
         MVI   ERROR,ACTSHORT      ENSURE THERE IS ROOM IN ELEMENT              
         CLI   1(R6),RSTLN3Q                                                    
         BL    ERRCUR                                                           
         LA    R4,FILTTAB                                                       
         MVI   ERROR,INVALID                                                    
*                                                                               
VVALF040 SR    R1,R1                                                            
         ICM   R1,1,0(R4)                                                       
         BZ    VVALFX              END OF TABLE                                 
*                                                                               
         LA    R1,0(R1,R6)         R1 = A(FILTER IN STATUS ELEMENT).            
         CLI   5(R2),0                                                          
         BE    VVALF060            NO INPUT.                                    
*                                                                               
         CLI   8(R2),C'.'          . = NO FILTER                                
         BE    ERRCUR                                                           
*                                                                               
         CLI   8(R2),C'?'          ? = HELP                                     
         BE    ERRCUR                                                           
*                                                                               
         CLI   8(R2),C'*'          * = NEGATIVE                                 
         BE    ERRCUR                                                           
*                                                                               
         TM    8(R2),X'40'         THIS BIT MUST BE ON                          
         BZ    ERRCUR                                                           
*                                                                               
         CLI   BYTE,X'20'          ONLY WANT FIELDS INPUT THIS TIME             
         BNE   VVALF050            NO, SET ELEMENT                              
         TM    4(R2),X'20'         WAS FIELD PREV VALIDATED                     
         BO    *+10                YES                                          
VVALF050 MVC   0(1,R1),8(R2)       MOVE SCREEN VALUE TO ELEMENT.                
         B     VVALF080                                                         
*                                                                               
VVALF060 CLC   CUL+1(2),=C'SJ'     IS THIS A PRODUCTION ACCOUNT                 
*                                  (TESTED THE CHEAP WAY)                       
         BNE   *+8                 NO, NOT ALLOWED TO CLEAR FILTERS             
         MVI   0(R1),C' '          CLEAR THE FILTER IN THE ELEMENT              
         MVC   8(1,R2),0(R1)       MOVE FILTER TO SCREEN.                       
*                                                                               
*VALF080 OI    4(R2),X'20'         MARK FIELD AS VALIDATED                      
VVALF080 OI    6(R2),X'80'         AND SET TO TRANSMIT                          
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1               R2 = A(NEXT FLDHDR)                          
         LA    R4,1(R4)            RE = A(NEXT FILTER TABLE ENTRY).             
         B     VVALF040                                                         
*                                                                               
VVALFX   MVI   ERROR,0                                                          
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
*                                                                               
FILTTAB  DC    AL1(RSTFILT1-RSTELD)                                             
         DC    AL1(RSTFILT2-RSTELD)                                             
         DC    AL1(RSTFILT3-RSTELD)                                             
         DC    AL1(RSTFILT4-RSTELD)                                             
         DC    AL1(RSTFILT5-RSTELD)                                             
         DC    AL1(0)                                                           
         EJECT                                                                  
*              DELETE A JOB                                                     
*                                                                               
VJOBDEL  GOTO1 ANY                                                              
         LA    RF,KEY                                                           
         ST    RF,MJOB                                                          
*                                                                               
         MVI   MACTION,C'D'                                                     
         MVC   MCOMFACS,ACOMFACS                                                
         MVC   MPRORATA,=V(PRORATA)                                             
         MVI   MDRAFT,C'N'                                                      
*                                                                               
         LA    RF,TWAALIAS                                                      
         ST    RF,MPERSON                                                       
*                                                                               
         XC    MRAPPER,MRAPPER                                                  
         TM    COMPSTA6,CPYSRAPP   ONLY FOR AGENCIES THAT USE ACTIVITY          
         BZ    *+10                                                             
         MVC   MRAPPER,=V(ACRAPPER)                                             
*                                                                               
         GOTO1 =V(ACJOBMNT),MPARM,RR=YES                                        
         CLI   4(R1),X'00'         IS THERE AN ERROR                            
         BE    VJDEL50             NO.                                          
         MVI   ERROR,CANTDEL                                                    
         CLI   5(R1),X'00'         DO WE HAVE A SPECIFIC MESSAGE?               
         BE    VEXIT               NO, X'FF' WILL DO                            
         MVC   ERROR,5(R1)         EXTRACT USER ERROR                           
         B     VEXIT                                                            
*                                                                               
VJDEL50  OI    DMINBTS,X'08'                                                    
         GOTO1 READ                GET RECORD IN BUFFER                         
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              RESTORE A JOB                                                    
*                                                                               
VJOBRES  GOTO1 ANY                                                              
         LA    RF,KEY                                                           
         ST    RF,MJOB                                                          
*                                                                               
         MVI   MACTION,C'R'                                                     
         MVC   MCOMFACS,ACOMFACS                                                
         MVC   MPRORATA,=V(PRORATA)                                             
         MVI   MDRAFT,C'N'                                                      
*                                                                               
         LA    RF,TWAALIAS                                                      
         ST    RF,MPERSON                                                       
*                                                                               
         XC    MRAPPER,MRAPPER                                                  
         TM    COMPSTA6,CPYSRAPP   ONLY FOR AGENCIES THAT USE ACTIVITY          
         BZ    *+10                                                             
         MVC   MRAPPER,=V(ACRAPPER)                                             
*                                                                               
         GOTO1 =V(ACJOBMNT),MPARM,RR=YES                                        
         MVI   ERROR,RECNTDEL                                                   
         CLI   MPARM+4,0                                                        
         BNE   VEXIT                                                            
*                                                                               
         GOTO1 READ                GET RECORD IN BUFFER                         
         B     XIT                                                              
         EJECT                                                                  
*              OPEN A JOB                                                       
*                                                                               
VJOBOPN  GOTO1 ANY                                                              
         LA    RF,KEY                                                           
         ST    RF,MJOB                                                          
*                                                                               
         MVI   MACTION,C'O'                                                     
         MVC   MCOMFACS,ACOMFACS                                                
         MVC   MPRORATA,=V(PRORATA)                                             
         MVI   MDRAFT,C'N'                                                      
*                                                                               
         LA    RF,TWAALIAS                                                      
         ST    RF,MPERSON                                                       
*                                                                               
         XC    MRAPPER,MRAPPER                                                  
         TM    COMPSTA6,CPYSRAPP   ONLY FOR AGENCIES THAT USE ACTIVITY          
         BZ    *+10                                                             
         MVC   MRAPPER,=V(ACRAPPER)                                             
*                                                                               
         GOTO1 =V(ACJOBMNT),MPARM,RR=YES                                        
         MVC   ERROR,5(R1)         SET DATAMGR ERROR CODE, IF ANY               
         CLI   MPARM+4,0                                                        
         BNE   VEXIT                                                            
*                                                                               
         GOTO1 READ                GET RECORD IN BUFFER                         
         B     XIT                                                              
         EJECT                                                                  
*              CLOSE A JOB                                                      
*                                                                               
VJOBCLS  GOTO1 ANY                                                              
         LA    RF,KEY                                                           
         ST    RF,MJOB                                                          
*                                                                               
         MVI   MACTION,C'C'                                                     
         MVC   MCOMFACS,ACOMFACS                                                
         MVC   MPRORATA,=V(PRORATA)                                             
         MVI   MDRAFT,C'N'                                                      
         MVC   MGETOPT,GETOPT                                                   
*                                                                               
         LA    RF,TWAALIAS                                                      
         ST    RF,MPERSON                                                       
*                                                                               
         XC    MRAPPER,MRAPPER                                                  
         TM    COMPSTA6,CPYSRAPP   ONLY FOR AGENCIES THAT USE ACTIVITY          
         BZ    *+10                                                             
         MVC   MRAPPER,=V(ACRAPPER)                                             
*                                                                               
         GOTO1 =V(ACJOBMNT),MPARM,RR=YES                                        
         MVC   ERROR,5(R1)         SET DATAMGR ERROR CODE, IF ANY               
         CLI   MPARM+4,0                                                        
         BNE   VEXIT                                                            
*                                                                               
         GOTO1 READ                GET RECORD IN BUFFER                         
         B     XIT                                                              
         EJECT                                                                  
*              INFORMATIONAL MESSAGE ROUTINE                                    
*                                                                               
VINFOXIT TM    GENSTAT2,USGETTXT   CALL FOR GETTXT                              
         BZ    VINFX                                                            
         XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMTYP,GTMINF       SET INFORMATIONAL TYPE                       
         MVC   GTMSGNO,MYMSGNO     AND MESSAGE NUMBER                           
         MVC   GTMSYS,GETMSYS      AND MESSAGE SYSTEM                           
         CLI   MYMSYS,0            IF OVERRIDE SYSTEM AROUND                    
         BE    *+10                                                             
         MVC   GTMSYS,MYMSYS       USE IT                                       
*                                                                               
VINFX    GOTO1 ERREX                                                            
         EJECT                                                                  
*              ERROR MESSAGE WITH EXTRA DATA                                    
*                                                                               
VXTRAXIT XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMTYP,GTMERR       SET ERROR TYPE                               
         MVC   GTMSGNO,MYMSGNO     AND MESSAGE NUMBER                           
         MVC   GTMSYS,GETMSYS      AND MESSAGE SYSTEM                           
         LA    RE,WORK                                                          
         STCM  RE,7,GTASUBST       THIS IS FOR &1 MESSAGES                      
         OC    WORK,WORK                                                        
         BZ    VXTRAX              JUST EXIT IF WORK ISN'T SET                  
         LA    RE,WORK+1                                                        
         STCM  RE,7,GTATXT         THIS IS FOR &T MESSAGES                      
         MVC   GTLTXT,WORK                                                      
*                                                                               
VXTRAX   GOTO1 ERRCUR                                                           
         EJECT                                                                  
         SPACE 1                                                                
*              PERSONAL ID2 ELEMENT ROUTINES                                    
         SPACE 3                                                                
VPERSIN2 MVI   ELCODE,PACELQ       TO WORK/WORK+10/WORK+20                      
         GOTO1 REMELEM                                                          
NEW      USING PACELD,ELEMENT                                                   
         MVI   NEW.PACEL,PACELQ        RESET IN CASE NOT FOUND                  
         MVI   NEW.PACLN,PACLNQ2                                                
         MVC   NEW.PACPERS2,TWAALIAS   ADD NEW DATA                             
         MVC   NEW.PACDATE2,TODAYP     OLD DATA STILL IN ELEMENT                
         GOTO1 ADDELEM                                                          
         GOTO1 VACTIVE             ADD ACTIVITY ELEMENT                         
         B     XIT                                                              
         DROP  NEW                                                              
         SPACE 1                                                                
VPERSOU2 MVC   WORK,BLANKS         EXTRACT PERSON/DATE/PERSON ON DATE           
         MVI   ELCODE,PACELQ       TO WORK/WORK+10/WORK+20                      
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING PACELD,R6                                                        
         CLI   PACLN,PACLNQ2                                                    
         BL    XIT                                                              
         MVC   WORK(8),PACPERS2                                                 
         GOTO1 DATCON,DMCB,(1,PACDATE2),(8,WORK+10)                             
         DROP  R6                                                               
         MVC   WORK+20(8),WORK                                                  
         MVC   WORK+29(2),=C'ON'                                                
         MVC   WORK+32(8),WORK+10                                               
         GOTO1 SQUASHER,DMCB,WORK+20,20                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CHECKS WETHER EQUIVALENT ACCOUNT INPUT CONFORMS TO    *          
* THE RULES DEFINED IN THE X'CA' ELEMENT.                            *          
* PARAMETER 1 = ADDRESS OF X'CA' ELEMENT                             *          
* PARAMETER 2 = ADDRESS OF INPUT FLD HEADER                          *          
* ON EXIT EITHER DISPLAY ERROR MESSAGE OR EXITS OK                   *          
*--------------------------------------------------------------------*          
*                                                                               
         USING APRELD,R6                                                        
VCKCARUL L     R6,0(R1)            GET ADDRESS OF X'CA' ELEMENT                 
         L     R2,4(R1)            ADDRESS OF FIELD HEADER                      
*                                                                               
         ZIC   R3,5(R2)            GET LENGTH OF INPUT FLD                      
         LA    R2,8(R2)            R2 NOW POINTS TO INPUT                       
*                                                                               
         L     R1,AIO              GET KEY                                      
         MVI   ERROR,INPTLONG      INPUT FIELD LEN TOO LONG                     
         ZIC   R5,APRTLEN          GET LEN OF RULE                              
         CR    R3,R5               R3 HAS LEN OF INPUT                          
         BH    CKCANO              NOT GOOD                                     
         MVI   ERROR,INPTSHRT      INPUT FIELD LEN TOO SHORT                    
         BL    CKCANO                                                           
*                                                                               
         LA    R5,APRMLEN          POINT TO LENGTH OF FIRST LEVEL               
         SR    R0,R0               CLEAR INDEX                                  
         ZIC   R1,APRNLEVS         TOTAL NUMBER OF LEVEL                        
*                                                                               
CKCA20   STC   R1,NLEVS            TOTAL LEVELS LEFT,OUTER LOOP CNTR            
         ZIC   R6,0(R5)            DON'T SCREW UP R6 IT IS A LOOP CNTR          
         LA    R3,1(R5)            POINT TO FIRST CHARACTER OF RULE             
*                                                                               
CKCA30   AHI   R0,1                POSITION OF CHARACTER COUNTER                
         MVI   ERROR,SBALPNUM                                                   
         CLI   0(R3),X'83'         IS IT LOWER CASE 'C'                         
         BE    CKCA40              SKIP CHCKNG FOR LOWER 'A' AND '#'            
         MVI   ERROR,SBALPHA                                                    
         CLI   0(R3),X'81'         IS IT LOWER CASE 'A'                         
         BNE   CKCA50              CHECK FOR NUMBER SIGN                        
*                                                                               
CKCA40   CLI   0(R2),C'A'                                                       
         BL    CKCAERR                                                          
         CLI   0(R2),C'I'                                                       
         BNH   CKCA80                                                           
         CLI   0(R2),C'J'                                                       
         BL    CKCAERR                                                          
         CLI   0(R2),C'R'                                                       
         BNH   CKCA80                                                           
         CLI   0(R2),C'S'                                                       
         BL    CKCAERR                                                          
         CLI   0(R2),C'Z'                                                       
         BNH   CKCA80                                                           
         CLI   0(R3),X'83'         IF ALPHANUMERIC THEN CHECK FOR NUM           
         BE    CKCA60                                                           
         B     CKCAERR                                                          
*                                                                               
CKCA50   MVI   ERROR,SBNUMER                                                    
         CLI   0(R3),C'#'                                                       
         BNE   CKCA70                                                           
*                                                                               
CKCA60   CLI   0(R2),X'F0'         IS IT BETWEEN 0 AND 9                        
         BL    CKCAERR                                                          
         CLI   0(R2),X'F9'                                                      
         BH    CKCAERR                                                          
         B     CKCA80                                                           
*                                                                               
CKCA70   MVI   ERROR,SBCONST                                                    
         CLC   0(1,R3),0(R2)       HAS TO BE A CONSTANT                         
         BNE   CKCAERR                                                          
*                                                                               
CKCA80   LA    R2,1(R2)            BUMP ACCOUNT BY 1 CHARACTER                  
         LA    R3,1(R3)            BUMP RULE BY 1 CHARACTER                     
         BCT   R6,CKCA30                                                        
*                                                                               
         IC    R6,0(R5)                                                         
         AHI   R6,1                ADD OVERHEAD                                 
         AR    R5,R6                                                            
         ZIC   R1,NLEVS            GET HOW MANY MORE LEVELS LEFT                
         BCT   R1,CKCA20           GET NEXT RULE ELEMENT                        
         B     CKCAYES                                                          
*                                                                               
CKCAERR  LA    R1,DMCB                                                          
         L     R2,4(R1)            ADDRESS OF FIELD HEADER                      
         ST    R2,ACURFORC                                                      
         BCTR  R0,0                SUBTRACT TO GET DISPLACEMENT                 
         STCM  R0,1,ERRNDX                                                      
         AHI   R0,1                ADD BACK TO GET POSITION                     
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,3              SET LENGTH OF DATA                           
         CVD   R0,DUB                                                           
         CURED (R0),(2,WORK+1),0,ALIGN=LEFT                                     
         OI    GENSTAT2,USGETTXT                                                
         MVC   MYMSGNO1,ERROR                                                   
         GOTO1 XTRAXIT                                                          
         B     ERRCUR                                                           
*                                                                               
CKCANO   GOTO1 ERREX                                                            
CKCAYES  B     XIT                                                              
         EJECT                                                                  
*              COMMON EXIT ROUTINES                                             
*                                                                               
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
INVEND   MVI   ERROR,INVALID                                                    
         B     VERRXIT                                                          
*                                                                               
MYEND    MVI   ERROR,SUPPLIED                                                   
         B     VERRXIT                                                          
*                                                                               
VEXIT    DS    0H                                                               
VERRXIT  ZIC   R1,CONRECH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,FALCOMP                                                       
         BE    VERRX02                                                          
         C     R2,ATWA                                                          
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VERRX02  OI    6(R2),X'40'         POSITION CURSOR                              
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
         CLI   ERROR,SUPPLIED                                                   
         BE    VERRX2                                                           
         GOTO1 ERREX               SYSTEM MESSAGE                               
*                                                                               
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
FALCOMP  CLC   CONREC(0),=CL8'FALINK  '                                         
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS TABLES LTORG                                           
*                                                                               
DEFPANEL DC    C'9999'                                                          
BLANKS   DC    CL132' '                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TABLES OF RECORDS ACTIONS AND COMBINATIONS                       
*                                                                               
         DS    0D                                                               
RECACTS  DS    0CL16                                                            
*                                                                               
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
* XL4 SECURITY ACCESS BITS                                                      
*                                                                               
         DC    X'01',C'CLIENT  ',AL1(RECNCLI),X'0000',4X'00'                    
         DC    X'01',C'PRODUCT ',AL1(RECNPRO),X'0000',4X'00'                    
         DC    X'01',C'JOB     ',AL1(RECNJOB),X'0000',4X'00'                    
         DC    X'01',C'OPTION  ',AL1(RECNOPT),X'0000',4X'00'                    
         DC    X'01',C'USER    ',AL1(RECNUSR),X'0000',4X'00'                    
         DC    X'01',C'MGROUP  ',AL1(RECNMG),X'0000',4X'00'                     
         DC    X'01',C'MEDIA   ',AL1(RECNMED),X'0000',4X'00'                    
         DC    X'01',C'COMMENT ',AL1(RECNCOM),X'0000',4X'00'                    
         DC    X'01',C'OGROUP  ',AL1(RECNOG),X'0000',4X'00'                     
         DC    X'01',C'OFFICE  ',AL1(RECNOFF),X'0000',4X'00'                    
         DC    X'01',C'WGROUP  ',AL1(RECNWG),X'0000',4X'00'                     
         DC    X'01',C'WORKCODE',AL1(RECNWORK),X'0000',4X'00'                   
         DC    X'01',C'SCHEME  ',AL1(RECNSCH),X'0000',4X'00'                    
         DC    X'01',C'CATEGORY',AL1(RECNCAT),X'0000',4X'00'                    
         DC    X'01',C'PANEL   ',AL1(RECNPAN),X'0000',4X'00'                    
         DC    X'01',C'FIELD   ',AL1(RECNFLD),X'0000',4X'00'                    
         DC    X'01',C'TEXT    ',AL1(RECNTEXT),X'0000',4X'00'                   
         DC    X'04',C'ESTIMATE',AL1(RECNEST),X'0000',4X'00'                    
         DC    X'01',C'ELIST   ',AL1(RECNEL),X'0000',4X'00'                     
         DC    X'01',C'JNUMBER ',AL1(RECNJNUM),X'0000',4X'00'                   
         DC    X'01',C'SESSION ',AL1(RECNSES),X'0000',4X'00'                    
         DC    X'01',C'LINK    ',AL1(RECNLNK),X'0000',4X'00'                    
         DC    X'01',C'STUDIO  ',AL1(RECNSTU),X'0000',4X'00'                    
         DC    X'01',C'PRICE   ',AL1(RECNPRC),X'0000',4X'00'                    
         DC    X'01',C'GROUP   ',AL1(RECNJGRP),X'0000',4X'00'                   
         DC    X'01',C'AUTH    ',AL1(RECNAUTH),X'0000',4X'00'                   
         DC    X'01',C'FUND    ',AL1(RECNFUND),X'0000',4X'00'                   
         DC    X'01',C'ADVERT  ',AL1(RECNADV),X'0000',4X'00'                    
         DC    X'01',C'AGROUP  ',AL1(RECNAG),X'0000',4X'00'                     
         DC    X'01',C'CLI2    ',AL1(RECNXCLI),X'0000',4X'00'                   
         DC    X'01',C'PRO2    ',AL1(RECNXPRO),X'0000',4X'00'                   
         DC    X'01',C'JOB2    ',AL1(RECNXJOB),X'0000',4X'00'                   
         DC    X'01',C'FALINK  ',AL1(RECNFALN),X'0000',4X'00'                   
         DC    X'01',C'JOB@@@@ ',AL1(RECNJOB@),X'0000',4X'00'                   
         DC    X'01',C'ITEMS   ',AL1(RECNARTS),X'0000',4X'00'                   
*                                                                               
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
* XL4 SECURITY ACCESS BITS                                                      
*                                                                               
         DC    X'02',C'ADD     ',AL1(ACTNADD,ACTNADD,0),4X'00'                  
         DC    X'02',C'CHANGE  ',AL1(ACTNCHA,ACTNCHA,0),4X'00'                  
         DC    X'02',C'CLOSE   ',AL1(ACTNCHA,50,0),4X'00'                       
         DC    X'02',C'OPEN    ',AL1(ACTNCHA,50,0),4X'00'                       
         DC    X'02',C'DISPLAY ',AL1(ACTNDIS,ACTNDIS,0),4X'00'                  
         DC    X'02',C'DETAIL  ',AL1(ACTNDET,ACTNDET,0),4X'00' DE               
         DC    X'02',C'DELETE  ',AL1(ACTNDEL,ACTNDEL,0),4X'00'                  
         DC    X'02',C'SELECT  ',AL1(ACTNSEL,ACTNSEL,0),4X'00'                  
         DC    X'02',C'REPORT  ',AL1(ACTNREP,ACTNREP,0),4X'00' R + RE           
         DC    X'02',C'RESTORE ',AL1(ACTNRES,ACTNRES,0),4X'00'                  
         DC    X'02',C'MAINT   ',AL1(ACTNMNT,ACTNMNT,0),4X'00'                  
         DC    X'02',C'ESTIMATE',AL1(ACTNEST,ACTNEST,0),4X'00'                  
         DC    X'02',C'COPY    ',AL1(ACTNCOPY,ACTNCOPY,0),4X'00'                
         DC    X'02',C'LIST    ',AL1(ACTNLIST,ACTNLIST,0),4X'00'                
         DC    X'02',C'UPDATE  ',AL1(ACTNUPD,ACTNUPD,0),4X'00'                  
         DC    X'02',C'ELIST   ',AL1(ACTNEL,ACTNEL,0),4X'00'                    
         DC    X'02',C'RENAME  ',AL1(ACTNREN,ACTNREN,0),4X'00'                  
         DC    X'02',C'CONTROL ',AL1(ACTNCON,ACTNCON,0),4X'00'                  
         DC    X'02',C'SUMMARY ',AL1(ACTNSUM,ACTNSUM,0),4X'00'                  
         DC    X'02',C'AUTO    ',AL1(ACTNADD,51,0),4X'00'                       
         DC    X'02',C'CYCLE   ',AL1(ACTNCYC,ACTNCYC,0),4X'00'                  
         DC    X'02',C'HISTORY ',AL1(ACTNDIS,52,0),4X'00'                       
*                                                                               
* ACTION EQUATES:                                                               
*              50 = VALID FOR JOB ONLY                                          
*              51 = VALID FOR JOB ONLY                                          
*              52 = VALID FOR AUTHORIZATION                                     
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 3                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS 01=NON GENCON          
*                                                           LIST                
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
* XL4 SECURITY ACCESS BITS                                                      
*                                                                               
*                                           SC  SP  OK                          
*                                             OV  RP                            
* CLIENT                                                                        
*                                                                               
         DC    X'03',AL1(RECNCLI,ACTNADD),X'F101000180',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNCLI,ACTNCHA),X'F101000180',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNCLI,ACTNDIS),X'F101000180',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNCLI,ACTNDEL),X'F101000180',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNCLI,ACTNRES),X'F101000180',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNCLI,ACTNLIST),X'E111001181',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* PRODUCT                                                                       
*                                                                               
         DC    X'03',AL1(RECNPRO,ACTNADD),X'F202000280',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPRO,ACTNCHA),X'F202000280',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPRO,ACTNDIS),X'F202000280',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRO,ACTNDEL),X'F202000280',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPRO,ACTNRES),X'F202000280',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPRO,ACTNLIST),X'E212001281',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* JOB                                                                           
*                                                                               
         DC    X'03',AL1(RECNJOB,ACTNADD),X'F303000380',C'    '                 
         DC    AL1(CAT5Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJOB,51),X'F303000380',C'    '                      
         DC    AL1(CAT5Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJOB,ACTNCHA),X'F303000380',C'    '                 
         DC    AL1(CAT3Q+CAT5Q,0,0,0)                                           
         DC    X'03',AL1(RECNJOB,50),X'F303000380',C'    '                      
         DC    AL1(CAT5Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJOB,ACTNDIS),X'F303000380',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJOB,ACTNDEL),X'F303000380',C'    '                 
         DC    AL1(CAT5Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJOB,ACTNRES),X'F303000380',C'    '                 
         DC    AL1(CAT5Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJOB,ACTNLIST),X'E313001381',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJOB,ACTNREP),X'D323002338',C'JLJL'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJOB,ACTNEL),X'C333003381',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJOB,ACTNEST),X'C232003281',C'JEJE'                 
         DC    AL1(CAT4Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJOB,ACTNDET),X'C737003781',C'JDJD'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJOB,ACTNSUM),X'B747004781',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJOB,ACTNCYC),X'CD3D003D81',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* OPTION                                                                        
*                                                                               
         DC    X'03',AL1(RECNOPT,ACTNMNT),X'F404000481',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOPT,ACTNDIS),X'F404000481',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOPT,ACTNLIST),X'E414001481',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOPT,ACTNREP),X'D424002438',C'OLOL'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOPT,ACTNCON),X'F505000538',C'OCOC'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOPT,ACTNSUM),X'E515001538',C'OSOS'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* USER                                                                          
*                                                                               
         DC    X'03',AL1(RECNUSR,ACTNADD),X'F606000680',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNUSR,ACTNCHA),X'F606000680',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNUSR,ACTNDIS),X'F606000680',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNUSR,ACTNDEL),X'F606000680',C'    '                 
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNUSR,ACTNSEL),X'F606000680',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNUSR,ACTNRES),X'F606000680',C'    '                 
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNUSR,ACTNLIST),X'E616001680',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNUSR,ACTNREP),X'D626002678',C'ULUL'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNUSR,ACTNUPD),X'D727002738',C'UUUU'                 
         DC    AL1(CAT1Q,0,0,0)                                                 
*                                                                               
* MGROUP                                                                        
*                                                                               
         DC    X'03',AL1(RECNMG,ACTNADD),X'F707000780',C'    '                  
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNMG,ACTNCHA),X'F707000780',C'    '                  
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNMG,ACTNDIS),X'F707000780',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNMG,ACTNDEL),X'F707000780',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNMG,ACTNSEL),X'F707000780',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNMG,ACTNRES),X'F707000780',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNMG,ACTNLIST),X'E717001780',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNMG,ACTNREP),X'F707000778',C'MGMG'                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* MEDIA                                                                         
*                                                                               
         DC    X'03',AL1(RECNMED,ACTNADD),X'F808000880',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNMED,ACTNCHA),X'F808000880',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNMED,ACTNDIS),X'F808000880',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNMED,ACTNDEL),X'F808000880',C'    '                 
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNMED,ACTNSEL),X'F808000880',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNMED,ACTNRES),X'F808000880',C'    '                 
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNMED,ACTNLIST),X'E818001880',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNMED,ACTNREP),X'D828002878',C'MLML'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* COMMENT                                                                       
*                                                                               
         DC    X'03',AL1(RECNCOM,ACTNADD),X'F909000980',C'    '                 
         DC    AL1(CAT1Q+CAT3Q+CAT5Q,0,0,0)                                     
         DC    X'03',AL1(RECNCOM,ACTNCHA),X'F909000980',C'    '                 
         DC    AL1(CAT1Q+CAT3Q+CAT5Q,0,0,0)                                     
         DC    X'03',AL1(RECNCOM,ACTNDIS),X'F909000980',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNCOM,ACTNDEL),X'F909000980',C'    '                 
         DC    AL1(CAT3Q+CAT5Q,0,0,0)                                           
         DC    X'03',AL1(RECNCOM,ACTNSEL),X'F909000980',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNCOM,ACTNRES),X'F909000980',C'    '                 
         DC    AL1(CAT3Q+CAT5Q,0,0,0)                                           
         DC    X'03',AL1(RECNCOM,ACTNLIST),X'E919001980',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNCOM,ACTNREP),X'D929002938',C'NLNL'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* OGROUP                                                                        
*                                                                               
         DC    X'03',AL1(RECNOG,ACTNADD),X'FB0B000B80',C'    '                  
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNOG,ACTNCHA),X'FB0B000B80',C'    '                  
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNOG,ACTNDIS),X'FB0B000B80',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOG,ACTNDEL),X'FB0B000B80',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNOG,ACTNSEL),X'FB0B000B80',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOG,ACTNRES),X'FB0B000B80',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNOG,ACTNLIST),X'EB1B001B80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOG,ACTNREP),X'FB0B000B78',C'OGOG'                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* OFFICE                                                                        
*                                                                               
         DC    X'03',AL1(RECNOFF,ACTNADD),X'FC0C000C80',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNOFF,ACTNCHA),X'FC0C000C80',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNOFF,ACTNDIS),X'FC0C000C80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOFF,ACTNDEL),X'FC0C000C80',C'    '                 
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNOFF,ACTNSEL),X'FC0C000C80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOFF,ACTNRES),X'FC0C000C80',C'    '                 
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNOFF,ACTNLIST),X'EC1C001C80',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNOFF,ACTNREP),X'FC0C000C78',C'OFOF'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* WGROUP                                                                        
*                                                                               
         DC    X'03',AL1(RECNWG,ACTNADD),X'FD0D000D80',C'    '                  
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNWG,ACTNCHA),X'FD0D000D80',C'    '                  
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNWG,ACTNDIS),X'FD0D000D80',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNWG,ACTNDEL),X'FD0D000D80',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNWG,ACTNSEL),X'FD0D000D80',C'    '                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNWG,ACTNRES),X'FD0D000D80',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNWG,ACTNLIST),X'ED1D001D80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNWG,ACTNREP),X'FD0D000D38',C'WGWG'                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* WORKCODE                                                                      
*                                                                               
         DC    X'03',AL1(RECNWORK,ACTNADD),X'FE0E000E80',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNWORK,ACTNCHA),X'FE0E000E80',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNWORK,ACTNDIS),X'FE0E000E80',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNWORK,ACTNDEL),X'FE0E000E80',C'    '                
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNWORK,ACTNSEL),X'FE0E000E80',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNWORK,ACTNRES),X'FE0E000E80',C'    '                
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNWORK,ACTNLIST),X'EE1E001E80',C'    '               
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNWORK,ACTNREP),X'DE2E002E38',C'WLWL'                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* SCHEME                                                                        
*                                                                               
         DC    X'03',AL1(RECNSCH,ACTNADD),X'C030003080',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNSCH,ACTNCHA),X'C030003080',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNSCH,ACTNCOPY),X'CF3F003F80',C'    '                
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNSCH,ACTNDIS),X'C030003080',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSCH,ACTNDEL),X'C030003080',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNSCH,ACTNRES),X'C030003080',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNSCH,ACTNLIST),X'B040004081',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSCH,ACTNREP),X'D020002078',C'SRSR'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* CATEGORY                                                                      
*                                                                               
         DC    X'03',AL1(RECNCAT,ACTNMNT),X'B141004181',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNCAT,ACTNDET),X'C131003181',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
*                                                                               
* PANEL                                                                         
*                                                                               
         DC    X'03',AL1(RECNPAN,ACTNADD),X'C434003480',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPAN,ACTNCHA),X'C434003480',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPAN,ACTNDIS),X'C434003480',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPAN,ACTNDEL),X'C434003480',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPAN,ACTNRES),X'C434003480',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPAN,ACTNCOPY),X'C534003481',C'    '                
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPAN,ACTNREN),X'C534003481',C'    '                 
         DC    AL1(CAT2Q,0,0,0)                                                 
         DC    X'03',AL1(RECNPAN,ACTNLIST),X'B444004481',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* FIELD                                                                         
*                                                                               
         DC    X'03',AL1(RECNFLD,ACTNMNT),X'C636003681',C'FMFM'                 
         DC    AL1(CAT2Q,0,0,0)                                                 
*                                                                               
* TEXT                                                                          
*                                                                               
         DC    X'03',AL1(RECNTEXT,ACTNMNT),X'C838003881',C'    '                
         DC    AL1(CAT2Q+CAT4Q,0,0,0)                                           
         DC    X'03',AL1(RECNTEXT,ACTNDEL),X'C838003881',C'    '                
         DC    AL1(CAT2Q+CAT4Q,0,0,0)                                           
         DC    X'03',AL1(RECNTEXT,ACTNLIST),X'B848004881',C'    '               
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* ESTIMATE                                                                      
*                                                                               
         DC    X'03',AL1(RECNEST,ACTNADD),X'C939004980',C'    '                 
         DC    AL1(CAT2Q+CAT4Q,0,0,0)                                           
         DC    X'03',AL1(RECNEST,ACTNCHA),X'C939004980',C'    '                 
         DC    AL1(CAT2Q+CAT4Q,0,0,0)                                           
         DC    X'03',AL1(RECNEST,ACTNDIS),X'C939004980',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNEST,ACTNDEL),X'C939004980',C'    '                 
         DC    AL1(CAT2Q+CAT4Q,0,0,0)                                           
         DC    X'03',AL1(RECNEST,ACTNSEL),X'C939004980',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNEST,ACTNRES),X'C939004980',C'    '                 
         DC    AL1(CAT2Q+CAT4Q,0,0,0)                                           
         DC    X'03',AL1(RECNEST,ACTNLIST),X'C939004980',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNEST,ACTNREP),X'C939004938',C'ESES'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* ELIST                                                                         
*                                                                               
         DC    X'03',AL1(RECNEL,ACTNREP),X'D535003578',C'ELEJ'                  
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* JNUMBER                                                                       
*                                                                               
         DC    X'03',AL1(RECNJNUM,ACTNADD),X'FA0A000A80',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJNUM,ACTNCHA),X'FA0A000A80',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJNUM,ACTNDIS),X'FA0A000A80',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJNUM,ACTNDEL),X'FA0A000A80',C'    '                
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNJNUM,ACTNRES),X'FA0A000A80',C'    '                
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNJNUM,ACTNSEL),X'FA0A000A80',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNJNUM,ACTNLIST),X'EA1A001A81',C'    '               
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* TALENT SESSION ESTIMATING                                                     
*                                                                               
         DC    X'03',AL1(RECNSES,ACTNADD),X'CA3A003A80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSES,ACTNCHA),X'CA3A003A80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSES,ACTNDIS),X'CA3A003A80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSES,ACTNLIST),X'CB3B003B81',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSES,ACTNDEL),X'CA3A003A80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSES,ACTNRES),X'CA3A003A80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* STUDIO TYPE MAINTENANCE                                                       
*                                                                               
         DC    X'03',AL1(RECNSTU,ACTNADD),X'CC3C003C80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSTU,ACTNCHA),X'CC3C003C80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSTU,ACTNDIS),X'CC3C003C80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSTU,ACTNLIST),X'EF1F001F80',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNSTU,ACTNSEL),X'CC3C003C80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* PRICE MAINTENANCE                                                             
*                                                                               
         DC    X'03',AL1(RECNPRC,ACTNADD),X'CE3E003E80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNCHA),X'CE3E003E80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNDIS),X'CE3E003E80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNLIST),X'E010001080',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNSEL),X'CE3E003E80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNDEL),X'CE3E003E80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNRES),X'CE3E003E80',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNPRC,ACTNREP),X'D222002278',C'PRPR'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* LINK MAINTENANCE                                                              
*                                                                               
         DC    X'03',AL1(RECNLNK,ACTNMNT),X'B545004581',C'    '                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNLNK,ACTNLIST),X'B225002581',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNLNK,ACTNREP),X'B325002578',C'LRLR'                 
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* ADVERTISER                                                                    
*                                                                               
         DC    X'03',AL1(RECNADV,ACTNMNT),X'AC58005880',C'    '                 
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNADV,ACTNLIST),X'AD59005981',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
*                                                                               
*                                                                               
* AGROUP                                                                        
*                                                                               
         DC    X'03',AL1(RECNAG,ACTNADD),X'AE60006080',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNAG,ACTNCHA),X'AE60006080',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNAG,ACTNDIS),X'AE60006080',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNAG,ACTNDEL),X'AE60006080',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNAG,ACTNSEL),X'AE60006080',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNAG,ACTNRES),X'AE60006080',C'    '                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNAG,ACTNLIST),X'AF61006181',C'    '                 
         DC    AL1(0,0,0,0)                                                     
*                                                                               
* CLI2                                                                          
*                                                                               
         DC    X'03',AL1(RECNXCLI,ACTNCHA),X'DA01000180',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNXCLI,ACTNDIS),X'DA01000180',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* PRO2                                                                          
*                                                                               
         DC    X'03',AL1(RECNXPRO,ACTNCHA),X'DB02000280',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNXPRO,ACTNDIS),X'DB02000280',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* JOB2                                                                          
*                                                                               
         DC    X'03',AL1(RECNXJOB,ACTNCHA),X'DC03000280',C'    '                
         DC    AL1(CAT3Q+CAT5Q,0,0,0)                                           
         DC    X'03',AL1(RECNXJOB,ACTNDIS),X'DC03000280',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* GROUP                                                                         
*                                                                               
         DC    X'03',AL1(RECNJGRP,ACTNADD),X'B962006280',C'    '                
         DC    AL1(CAT3Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJGRP,ACTNCHA),X'B962006280',C'    '                
         DC    AL1(CAT3Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJGRP,ACTNDIS),X'B962006280',C'    '                
         DC    AL1(CAT3Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJGRP,ACTNDEL),X'B962006280',C'    '                
         DC    AL1(CAT3Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJGRP,ACTNSEL),X'B962006280',C'    '                
         DC    AL1(CAT3Q,0,0,0)                                                 
         DC    X'03',AL1(RECNJGRP,ACTNLIST),X'BA63006380',C'    '               
         DC    AL1(CAT3Q,0,0,0)                                                 
*                                                                               
* AUTHORIZATION                                                                 
*                                                                               
         DC    X'03',AL1(RECNAUTH,ACTNADD),X'BE64006480',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,ACTNCHA),X'BE64006480',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,ACTNDIS),X'BE64006480',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,ACTNSEL),X'BE64006480',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,ACTNDEL),X'BE64006480',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,ACTNRES),X'BE64006480',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,ACTNLIST),X'BF65006581',C'    '               
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
         DC    X'03',AL1(RECNAUTH,52),X'BB67006780',C'    '                     
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* FUND                                                                          
*                                                                               
         DC    X'03',AL1(RECNFUND,ACTNMNT),X'B666006681',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*        DC    X'03',AL1(RECNFUND,ACTNREP),X'DA68006878',C'FRFR'                
*        DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
* FALINK                                                                        
*                                                                               
         DC    X'03',AL1(RECNFALN,ACTNMNT),X'0070007080',C'    '                
         DC    AL1(0,0,0,0)                                                     
*                                                                               
* JOB@@@@ (THIS IS FOR JOB UPLOAD DDLINK SCRIPT FOR PRESTO)                     
*                                                                               
         DC    X'03',AL1(RECNJOB@,ACTNADD),X'F303000380',C'    '                
         DC    AL1(CAT5Q,0,0,0)                                                 
*                                                                               
* ARTICLES/ITEMS                                                                
*                                                                               
         DC    X'03',AL1(RECNARTS,ACTNADD),X'AB54005489',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNARTS,ACTNCHA),X'AB54005489',C'    '                
         DC    AL1(CAT1Q,0,0,0)                                                 
         DC    X'03',AL1(RECNARTS,ACTNDIS),X'AB54005489',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
***      DC    X'03',AL1(RECNARTS,ACTNDEL),X'AB54005489',C'    '                
***      DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNARTS,ACTNSEL),X'AB54005488',C'    '                
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
***      DC    X'03',AL1(RECNARTS,ACTNRES),X'AB54005489',C'    '                
***      DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(RECNARTS,ACTNLIST),X'A355005580',C'    '               
         DC    AL1(CAT1Q+CAT2Q+CAT3Q+CAT4Q+CAT5Q+CAT6Q,0,0,0)                   
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*        TABLE OF RECORD/ACTIONS THAT BYPASS SECURITY CHECK                     
*                                                                               
RECSEC   DS    0CL2                                                             
         DC    AL1(RECNLNK,ACTNMNT)                                             
         DC    AL1(RECNLNK,ACTNLIST)                                            
         DC    AL1(RECNLNK,ACTNREP)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACOPTTAB                                                       
         EJECT                                                                  
       ++INCLUDE ACPROWORKD                                                     
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACVATICAND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDSCANBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*ACRECEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
*ACPROCPTRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROCPTRD                                                     
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FAGETTXTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
GOXBLKD  DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
GOBBLKD  DSECT                                                                  
       ++INCLUDE ACGOBBLOCK                                                     
GOPANBKD DSECT                                                                  
       ++INCLUDE ACPANBLOCK                                                     
         PRINT ON                                                               
EQUTBD   DSECT                                                                  
EQUSEQ   DS    XL1           SEQUENCE NUMBER                                    
EQUCODE  DS    CL10          ACCOUNT/SYSTEM NAME EG HYPERION=                   
EQULNQ   EQU   *-EQUTBD                                                         
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079ACPROGEN  07/10/20'                                      
         END                                                                    
