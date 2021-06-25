*          DATA SET ACPRO26    AT LEVEL 016 AS OF 09/12/02                      
*PHASE T60B26A,*                                                                
         TITLE 'T60B26 - PRODUCTION USER RECORD REPORT'                         
T60B26   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B26,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
VREC     NTR1                                                                   
         MVC   CLICODE,SPACES                                                   
         MVC   PRODCODE,SPACES                                                  
         MVC   JOBNUM,SPACES                                                    
         MVI   OPTION,C'Y'                                                      
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR             NEED KEY LENGTHS                             
*                                                                               
         LA    R2,PROCLIH          CLIENT OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         GOTO1 VALCLI                                                           
*                                                                               
         LA    R2,PROPROH          PRODUCT OPTIONAL                             
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         GOTO1 VALPROD                                                          
*                                                                               
         LA    R2,PROJOBH          JOB OPTIONAL                                 
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         LA    R3,12                                                            
         GOTO1 VALJOB                                                           
*                                                                               
VREC10   B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
*                                                                               
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         LA    R4,KEY                                                           
         USING ACUFKEY,R4                                                       
*                                                                               
DR10     XC    ACUFKEY,ACUFKEY                                                  
         MVC   ACUFRTYP(2),=X'2C10'                                             
         MVC   ACUFCUL,CUL                                                      
         MVC   SAVEH4,=CL20'AGENCY RECORDS'                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR14                                                             
*                                                                               
DR12     GOTO1 SEQ                                                              
*                                                                               
DR14     CLC   ACUFKEY(ACUFMG-ACUFKEY),KEYSAVE                                  
         BNE   DR20                                                             
         BAS   RE,CHKMED                                                        
         BNE   DR12                                                             
         BAS   RE,PROCRAW                                                       
         B     DR12                                                             
*                                                                               
DR20     XC    ACUFKEY,ACUFKEY                                                  
         MVC   ACUFRTYP(2),=X'2C10'                                             
         MVC   ACUFCUL,CUL                                                      
         MVC   ACUFOG,EFFOFG                                                    
         CLI   ACUFOG,0                                                         
         BNE   *+8                                                              
         MVI   ACUFOG,C'A'                                                      
         MVC   SAVEH4,=CL20'OFFICE GROUP RECORDS'                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR24                                                             
*                                                                               
DR22     GOTO1 SEQ                                                              
*                                                                               
DR24     CLC   ACUFKEY(ACUFOG-ACUFKEY),KEYSAVE                                  
         BNE   DR30                                                             
         CLI   EFFOFG,0                                                         
         BE    DR26                                                             
         CLC   EFFOFG,ACUFOG                                                    
         BNE   DR30                                                             
*                                                                               
DR26     BAS   RE,CHKMED                                                        
         BNE   DR22                                                             
         BAS   RE,PROCRAW                                                       
         B     DR22                                                             
*                                                                               
DR30     XC    ACUFKEY,ACUFKEY                                                  
         MVC   ACUFRTYP(2),=X'2C10'                                             
         MVC   ACUFCUL,CUL                                                      
         MVC   ACUFOFC,EFFOFFC                                                  
         CLC   ACUFOFC,SPACES                                                   
         BH    *+8                                                              
         MVI   ACUFOFC,C'A'                                                     
         MVC   SAVEH4,=CL20'OFFICE RECORDS'                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR34                                                             
*                                                                               
DR32     GOTO1 SEQ                                                              
*                                                                               
DR34     CLC   ACUFKEY(ACUFOFC-ACUFKEY),KEYSAVE                                 
         BNE   DR40                                                             
         CLC   EFFOFFC,SPACES                                                   
         BNH   DR36                                                             
         CLC   EFFOFFC,ACUFOFC                                                  
         BNE   DR40                                                             
*                                                                               
DR36     BAS   RE,CHKMED                                                        
         BNE   DR32                                                             
         BAS   RE,PROCRAW                                                       
         B     DR32                                                             
*                                                                               
DR40     XC    ACUFKEY,ACUFKEY                                                  
         MVC   ACUFRTYP(2),=X'2C10'                                             
         MVC   ACUFCUL,CUL                                                      
         MVC   SAVEH4,=CL20'CLIENT RECORDS'                                     
         MVC   ACUFCLI,CLICODE                                                  
         CLI   PROPROH+5,0                                                      
         BE    *+10                                                             
         MVC   ACUFPRO,PRODCODE                                                 
         CLI   PROJOBH+5,0                                                      
         BE    *+10                                                             
         MVC   ACUFJOB,JOBNUM                                                   
         CLI   CLICODE,C' '                                                     
         BNE   *+8                                                              
         MVI   ACUFCLI,C'A'                                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DR44                                                             
*                                                                               
DR42     GOTO1 SEQ                                                              
*                                                                               
DR44     CLC   ACUFKEY(ACUFCLI-ACUFKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLI   PROCLIH+5,0                                                      
         BE    DR46                                                             
         CLC   ACUFKEY(ACUFPRO-ACUFKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLI   PROPROH+5,0                                                      
         BE    DR46                                                             
         CLC   ACUFKEY(ACUFMG-ACUFKEY),KEYSAVE                                  
         BNE   XIT                                                              
         CLI   PROJOBH+5,0                                                      
         BE    DR46                                                             
         CLC   ACUFKEY(ACUFJOB+L'ACUFJOB-ACUFKEY),KEYSAVE                       
         BNE   XIT                                                              
*                                                                               
DR46     BAS   RE,CHKMED                                                        
         BNE   DR42                                                             
         BAS   RE,PROCRAW                                                       
         B     DR42                                                             
         EJECT                                                                  
*              ROUTINE TO CHECK MEDIA AND MEDIA GROUP AGAINST RECORD            
*                                                                               
CHKMED   CLI   ACUFMED,0           ANY MEDIA IN RECORD?                         
         BE    CHKMED2             NO, NOTHING TO TEST                          
         CLI   MEDIA,0             ANY MEDIA SPECIFIED?                         
         BE    CHKMED2             NO, NOTHING TO COMPARE                       
         CLC   ACUFMED,MEDIA       YES, DO WE MATCH?                            
         BNER  RE                  NO, SKIP IT                                  
*                                                                               
CHKMED2  CLI   ACUFMG,0            ANY MEDIA GROUP IN RECORD?                   
         BER   RE                  NO, NOTHING TO TEST                          
         CLI   MGROUP,0            ANY MEDIA GROUP SPECIFIED?                   
         BER   RE                  NO, NOTHING TO COMPARE                       
         CLC   ACUFMG,MGROUP       YES, DO WE MATCH                             
         BNER  RE                  NO, SKIP IT                                  
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS A RAW OPTION RECORD                           
         SPACE 3                                                                
PROCRAW  NTR1                                                                   
         USING ACUFKEY,R4                                                       
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
         MVC   PRTOG,ACUFOG                                                     
         MVC   PRTOFF,ACUFOFC                                                   
         MVC   PRTCLI,ACUFCLI                                                   
         MVC   PRTPRO,ACUFPRO                                                   
         MVC   PRTJOB,ACUFJOB                                                   
         MVC   PRTMG,ACUFMG                                                     
         MVC   PRTMED,ACUFMED                                                   
         MVI   ANYOPT,C'N'                                                      
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETELIO                                                       
         B     PRAW4                                                            
*                                                                               
PRAW2    BAS   RE,NEXTEL                                                        
*                                                                               
PRAW4    BE    PRAW6                                                            
         CLI   ANYOPT,C'Y'                                                      
         BNE   XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         USING ACUFD,R6                                                         
PRAW6    MVI   ANYOPT,C'Y'                                                      
         MVC   PRTCD,ACUFCODE                                                   
         MVC   PRTFLD,ACUFDESC                                                  
         MVC   PRTEDT,ACUFEDIT                                                  
         EDIT  (1,ACUFMXLN),(2,PRTMAX)                                          
         MVI   PRTREQ,C'N'                                                      
         TM    ACUFSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   PRTREQ,C'Y'                                                      
         MVI   PRTNED,C'N'                                                      
         TM    ACUFSTAT,X'40'                                                   
         BNO   *+8                                                              
         MVI   PRTNED,C'Y'                                                      
         MVI   PRTEST,C'N'                                                      
         TM    ACUFSTAT,X'20'                                                   
         BNO   *+8                                                              
         MVI   PRTEST,C'Y'                                                      
         MVI   PRTBIL,C'N'                                                      
         TM    ACUFSTAT,X'10'                                                   
         BNO   *+8                                                              
         MVI   PRTBIL,C'Y'                                                      
         MVI   PRTREC,C'N'                                                      
         TM    ACUFSTAT,X'08'                                                   
         BNO   *+8                                                              
         MVI   PRTREC,C'Y'                                                      
         MVI   PRTAUT,C'N'                                                      
         TM    ACUFSTAT,X'04'                                                   
         BNO   *+8                                                              
         MVI   PRTAUT,C'Y'                                                      
         GOTO1 DATCON,DMCB,(1,ACUFCUT),(8,PRTCUT)                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRAW2                                                            
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         LA    R3,H1+48            DEAL WITH HEADING                            
         MVC   0(32,R3),=CL32'USER RECORD REPORT'                               
         CLI   PROTITH+5,0                                                      
         BE    HOOKRAW2                                                         
         LA    R2,PROTITH                                                       
         GOTO1 ANY                                                              
         MVC   0(32,R3),WORK                                                    
*                                                                               
HOOKRAW2 GOTO1 CENTER,DMCB,(R3),32                                              
         GOTO1 UNDERLIN,DMCB,(32,(R3)),(X'BF',132(R3))                          
*                                                                               
         MVC   H4+1(20),SAVEH4                                                  
         MVC   H8+1(3),=C'OFF'                                                  
         MVC   H9+1(3),=C'GRP'                                                  
         MVC   H8+7(3),=C'OFF'                                                  
         MVC   H8+13(6),=C'CLIENT'                                              
         MVC   H8+22(6),=C'PRODCT'                                              
         MVC   H8+31(6),=C'JOB   '                                              
         MVC   H8+40(3),=C'MED'                                                 
         MVC   H9+40(3),=C'GRP'                                                 
         MVC   H8+46(3),=C'MED'                                                 
         MVC   H8+52(2),=C'CD'                                                  
         MVC   H8+57(12),=C'FIELD HEADER'                                       
         MVC   H8+72(3),=C'EDT'                                                 
         MVC   H8+78(3),=C'MAX'                                                 
         MVC   H9+78(3),=C'LEN'                                                 
         MVC   H8+84(3),=C'REQ'                                                 
         MVC   H9+84(3),=C'FLD'                                                 
         MVC   H8+90(3),=C'REQ'                                                 
         MVC   H9+90(3),=C'BIL'                                                 
         MVC   H8+96(3),=C'SHW'                                                 
         MVC   H9+96(3),=C'EST'                                                 
         MVC   H8+102(3),=C'SHW'                                                
         MVC   H9+102(3),=C'BIL'                                                
         MVC   H8+108(3),=C'SHW'                                                
         MVC   H9+108(3),=C'REC'                                                
         MVC   H8+114(3),=C'SHW'                                                
         MVC   H9+114(3),=C'AUT'                                                
         MVC   H8+121(6),=C'CUTOFF'                                             
         MVC   H9+121(6),=C' DATE '                                             
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+5,C'C'                                                   
         MVI   BOXCOLS+11,C'C'                                                  
         MVI   BOXCOLS+20,C'C'                                                  
         MVI   BOXCOLS+29,C'C'                                                  
         MVI   BOXCOLS+38,C'C'                                                  
         MVI   BOXCOLS+44,C'C'                                                  
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+70,C'C'                                                  
         MVI   BOXCOLS+76,C'C'                                                  
         MVI   BOXCOLS+82,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXCOLS+94,C'C'                                                  
         MVI   BOXCOLS+100,C'C'                                                 
         MVI   BOXCOLS+106,C'C'                                                 
         MVI   BOXCOLS+112,C'C'                                                 
         MVI   BOXCOLS+118,C'C'                                                 
         MVI   BOXCOLS+129,C'R'                                                 
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
ANYOPT   DS    CL1                                                              
SAVEH4   DS    CL20                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRINTD   DSECT                                                                  
         DS    CL2                                                              
PRTOG    DS    CL1                                                              
         DS    CL5                                                              
PRTOFF   DS    CL2                                                              
         DS    CL3                                                              
PRTCLI   DS    CL6                                                              
         DS    CL3                                                              
PRTPRO   DS    CL6                                                              
         DS    CL3                                                              
PRTJOB   DS    CL6                                                              
         DS    CL4                                                              
PRTMG    DS    CL1                                                              
         DS    CL5                                                              
PRTMED   DS    CL1                                                              
         DS    CL4                                                              
PRTCD    DS    CL2                                                              
         DS    CL3                                                              
PRTFLD   DS    CL12                                                             
         DS    CL4                                                              
PRTEDT   DS    CL1                                                              
         DS    CL4                                                              
PRTMAX   DS    CL2                                                              
         DS    CL5                                                              
PRTREQ   DS    CL1                                                              
         DS    CL5                                                              
PRTNED   DS    CL1                                                              
         DS    CL5                                                              
PRTEST   DS    CL1                                                              
         DS    CL5                                                              
PRTBIL   DS    CL1                                                              
         DS    CL5                                                              
PRTREC   DS    CL1                                                              
         DS    CL5                                                              
PRTAUT   DS    CL1                                                              
         DS    CL4                                                              
PRTCUT   DS    CL9                                                              
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
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROD6D                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACPRO26   09/12/02'                                      
         END                                                                    
