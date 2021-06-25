*          DATA SET TAGEN21    AT LEVEL 022 AS OF 05/01/02                      
*PHASE T70221A,*                                                                
         TITLE 'T70221 - PASSWORD MAINTENANCE'                                  
T70221   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70221                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
*                                                                               
         TM    PRGSTAT,STAYPCHG    IF FORCING PASSWORD/CHANGE                   
         BZ    MN5                                                              
         OI    CONRECH+1,X'20'     SET RECORD AND ACTION FLDS PROT              
         OI    CONRECH+6,X'80'                                                  
         OI    CONACTH+1,X'20'                                                  
         OI    CONACTH+6,X'80'                                                  
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
MN5      GOTO1 INITIAL,DMCB,0      INITIALIZE OVERLAY                           
*                                                                               
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
*                                                                               
         TM    PRGSTAT,STAYPCHG    IF FORCING PASSWORD CHANGE                   
         BZ    *+8                                                              
         BAS   RE,DISPTXT          DISPLAY INFORMATIONAL TEXT                   
*                                                                               
         MVC   TGUSER,TWAORIG      SET TO READ STAFF RECORD FOR UPDATE          
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'B0',TGCTSTAF)                             
         BNE   ERRRNF                                                           
*                                                                               
         GOTO1 ACTVOUT,DMCB,(X'80',SPWLCHGH)  DISPLAY LAST CHANGED              
*                                                                               
         L     R4,AIO              POINT TO STAFF ELEMENT                       
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASTD,R4                                                         
*                                                                               
         LA    R2,SPWOLDH          VALIDATE OLD PASSWORD                        
         GOTO1 ANY                                                              
         TM    SECMASKS,X'80'      IF ORIGINALLY WAS PROGRAMMER                 
         BZ    MN20                                                             
         CLI   5(R2),1             AND ONLY 1 CHARACTER IN PASSWORD             
         BNE   MN20                                                             
         GOTO1 STAFVAL,DMCB,8(R2),0  AND IF INPUT IS VALID STAFF TYPE           
         BNE   MN20                                                             
         MVC   TGCTSTTY,TGSTEQU    SET NEW CONNECT STAFF TYPE                   
         MVC   TGCTSTLV,TGSTLVL    AND NEW CONNECT STAFF LEVEL                  
         MVC   CONHED2+25(12),=C'YOU''RE NOW A'                                 
         MVC   CONHED2+38(16),TGSTNAME                                          
         B     MNX                 DON'T DO ANYTHING ELSE                       
*                                                                               
MN20     CLC   TASTPWD,WORK                                                     
         BNE   ERRINV                                                           
*                                                                               
         LA    R2,SPWNEWH          VALIDATE NEW PASSWORD                        
         GOTO1 ANY                                                              
         MVC   TASTPWD,WORK        SAVE NEW PASSWORD                            
*                                                                               
         LA    R2,SPWNEW2H         MAKE THEM TYPE IT IN TWICE                   
         GOTO1 ANY                                                              
         CLC   TASTPWD,WORK                                                     
         BNE   ERRSAME                                                          
*                                                                               
         GOTO1 PWRDVAL,DMCB,TASTPWD CHECKS PASSWRD NOT RECENTLY USED            
         BNE   ERRDUP              AND UPDATES TAPWD ELEMENTS                   
*                                                                               
         GOTO1 ACTVIN,DMCB,(X'80',0)   UPDATE LAST CHANGED ELEMENT              
         GOTO1 ACTVOUT,DMCB,(X'80',SPWLCHGH)  DISPLAY NEW LAST CHANGED          
*                                                                               
         GOTO1 PUTREC              WRITE BACK STAFF RECORD                      
*                                                                               
         NI    PRGSTAT,X'FF'-STAYPCHG  TURN OFF FORCE FOR REC/ACT               
         NI    CONRECH+1,X'DF'     SET RECORD AND ACTION FLDS UNPROT            
         OI    CONRECH+6,X'80'                                                  
         NI    CONACTH+1,X'DF'                                                  
         OI    CONACTH+6,X'80'                                                  
         OI    SPWTXT1H+1,X'0C'    SET TEXT FIELDS ZERO-INTENSITY               
         OI    SPWTXT1H+6,X'80'                                                 
         OI    SPWTXT2H+1,X'0C'                                                 
         OI    SPWTXT2H+6,X'80'                                                 
MNX      B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY TEXT MESSAGE ABOUT                            
*              CHANGING PASSWORDS                                               
         SPACE 1                                                                
DISPTXT  NTR1                                                                   
         LA    R2,SPWTXT1H         R2=A(TEXT FIELD)                             
         MVC   8(L'LTTXT1,R2),LTTXT1                                            
         NI    1(R2),X'FB'         SET TO HIGH INTENSITY                        
         OI    6(R2),X'80'         SET TRANSMITTED                              
*                                                                               
         LA    R2,SPWTXT2H         R2=A(TEXT FIELD NUMBER 2)                    
         MVC   8(L'LTTXT2,R2),LTTXT2                                            
         NI    1(R2),X'FB'         SET TO HIGH INTENSITY                        
         OI    6(R2),X'80'         SET TRANSMITTED                              
         B     XIT                                                              
         EJECT                                                                  
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         B     ERREXIT                                                          
*                                                                               
ERRSAME  MVI   ERROR,ERNOTSAM      NEW PASSWORD FIELDS NOT THE SAME             
         XC    SPWNEW2,SPWNEW2     CLEAR FIELD FOR THEM                         
         OI    SPWNEW2H+6,X'80'                                                 
         B     ERREXIT                                                          
*                                                                               
ERRDUP   LA    R2,SPWNEWH          R2=A(FIRST NEW PASSWORD FIELD)               
         XC    8(8,R2),8(R2)       CLEAR FIELDS FOR THEM                        
         OI    6(R2),X'80'                                                      
         XC    SPWNEW2,SPWNEW2                                                  
         OI    SPWNEW2H+6,X'80'                                                 
         MVI   ERROR,ERDUPASS      NEW PASSWORD RECENTLY USED                   
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
LTTXT1   DC    CL50'Your password has expired.  Passwords must'                 
LTTXT2   DC    CL50'be changed at least every ninety days.'                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR21D                                                       
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022TAGEN21   05/01/02'                                      
         END                                                                    
