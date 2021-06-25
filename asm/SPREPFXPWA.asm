*          DATA SET SPREPFXPWA AT LEVEL 002 AS OF 06/18/97                      
*PHASE SPFX026                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'SPFX02 - COUNT # OF PW RECORDS W/. CABLE STATIONS'              
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   AREC,ADPWREC                                                     
         SR    RF,RF                                                            
         DROP  RF                                                               
                                                                                
         XC    TMPSTA,TMPSTA                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'     PW RECORDS ONLY                              
         GOTO1 HIGH                                                             
         B     FX22                                                             
*                                                                               
FX20     DS    0H                                                               
         GOTO1 SEQ                                                              
                                                                                
FX22     DS    0H                                                               
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         CLC   PWKTYP,=X'0D7A'                                                  
         BNE   FX100                                                            
         OC    PWKSTA,PWKSTA                                                    
         BZ    FX20                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         TM    PWKSTA,X'F0'                                                     
         BNO   FX20                                                             
*                                                                               
         MVC   TMPSTA,PWKSTA                                                    
         NC    TMPSTA,=X'FFFF80'                                                
         CLC   CURRSTA,TMPSTA                                                   
         BE    FX26X                                                            
         MVI   P1,C' '                                                          
         GOTO1 REPORT                                                           
         MVC   CURRSTA,TMPSTA                                                   
         LA    R0,1                                                             
         A     R0,CNTCREAT                                                      
         ST    R0,CNTCREAT                                                      
FX26X    EQU   *                                                                
         EJECT                                                                  
         DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTCABST                                                      
         ST    R0,CNTCABST                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         GOTO1 HEXOUT,DMCB,PWFKEY,WORK,18,=C'TOG',0                             
*                                                                               
         DS    0H                                                               
         LA    RF,WORK                                                          
         LA    R2,P1                                                            
         USING P1DSECT,R2                                                       
         MVC   P1KTYPE,0(RF)                                                    
         LA    RF,2*L'PWKTYP(RF)                                                
                                                                                
         MVC   P1KAGYMD,0(RF)                                                   
         LA    RF,2*L'PWKAGMD(RF)                                               
                                                                                
         MVC   P1KCLT,0(RF)                                                     
         LA    RF,2*L'PWKCLT(RF)                                                
                                                                                
         MVC   P1KPRD,0(RF)                                                     
         LA    RF,2*L'PWKPRD(RF)                                                
                                                                                
         MVC   P1KEST,0(RF)                                                     
         LA    RF,2*L'PWKEST(RF)                                                
                                                                                
         MVC   P1KMKSTA,0(RF)                                                   
         LA    RF,(2*(L'PWKMKT+L'PWKSTA))(RF)                                   
                                                                                
         MVC   P1KSPARE,0(RF)                                                   
         LA    RF,(2*1)(RF)                                                     
                                                                                
         MVC   P1CNTL,0(RF)                                                     
         LA    RF,(2*L'PWCNTL)(RF)                                              
                                                                                
         MVC   P1DSKADD,0(RF)                                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
*                                                                               
         B     FX20                                                             
         EJECT                                                                  
FX100    DS    0H                  REPORTING                                    
         MVC   P1(37),=C'NUMBER OF PW RECORDS W/ CABLE STATION'                 
         L     R1,CNTCABST                                                      
         EDIT  (R1),(6,P+40),ZERO=NOBLANK,COMMAS=YES                            
         MVI   P2,C' '                                                          
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(30),=C'NUMBER OF PW RECORDS TO CREATE'                        
         L     R1,CNTCREAT                                                      
         EDIT  (R1),(6,P1+40),ZERO=NOBLANK,COMMAS=YES                           
         MVI   P2,C' '                                                          
         GOTO1 REPORT                                                           
                                                                                
*                                                                               
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
FXELTAB  DC    AL1(PWDOLCDQ,PWCURCDQ,PWBAKDOL,0)                                
*                                                                               
AGYCOUNT DS    0XL(2+6+4+4+4)                                                   
*  BYTES 1-2   : ALPHA AGENCY CODE                                              
*  BYTES 3-8   : AGENCY ID                                                      
*  BYTES 9-12  : # OF PW MKT-LEVEL RECDS                                        
*  BYTES 13-16 : # OF PW MKTS W/ TAX                                            
*  BYTES 17-20 : # OF PW STATION-LEVEL RECDS                                    
SJCOUNT  DC    CL2'SJ',CL6'SJR   ',AL4(0),AL4(0),AL4(0)                         
TCCOUNT  DC    CL2'TC',CL6'TRC   ',AL4(0),AL4(0),AL4(0)                         
WICOUNT  DC    CL2'WI',CL6'WILA  ',AL4(0),AL4(0),AL4(0)                         
WJCOUNT  DC    CL2'WJ',CL6'WITEST',AL4(0),AL4(0),AL4(0)                         
OTCOUNT  DC    XL2'0000',CL6'OTHERS',AL4(0),AL4(0),AL4(0)                       
                                                                                
OLDSIZE  DC    F'0'                OLD SIZE OF ALL PW RECDS                     
NEWSIZE  DC    F'0'                NEW SIZE OF ALL PW RECDS                     
                                                                                
CNTCABST DC    F'0'                COUNT # OF PW RECDS W/ CABLE STATION         
CNTCREAT DC    F'0'                COUNT # OF PW RECDS TO CREATE                
                                                                                
ELCODE   DS    X                                                                
TMPSTA   DS    XL(L'BSTA)                                                       
CURRSTA  DS    XL(L'BSTA)                                                       
                                                                                
         EJECT                                                                  
***********************************************************************         
*============================= PRINT LINE ============================*         
                                                                                
P1DSECT  DSECT                                                                  
P1KTYPE  DS    XL(2*L'PWKTYP)                                                   
         DS    CL1                                                              
P1KAGYMD DS    XL(2*L'PWKAGMD)                                                  
         DS    CL1                                                              
P1KCLT   DS    XL(2*L'PWKCLT)                                                   
         DS    CL1                                                              
P1KPRD   DS    XL(2*L'PWKPRD)                                                   
         DS    CL1                                                              
P1KEST   DS    XL(2*L'PWKEST)                                                   
         DS    CL1                                                              
P1KMKSTA DS    XL(2*(L'PWKMKT+L'PWKSTA))                                        
         DS    CL1                                                              
P1KSPARE DS    XL(2*1)                                                          
         DS    CL2                                                              
P1CNTL   DS    XL(2*L'PWCNTL)                                                   
         DS    CL1                                                              
P1DSKADD DS    XL(2*4)                                                          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
OWDOLEL  DSECT                     OLD WEEKLY (LOCKED) DOLLAR ELEMENT           
OWDOLCD  DS    X'06'               ELEMENT CODE (X'06')                         
OWDOLCDQ EQU   X'06'                                                            
OWDOLLEN DS    AL1(OWDOLLNQ)       ELEMENT LENGTH                               
OWDOLWK  DS    XL2                 WEEK DATE                                    
OWDOLSPT DS    XL4                 SPOTS                                        
OWDOLWG  DS    XL4                 WIM GROSS (IN PENNIES)                       
OWDOLWN  DS    XL4                 WIM NET    "                                 
OWDOLCG  DS    XL4                 CLT GROSS  "                                 
OWDOLCN  DS    XL4                 CLT NET    "                                 
OWDOLTAX DS    XL4                 TAX        "   (WIMTAX)                      
* FOLLOWING FIELD NON-ZERO IN FIRST WEEK OF EACH MONTH ONLY IN BOTH             
*  THE MKT-LEVEL AND STATION-LEVEL RECORDS.  SINCE THERE IS NO WAY TO           
*  DISTRIBUTE THE BILL OVRD AMOUNT ON A STATION-BY-STATION BASIS, EACH          
*  STATION WILL HAVE THE SAME BILL OVRD $'S AS THE MKT-LEVEL RECORD.            
OWDOLBIL DS    XL4                 BILL OVRD DOLLARS (X'80000000'=$0)           
OWDOLBLD DS    XL2                 ADJUSTMENT BILLING DATE                      
OWDOLLNQ EQU   *-OWDOLEL                                                        
                                                                                
                                                                                
OWCUREL  DSECT                     OLD WEEKLY CURRENT DOLLAR ELEMENT            
OWCURCD  DS    X'07'               ELEMENT CODE (X'07')                         
OWCURCDQ EQU   X'07'                                                            
OWCURLEN DS    AL1(OWCURLNQ)       ELEMENT LENGTH                               
OWCURWK  DS    XL2                 WEEK DATE                                    
OWCURSPT DS    XL4                 SPOTS                                        
OWCURWG  DS    XL4                 WIM GROSS (IN PENNIES)                       
OWCURWN  DS    XL4                 WIM NET    "                                 
OWCURCG  DS    XL4                 CLT GROSS  "                                 
OWCURCN  DS    XL4                 CLT NET    "                                 
OWCURTAX DS    XL4                 TAX        "   (WIMTAX)                      
* FOLLOWING FIELD NON-ZERO IN FIRST WEEK OF MONTH ONLY.                         
OWCURBIL DS    XL4                 BILL OVRD DOLLARS (X'80000000'=$0)           
OWCURBLD DS    XL2                 ADJUSTMENT BILLING DATE                      
OWCURLNQ EQU   *-OWCUREL                                                        
***********************************************************************         
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPFXPWA06/18/97'                                      
         END                                                                    
