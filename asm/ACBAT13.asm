*          DATA SET ACBAT13    AT LEVEL 054 AS OF 05/01/02                      
*PHASE T61B13A                                                                  
         TITLE 'ONE-SIDED POSTING - T61B13'                                     
T61B13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT13,CLEAR=YES                                   
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         EJECT                                                                  
*              READ & SAVE ACCOUNTS                                             
         SPACE 1                                                                
         MVC   KEY,SPACES          ACCOUNT                                      
         MVC   KEY(1),COMPANY                                                   
         SR    R3,R3                                                            
         SR    R6,R6                                                            
         LA    R2,ONEACCH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,17                                                        
         IC    R3,ONEACCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),ONEACC                                                  
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         MVI   ERRNUM,2                                                         
         LA    R2,ONEDRCRH                                                      
         CLI   ONEDRCR,C'D'        NO DEBITS TO PAYEE ACCOUNTS                  
         BNE   ONE0                                                             
         CLC   =C'SF',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SP',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SQ',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SS',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'ST',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SU',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SV',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SW',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SX',ONEACC                                                    
         BE    ERROR                                                            
         CLC   =C'SY',ONEACC                                                    
         BE    ERROR                                                            
         B     ONE1                                                             
*                                                                               
ONE0     CLI   ONEDRCR,C'C'        OR CREDITS TO JOBS                           
         BNE   ONE1                                                             
         CLC   =C'SJ',ONEACC                                                    
         BE    ERROR                                                            
*                                                                               
ONE1     DS    0H                                                               
         MVC   NUM,ACCTNUM         SAVE ACCOUNT NAME AND NUMBER                 
         MVC   NAME,ACCTNAME                                                    
         TM    ONEACCH+4,X'20'     IF NOT MARKED VALIDATED, DO SO               
         BO    ONE2                                                             
         OI    ONEACCH+4,X'20'                                                  
         MVC   ONEACCN,ACCTNAME                                                 
         FOUT  ONEACCNH            PRINT BACK THE NAME                          
*                                                                               
ONE2     LA    R2,ONECACH          MUST HAVE CONTRA ACCOUNT                     
         BAS   RE,ANY                                                           
*       - - - - - - - - - - - - - - - -  VALIDATE THE WORKCODE                  
         LA    R2,ONEWRKH                                                       
         OC    8(2,R2),SPACES                                                   
*                                                                               
         CLC   ACCTNUM+1(2),=C'SJ'   SJ ACCOUNT?                                
         BNE   ONE2A                 YES                                        
*                                                                               
         GOTO1 AGETWRK,ONEWRK                                                   
         BNE   ERROR                                                            
         B     ONE2C                                                            
*                                                                               
ONE2A    GOTO1 AVALOFF,DMCB,(X'80',8(R2))                                       
         BNE   ERROR                                                            
*                                                                               
ONE2C    DS    0H                                                               
         LA    R2,ONECACH                                                       
*       - - - - - - - - - - - - - - - - - -                                     
         CLC   NUM+1(2),=C'SP'     CONTRA MUST BE '***' FOR SP                  
         BE    ONE3                                                             
         CLC   NUM+1(2),=C'SQ'     SQ                                           
         BE    ONE3                                                             
         CLC   NUM+1(2),=C'SR'     SR                                           
         BE    ONE3                                                             
         CLC   NUM+1(2),=C'SS'     SS                                           
         BE    ONE3                                                             
         CLC   NUM+1(2),=C'ST'     ST                                           
         BE    ONE3                                                             
         CLC   NUM+1(2),=C'SU'     SU                                           
         BE    ONE3                                                             
         CLC   NUM+1(6),=C'SCC003'                                              
         BE    ONE3                                                             
         CLC   NUM+1(6),=C'SCC006'                                              
         BNE   ONE4                                                             
*                                                                               
ONE3     MVI   ERRNUM,2                                                         
         CLC   ONECAC(3),=C'***'                                                
         BNE   ERROR                                                            
         MVC   ONECAC(3),SPACES    CONVERT ASTERISKS TO SPACES                  
*                                                                               
ONE4     MVI   ERRNUM,2                                                         
         CLI   ONECAC,C'*'         MUST BE 29 OR 29                             
         BNE   ONE4A                                                            
         CLI   ONECAC+1,C'*'                                                    
         BE    ERROR                                                            
         CLC   ONEACC(2),=C'29'                                                 
         BE    ONE4A                                                            
         CLC   ONEACC(2),=C'2P'                                                 
         BNE   ERROR                                                            
*                                                                               
ONE4A    MVC   KEY+1(14),SPACES                                                 
         SR    R3,R3                                                            
         IC    R3,ONECACH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),ONECAC                                                  
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE     VALID CONTRA ACCOUNT?                        
         BE    ONE6                YES                                          
*                                                                               
         MVI   ERRNUM,INVACC       INVALID OK FOR CERTAIN ACCOUNTS              
         CLC   ONEACC(2),=C'SZ'    INVALID OK FOR THIS ONE                      
         BE    ONE5                                                             
         CLC   ONECAC(2),=C'13'    IS CONTRA A 13 ACCOUNT?                      
         BNE   ONE4B               NO                                           
         CLC   ONEACC(2),=C'1C'    YES, WAS ACCOUNT 1C?                         
         BNE   ERROR               NO, ERROR                                    
         B     ONE5                YES, OK AS INVALID                           
*                                                                               
ONE4B    MVC   KEY,SPACES          READ FOR WHOLE KEY                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),ONECAC                                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    ONE6                                                             
*                                                                               
         MVI   ERRNUM,INVACC                                                    
         CLC   ONECAC(3),SPACES    IF CONTRA '***', INVALID OK                  
         BE    ONE5                                                             
         CLI   ONECAC,C'*'         IF CONTRA '*', INVALID OK                    
         BNE   ERROR               INVALID                                      
*                                                                               
ONE5     XC    CNAME,CNAME         CLEAR NAME AND NUMBER                        
         MVI   CNAME,C' '                                                       
         MVC   CNUM,KEYSAVE                                                     
         B     ONE7                                                             
*                                                                               
ONE6     DS    0H                                                               
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'      HAS TO HAVE BALANCE ELEMENT                  
         BZ    ERROR                                                            
         MVC   CNUM,ACCTNUM                                                     
         MVC   CNAME,ACCTNAME                                                   
         TM    ONECACH+4,X'20'                                                  
         BO    ONE7                                                             
         OI    ONECACH+4,X'20'                                                  
         MVC   ONECACN,ACCTNAME                                                 
         FOUT  ONECACNH                                                         
         B     ONE7                                                             
         EJECT                                                                  
*              BUILD 64 ELEMENT                                                 
         SPACE 2                                                                
ONE7     LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         LA    R2,ONEDOCH                                                       
         BAS   RE,ANY                                                           
         MVC   DLDSREF,SPACES                                                   
         IC    R3,ONEDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),ONEDOC                                                
         SPACE 1                                                                
         LA    R2,ONEDATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   ONEDATH+5,0                                                      
         BNE   *+12                                                             
         BAS   RE,GETODAY                                                       
         B     ONE8                                                             
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         SPACE 1                                                                
ONE8     GOTO1 DATCON,DMCB,(0,WORK),(1,DLDSDATE)                                
         GOTO1 DATECHK,DMCB,DLDSDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORIZE                                    
         LA    R2,ONENARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
         SPACE 1                                                                
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               ELEMENT - NARRATIVE                          
         AH    R5,=H'2'                                                         
         AR    R5,R6               R6 = L'NARRATIVE                             
         STH   R5,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         SH    R5,=H'2'                                                         
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         EJECT                                                                  
*              BUILD A 69 OR 6A ELEMENT                                         
         SPACE 2                                                                
         AR    R8,R5                                                            
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'69'        ASSUME DEBIT FOR NOW                         
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,NUM                                                     
         MVC   DLPSDBNM,NAME                                                    
         MVC   DLPSCRAC,CNUM                                                    
         MVC   DLPSCRNM,CNAME                                                   
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SPACES                                                  
         MVI   ERRNUM,19                                                        
         LA    R2,ONEWRKH                                                       
         CLC   ONEWRK(2),=C'99'                                                 
         BE    ERROR                                                            
         CLI   ONEWRKH+5,0                                                      
         BE    *+10                                                             
         MVC   DLPSANAL,ONEWRK                                                  
*&&UK                                                                           
         CLI   TWAACCS,C'*'        FOR OFFICE LOGON CHECK                       
         BNE   ONE9                THAT THE OFIICE IS CORRECT                   
         CLC   =C'SJ',ONEACC       BUT DONT BOTHER FOR JOBS                     
         BE    ONE9                                                             
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLC   ONEWRK(1),TWAACCS+1                                              
         BE    ONE9                                                             
         MVI   ERRNUM,SECLOCK                                                   
         B     ERROR                                                            
*&&                                                                             
ONE9     LA    R2,ONEAMTH                                                       
         MVI   ERRNUM,25                                                        
         IC    R3,ONEAMTH+5                                                     
         GOTO1 AMTVAL,DMCB,ONEAMT,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)                                                         
         ZAP   DLPSAMNT,0(8,R4)                                                 
         ZAP   TRANSAMT,0(8,R4)                                                 
         SPACE 2                                                                
         LA    R2,ONEDRCRH                                                      
         MVI   ERRNUM,2                                                         
         CLC   ONEDRCR,=C'DR'                                                   
         BE    ONE10                                                            
         CLC   ONEDRCR,=C'CR'                                                   
         BNE   ERROR                                                            
         SPACE 2                                                                
         MVI   DLPSEL,X'6A'        CREDIT POSTING                               
         MVC   DLPSDBAC,CNUM       INVERT NAMES/NUMBERS                         
         MVC   DLPSDBNM,CNAME      FOR CREDIT POSTING                           
         MVC   DLPSCRAC,NUM                                                     
         MVC   DLPSCRNM,NAME                                                    
         SPACE 2                                                                
ONE10    IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         MVI   0(R8),0             END OF RECORD                                
         MVC   HALF,IOAREA                                                      
         LH    R4,HALF                                                          
         AR    R4,R3                                                            
         LA    R4,1(R4)            1 FOR X'00' AT END OF RECORD                 
         STH   R4,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         SPACE 3                                                                
         XC    WORK,WORK                                                        
         IC    R3,ONEDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ONEDOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         LA    R2,ONEDOCH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                   ** NEW             
       EJECT                                                                    
       ++INCLUDE ACBATDSECT                                  ** NEW             
       ++INCLUDE ACBATEBD                                                       
         EJECT                                                                  
PROGD    DSECT                                               ** NEW             
NUM      DS    CL15                                                             
NAME     DS    CL36                                                             
CNUM     DS    CL15                                                             
CNAME    DS    CL36                                                             
KEY      DS    CL49                                                             
IOAREA   DS    CL2000                                                           
PROGDX   DS    0C                                                               
         EJECT                                                                  
*              ACGENBOTH                                                        
*              ACGENDAY                                                         
*              DDFLDIND                                                         
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACBAT13   05/01/02'                                      
         END                                                                    
