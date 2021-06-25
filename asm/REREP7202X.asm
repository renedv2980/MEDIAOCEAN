*          DATA SET REREP7202X AT LEVEL 048 AS OF 05/01/02                      
*PHASE RE7202A,+0                                                               
***********************************************************************         
* HISTORY:                                                                      
*                                                                               
* 28APR92 (SKU) ADD CATEGORY FIELD                                              
*                                                                               
* 12APR92 (SKU) OPTION TO SHOW ONLY ADVERTISER CODES W/O A CATEGORY             
*               CODE                                                            
*                                                                               
* 08NOV95 (RHV) ADD PRINTING OF KATZ CODES TO REPORT                            
*                                                                     *         
* DEC14/95 (BG )  38 CHANGE REGENALL TO REGENALL1 2K CON              *         
*                                                                     *         
* FEB26/97 (DBU)  ACTIVITY DATE FOR INDIVIDUAL RECORD MUST BE         *         
*                 WITHIN REQUEST DATES FOR RECORD TO BE DISPLAYED     *         
*                                                                     *         
*                          ** END TOMBSTONE **                        *         
***********************************************************************         
         TITLE 'ADVERTISER LISTING PROGRAM'                                     
RE7202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7202,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*              CHECK MODE SETTINGS                                              
         SPACE 3                                                                
         MVI   RCSUBPRG,0                                                       
         CLI   QOPTION2,C' '                                                    
         BE    AD1                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   QOPTION2,C'S'                                                    
         BE    AD1                                                              
         MVI   RCSUBPRG,2                                                       
         SPACE 2                                                                
AD1      CLI   MODE,REQFRST                                                     
         BNE   AD2                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     ADEXT                                                            
         SPACE 2                                                                
AD2      CLI   MODE,REQLAST                                                     
         BNE   AD4                                                              
         BAS   RE,PRINTEM                                                       
         B     ADEXT                                                            
AD4      CLI   MODE,PROCADV                                                     
         BNE   ADEXT                                                            
*                                                                               
         CLI   QOPTION1,C'A'       IF OPT 1, ONLY PRINT ADV W/O                 
         BNE   AD5                 CATEGORY CODES                               
         OC    RADVCATG,RADVCATG   CHECK IF NULLS OR SPACES                     
         BZ    AD5                                                              
         CLC   RADVCATG,SPACES                                                  
         BNE   ADEXT                                                            
*                                                                               
AD5      DS    0H                                                               
         CLC   QSTART(6),SPACES                                                 
         BE    AD7                                                              
         GOTO1 DATCON,DMCB,(2,RADVLCD),(3,DATE)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(3,STDATE)                                
         GOTO1 DATCON,DMCB,(0,QEND),(3,ENDDATE)                                 
         CLC   STDATE,DATE         IS DATE WITHIN REQUEST DATES?                
         BH    ADEXT               NO - SKIP IT                                 
         CLC   ENDDATE,DATE        IS DATE WITHIN REQUEST DATES?                
         BL    ADEXT               NO - SKIP IT                                 
AD7      EQU   *                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(4),RADVKADV                                               
         MVC   WORK+7(20),RADVNAME                                              
         BAS   RE,POSTEM                                                        
         CLI   QOPTIONS,C'F'                                                    
         BNE   ADEXT                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(5),RADVKATZ  MOVE KATZ CODE INTO LINE                     
         MVC   WORK+8(20),RADVCITY                                              
         BAS   RE,POSTEM                                                        
         MVC   WORK,SPACES                                                      
         CLC   RADVCATG,SPACES                                                  
         BE    AD10                                                             
         MVC   WORK+7(2),RADVCATG                                               
         BAS   RE,EXPCAT                                                        
AD10     BAS   RE,POSTEM                                                        
         MVC   WORK,SPACES                                                      
         BAS   RE,POSTEM                                                        
         SPACE 2                                                                
ADEXT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO POST TO TABLE                                         
***********************************************************************         
POSTEM   NTR1                                                                   
         SPACE 2                                                                
POST1    LA    R2,PAGETAB                                                       
         LA    R3,192                                                           
         SPACE 2                                                                
POST2    CLI   0(R2),0                                                          
         BE    POST4                                                            
         LA    R2,28(R2)           COLUMN WIDTH INCREASED TO 28 (RHV)           
         BCT   R3,POST2                                                         
         BAS   RE,PRINTEM                                                       
         B     POST1                                                            
         SPACE 2                                                                
POST4    MVC   0(28,R2),WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PRINT FROM TABLE                                      
***********************************************************************         
PRINTEM  NTR1                                                                   
         MVC   PMWORK(20),WORK                                                  
         LA    R2,PAGETAB                                                       
         LA    R3,48                                                            
         LA    R1,48                                                            
         MH    R1,=H'28'                                                        
         LA    R4,0(R2,R1)                                                      
         LA    R5,0(R4,R1)                                                      
         LA    R6,0(R5,R1)                                                      
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
PRINT2   MVC   P(28),0(R2)                                                      
         MVC   P+28(28),0(R4)                                                   
         MVC   P+56(28),0(R5)                                                   
         MVC   P+84(28),0(R6)                                                   
         GOTO1 REPORT                                                           
         XC    0(28,R2),0(R2)                                                   
         XC    0(28,R4),0(R4)                                                   
         XC    0(28,R5),0(R5)                                                   
         XC    0(28,R6),0(R6)                                                   
         LA    R2,28(R2)                                                        
         LA    R4,28(R4)                                                        
         LA    R5,28(R5)                                                        
         LA    R6,28(R6)                                                        
         BCT   R3,PRINT2                                                        
         MVC   WORK(20),PMWORK                                                  
PRINTEX  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO EXPAND CATEGORY NAME                                  
***********************************************************************         
EXPCAT   NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,IOAREA                                                        
         USING RCTGREC,R6                                                       
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,RADVKREP                                                
         MVC   RCTGKCTG,RADVCATG                                                
         MVC   KEY,IOAREA                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXPCATX                                                          
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
         MVC   WORK+10(18),RCTGNAME                                             
*                                                                               
EXPCATX  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
PMWORK   DS    CL20                                                             
RELO     DS    A                                                                
         DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
DATE     DS    CL3                 LAST CHANGED DATE IN BINARY                  
STDATE   DS    CL3                 FROM: REQUEST DATE                           
ENDDATE  DS    CL3                 TO: REQUEST DATE                             
SAVEKEY  DS    CL32                                                             
IOAREA   DS    CL1000                                                           
         SPACE 3                                                                
PAGETAB  DC    6000X'00'                                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048REREP7202X05/01/02'                                      
         END                                                                    
