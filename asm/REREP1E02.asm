*          DATA SET REREP1E02  AT LEVEL 021 AS OF 05/01/02                      
*PHASE RE1E02C,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREP1E02 - STATION REC COMBO ELEM SWITCHER'                    
**********************************************************************          
*                                                                    *          
*        REREP1E02 --- STATION REC COMBO ELEM SWITCHER               *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 10APR91 (EFJ) --- INITIAL ENTRY                                    *          
*                                                                    *          
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                    *          
**********************************************************************          
*                                                                               
RE1E02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1E02,RR=RE                                                 
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXXMOD              DO ALL IN REQFRST                            
*                                                                               
* READ ALL STATION RECS.  IF IT IS A COMBO REC, DELETE IT'S COMBO               
* ELEM AND REBUILD FOR NEW STATION REC FMT.  THEN READ STATIONS IN              
* IT'S COMBO ELEMENT AND ADD THE COMBO STATION INFO THERE                       
*                                                                               
* FOR QOPTION1=T, SKIP ALL DISK WRITES (PUTREC, ADDREC, ETC)                    
*                                                                               
MAIN     DS    0H                                                               
         LA    RF,RSTAREC                                                       
         ST    RF,AIOAREA                                                       
         XC    KEY,KEY             GET FIRST COMBO STATION REC                  
         MVI   KEY,X'02'                                                        
         GOTO1 HIGH                                                             
MAIN05   CLI   KEY,X'02'           MAKE SURE STILL STA RECS                     
         BE    *+6                                                              
         DC    H'0'                NO COMBO RECS FOUND                          
         CLI   KEY+26,C'C'                                                      
         BE    MAIN07                                                           
         GOTO1 SEQ                                                              
         B     MAIN05                                                           
MAIN07   DS    0H                                                               
         MVC   LASTCM,KEY          SAVE OFF KEY                                 
         GOTO1 GREC                                                             
         B     MAIN20                                                           
*                                                                               
MAIN10   DS    0H                                                               
         BAS   RE,GETCOMBO         GET NEXT COMBO STATION REC                   
         BZ    EXXMOD              CC=0 = NO MORE STATION RECS                  
MAIN20   BAS   RE,NEWELS           DELETE OLD COMBO ELEM AND PUT NEW            
         BZ    MAIN10              CC=0 = ERR IN CM REC, DON'T UPD STAS         
         BAS   RE,UPDSTATS         UPDATE COMBO'S STA WITH COMBO ELEM           
         B     MAIN10                                                           
         EJECT                                                                  
*                                                                               
* GET NEXT COMBINATION STATION RECORD - EXIT CC = 0 WHEN NO MORE RECS           
GETCOMBO NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(27),LASTCM                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
GC10     GOTO1 SEQ                                                              
         CLI   KEY,X'02'           STILL ON STA RECS?                           
         BNE   GCDONE                                                           
         CLI   KEY+26,C'C'                                                      
         BNE   GC10                                                             
*                                                                               
         GOTO1 GREC                                                             
         MVC   LASTCM,KEY                                                       
         LTR   RB,RB                                                            
         B     GCX                                                              
*                                                                               
GCDONE   SR    R0,R0                                                            
GCX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DELETE OLD COMBO ELEMS FROM COMBO REC AND ADD NEW STYLE                       
NEWELS   NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFIL'),(X'09',AIOAREA),0,0,   X        
               RR=RELO                                                          
         CLI   DMCB+12,X'00'       ANY ERRORS?                                  
         BNE   NEERR2                                                           
*                                                                               
         L     R6,DMCB+12          ADDR OF ELEM                                 
         USING RSTAAFEL,R6                                                      
*                                                                               
* NOW BUILD 2 NEW CM EL'S (X'0A') - FIRST AM, THEN FM                           
         XC    NEWEL,NEWEL                                                      
         MVC   NEWEL(2),=X'0A07'                                                
         MVC   NEWEL+2(4),RSTAAM                                                
         MVC   AM,RSTAAM                                                        
         MVI   NEWEL+6,C'A'                                                     
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFIL'),(0,AIOAREA),(0,NEWEL), X        
               0,0,RR=RELO                                                      
         CLI   DMCB+12,X'00'       ANY ERRORS?                                  
         BE    *+14                                                             
         CLI   DMCB+12,X'05'                                                    
         BE    NEERR                                                            
         DC    H'0'                NO OTHER ERROR ALLOWED                       
*                                                                               
         MVC   NEWEL+2(4),RSTAFM                                                
         MVC   FM,RSTAFM                                                        
         MVI   NEWEL+6,C'F'                                                     
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFIL'),(0,AIOAREA),(0,NEWEL), X        
               0,0,RR=RELO                                                      
         CLI   DMCB+12,X'00'       ANY ERRORS?                                  
         BE    *+14                                                             
         CLI   DMCB+12,X'05'                                                    
         BE    NEERR                                                            
         DC    H'0'                NO OTHER ERROR ALLOWED                       
         DROP  R6                                                               
*                                                                               
* NOW DELETE OLD X'09' ELEM                                                     
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFIL'),(X'09',AIOAREA),0,0,   X        
               RR=RELO                                                          
*                                                                               
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    NE10                                                             
*                                                                               
         GOTO1 PREC                                                             
NE10     DS    0H                                                               
         MVC   P(7),KEY+20                                                      
         GOTO1 REPORT                                                           
*                                                                               
         LTR   RB,RB               SET CC<>0                                    
         B     NEX                                                              
*                                                                               
NEERR    DS    0H                                                               
         MVC   P(43),=C'RECORD HAS BECOME TOO LARGE - NOT UPDATED: '            
         MVC   P+43(7),KEY+20                                                   
         GOTO1 REPORT                                                           
         LTR   RB,RB               SET CC<>0 - STILL OK TO UPD STAS             
         B     NEX                                                              
*                                                                               
NEERR2   DS    0H                                                               
         MVC   P(43),=C'OLD COMBO ELEM NOT FOUND    - NOT UPDATED: '            
         MVC   P+43(7),KEY+20                                                   
         GOTO1 REPORT                                                           
         SR    R0,R0               SET CC=0, DON'T KNOW STAS                    
         B     NEX                                                              
*                                                                               
NEX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* UPDATE COMBO STATIONS WITH COMBO ELEM                                         
UPDSTATS NTR1                                                                   
         MVC   KEY+22(4),AM                                                     
         MVI   KEY+26,C'A'                                                      
US05     GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   USERR                                                            
*                                                                               
         GOTO1 GREC                                                             
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFIL'),(X'0A',AIOAREA),0,0,   X        
               RR=RELO                                                          
         CLI   DMCB+12,X'06'                                                    
         BNE   USERR3                                                           
*                                                                               
         MVC   NEWEL(2),=X'0A07'                                                
         MVC   NEWEL+2(5),LASTCM+22                                             
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFIL'),(0,AIOAREA),(0,NEWEL), X        
               0,0,RR=RELO                                                      
         CLI   DMCB+12,X'00'       ANY ERRORS?                                  
         BE    *+14                                                             
         CLI   DMCB+12,X'05'                                                    
         BE    USERR2                                                           
         DC    H'0'                NO OTHER ERROR ALLOWED                       
*                                                                               
         CLI   QOPTION1,C'T'                                                    
         BE    US10                                                             
*                                                                               
         GOTO1 PREC                                                             
US10     DS    0H                                                               
         MVC   P+5(7),KEY+20                                                    
         GOTO1 REPORT                                                           
         B     US20                                                             
*                                                                               
US20     DS    0H                                                               
         CLI   KEYSAVE+26,C'F'                                                  
         BE    USX                                                              
         MVC   KEY+22(4),FM                                                     
         MVI   KEY+26,C'F'                                                      
         B     US05                                                             
*                                                                               
USERR    DS    0H                                                               
         MVC   P+5(29),=C'WARNING - STATION NOT FOUND: '                        
         MVC   P+34(7),KEYSAVE+20                                               
         GOTO1 REPORT                                                           
         B     US20                                                             
*                                                                               
USERR2   DS    0H                                                               
         MVC   P+5(43),=C'RECORD HAS BECOME TOO LARGE - NOT UPDATED: '          
         MVC   P+48(7),KEY+20                                                   
         GOTO1 REPORT                                                           
         B     US20                                                             
*                                                                               
USERR3   DS    0H                                                               
         MVC   P+5(43),=C'RECORD HAS COMBO ELEMENT    - NOT UPDATED: '          
         MVC   P+48(7),KEY+20                                                   
         L     R6,DMCB+12          ADDR OF ELEM                                 
         MVC   P+60(07),=C'COMBO: '                                             
         MVC   P+67(5),2(R6)                                                    
         GOTO1 REPORT                                                           
         B     US20                                                             
*                                                                               
USX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EXXMOD   XMOD1                                                                  
EXIT     XIT1                                                                   
*                                                                               
* DATAMGR INTERFACE                                                             
       ++INCLUDE RGENIO                                                         
*                                                                               
* WORKING STORAGE                                                               
LASTCM   DS    CL27                KEY OF LAST COMBO REC CHANGED                
RELO     DS    F                                                                
COMMAND  DS    CL8                                                              
AIOAREA  DS    A                                                                
NEWEL    DS    CL7                                                              
AM       DS    CL4                                                              
FM       DS    CL4                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021REREP1E02 05/01/02'                                      
         END                                                                    
