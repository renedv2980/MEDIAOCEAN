*          DATA SET SPREPFXSID AT LEVEL 021 AS OF 03/28/96                      
*PHASE SPFX02U                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPREPFXSID - FIX NSID AND DETAIL MARKETS'                       
SPFX02   CSECT                                                                  
*        PRINT NOGEN                                                            
         DS    4000C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8                                                      
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
RQF      DS    0H                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
                                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
                                                                                
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         USING SIRRECD,R6                                                       
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVI   SIRKAM,X'61'        <== PETRY                                    
         MVC   SIRKCODE,=X'BE78'   <== SCHEME PTY                               
         GOTO1 HIGH                                                             
                                                                                
FX10     CLC   KEY(SIRKMS-SIRKEY),KEYSAVE                                       
         BNE   FX100                                                            
         LA    R6,KEY                                                           
                                                                                
         OC    SIRKMS,SIRKMS                                                    
         BZ    FX50                                                             
         GOTO1 HEXOUT,DMCB,(R6),P,13                                            
         GOTO1 REPORT                                                           
         CLC   =X'0A51C617C2C5009F81',SIRKMS                                    
         BE    FX20                                                             
         CLC   =X'0A51C617C2C5019F81',SIRKMS                                    
         BNE   FX30                                                             
FX20     MVC   P(9),=C'THROW OUT'                                               
         GOTO1 REPORT                                                           
         B     FX50                                                             
                                                                                
FX30     XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(L'SIRKEY),KEY                                            
                                                                                
         GOTO1 MSUNPK,DMCB,SIRKMS,MARKT,STATN                                   
         DROP  R6                                                               
         USING STARECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,STATN                                                   
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT(6),=C'000000'                                            
         GOTO1 HIGHSTA                                                          
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADSTAT                                                        
         MVC   MARKT,SMKT                                                       
         DROP  R6                                                               
         GOTO1 MSPACK,DMCB,MARKT,STATN,PMKTSTA                                  
                                                                                
                                                                                
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'SAVEKEY),SAVEKEY                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* PUT RECORD TO SORTER                                                          
         LA    R6,RECOUT+4                                                      
         ST    R6,AREC                                                          
         USING SIRRECD,R6                                                       
         XCEF  RECOUT,4004         CLEAR AREA FIRST.                            
         GOTO1 GET                                                              
                                                                                
         MVC   SIRKMS,PMKTSTA                                                   
         ZICM  R1,SIRRLEN,2        GET RECORD LENGTH                            
         LA    R1,4(R1)            ADD (4)                                      
         STCM  R1,3,RECOUT         PUT LENGTH OF RECORD FOR SORT                
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT                                   
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
FX50     GOTO1 SEQ                                                              
         B     FX10                                                             
         DROP  R6                                                               
                                                                                
********************  PROCESSING END, SHOW QUOTAS  ********************         
FX100    MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
                                                                                
         XC    SAVEKEY,SAVEKEY                                                  
FX105    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    FX120                                                            
         CLC   SAVEKEY,4(R4)                                                    
         BNE   FX110                                                            
         MVC   P1(7),=C'DUP KEY'                                                
         GOTO1 HEXOUT,DMCB,4(R4),P2,13                                          
         GOTO1 REPORT                                                           
         B     FX115                                                            
FX110    PUT   TAPEOUT,(R4)                                                     
FX115    MVC   SAVEKEY,4(R4)                                                    
         B     FX105                                                            
                                                                                
FX120    GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (TAPEOUT,)                                                       
         GOTO1 AENDREQ                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**************************** MISCELLANEOUS ****************************         
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
** COUNTERS **                                                                  
COUNT    DS    F                   # OF RECORDS ON TAPE                         
*                                                                               
*                                                                               
SAGY     DS    CL2                 AGENCY                                       
SCTRY    DS    C                   COUNTRY CODE                                 
PMKTSTA  DS    XL5                 PACKED MARKET & STATION                      
MARKT    DS    CL4                 EBCIDIC MARKET                               
STATN    DS    CL5                 EBCIDIC STATION                              
SAVEKEY  DS    XL13                                                             
         DS    0F                                                               
STAWORK  DS    XL31                STAPACK DSECT                                
** SORTER'S CARDS **                                                            
SORTCARD DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4004'                                  
*                                                                               
RECOUT   DS    CL4004                                                           
         EJECT                                                                  
************************ STATION-RECORDS DSECT ************************         
*                                                                               
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREPFXSID03/28/96'                                      
         END                                                                    
