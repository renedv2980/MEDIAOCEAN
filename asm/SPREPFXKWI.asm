*          DATA SET SPREPFXKWI AT LEVEL 040 AS OF 01/05/97                      
*PHASE SPFX02U                                                                  
         TITLE 'SPREPFXKWI - FIX NSID AND DETAIL MARKETS'                       
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
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
                                                                                
         XC    COUNT,COUNT                                                      
*                                                                               
* GET HEADER RECORDS WHOSE KEYS ARE IN TABLE                                    
*                                                                               
         LA    R2,ADDKEYS                                                       
FX10     CLI   0(R2),X'FF'                                                      
         BE    GOAL10                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PTREC                                                         
         LA    R2,13(R2)                                                        
         B     FX10                                                             
*                                                                               
* GET GOALS                                                                     
GOAL10   DS    0H                                                               
         MVI   RECTYP,C'G'                                                      
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         MVC   KEY(5),=X'027180F302'  T,AHT,ADV                                 
GOAL20   GOTO1 HIGH                                                             
GOAL30   CLC   KEY(5),KEYSAVE                                                   
         BNE   GOAL60                                                           
         CLI   KEY+7,X'3E'         ESTIMATE 62 - 64                             
         BL    GOAL40                                                           
         CLI   KEY+7,X'40'                                                      
         BH    GOAL50                                                           
         BAS   RE,PTREC                                                         
         GOTO1 SEQ                                                              
         B     GOAL30                                                           
GOAL40   MVI   KEY+7,X'3E'                                                      
         XC    KEY+8(5),KEY+8                                                   
         B     GOAL20                                                           
GOAL50   MVC   KEY+7(6),=X'FFFFFFFFFFFF' GET NEXT MARKET                        
         B     GOAL20                                                           
                                                                                
                                                                                
GOAL60   DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         MVC   KEY(5),=X'027180F3FF'  T,AHT,POL                                 
GOAL70   GOTO1 HIGH                                                             
GOAL80   CLC   KEY(5),KEYSAVE                                                   
         BNE   GOAL110                                                          
         CLI   KEY+7,X'3E'         ESTIMATE 62 - 64                             
         BL    GOAL90                                                           
         CLI   KEY+7,X'40'                                                      
         BH    GOAL100                                                          
         BAS   RE,PTREC                                                         
         GOTO1 SEQ                                                              
         B     GOAL80                                                           
GOAL90   MVI   KEY+7,X'3E'                                                      
         XC    KEY+8(5),KEY+8                                                   
         B     GOAL70                                                           
GOAL100  MVC   KEY+7(6),=X'FFFFFFFFFFFF' GET NEXT MARKET                        
         B     GOAL70                                                           
                                                                                
GOAL110  DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         MVC   KEY(5),=X'027280F302'  R,AHT,ADV                                 
GOAL120  GOTO1 HIGH                                                             
GOAL130  CLC   KEY(5),KEYSAVE                                                   
         BNE   GOAL160                                                          
         CLI   KEY+7,X'3E'         ESTIMATE 62                                  
         BL    GOAL140                                                          
         BH    GOAL150                                                          
         BAS   RE,PTREC                                                         
         GOTO1 SEQ                                                              
         B     GOAL130                                                          
*                                                                               
GOAL140  MVI   KEY+7,X'3E'                                                      
         XC    KEY+8(5),KEY+8                                                   
         B     GOAL120                                                          
GOAL150  MVC   KEY+7(6),=X'FFFFFFFFFFFF' GET NEXT MARKET                        
         B     GOAL120                                                          
*                                                                               
GOAL160  DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         MVC   KEY(5),=X'027280F3FF'  R,AHT,POL                                 
GOAL170  GOTO1 HIGH                                                             
GOAL180  CLC   KEY(5),KEYSAVE                                                   
         BNE   BUY10                                                            
         CLI   KEY+7,X'3E'         ESTIMATE 62                                  
         BL    GOAL190                                                          
         BH    GOAL200                                                          
         BAS   RE,PTREC                                                         
         GOTO1 SEQ                                                              
         B     GOAL180                                                          
*                                                                               
GOAL190  MVI   KEY+7,X'3E'                                                      
         XC    KEY+8(5),KEY+8                                                   
         B     GOAL170                                                          
GOAL200  MVC   KEY+7(6),=X'FFFFFFFFFFFF' GET NEXT MARKET                        
         B     GOAL170                                                          
*                                                                               
* GET BUYS                                                                      
*                                                                               
BUY10    DS    0H                                                               
         MVI   RECTYP,C'B'                                                      
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         MVC   KEY(4),=X'7180F302'  T,AHT,ADV                                   
BUY20    GOTO1 HIGH                                                             
BUY30    CLC   KEY(4),KEYSAVE                                                   
         BNE   BUY60                                                            
         CLI   KEY+10,X'80'        TEST FOR SPILL POINTER                       
         BE    BUY35                                                            
         CLI   KEY+9,X'3E'         ESTIMATE 62                                  
         BL    BUY50                                                            
         CLI   KEY+9,X'40'         ESTIMATE 64                                  
         BH    BUY40                                                            
         BAS   RE,PTREC                                                         
BUY35    DS    0H                                                               
         GOTO1 SEQ                                                              
         B     BUY30                                                            
*                                                                               
BUY40    MVC   KEY+9(4),=X'FFFFFFFF'   GET NEXT STATION                         
         B     BUY20                                                            
BUY50    MVI   KEY+9,X'3E'                                                      
         XC    KEY+10(3),KEY+10                                                 
         B     BUY20                                                            
                                                                                
* GET RADIO BUYS                                                                
BUY60    DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R6,KEY                                                           
         MVC   KEY(4),=X'7280F302'  R,AHT,ADV                                   
BUY70    GOTO1 HIGH                                                             
BUY80    CLC   KEY(4),KEYSAVE                                                   
         BNE   FX200                                                            
         CLI   KEY+9,X'3E'         ESTIMATE 62                                  
         BL    BUY90                                                            
         BH    BUY100                                                           
         BAS   RE,PTREC                                                         
         GOTO1 SEQ                                                              
         B     BUY80                                                            
                                                                                
BUY90    MVI   KEY+9,X'3E'                                                      
         XC    KEY+10(3),KEY+10                                                 
         B     BUY70                                                            
BUY100   MVC   KEY+9(4),=X'FFFFFFFF'   GET NEXT STATION                         
         B     BUY70                                                            
                                                                                
********************  PROCESSING END, SHOW QUOTAS  ********************         
FX200    MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         CLOSE (TAPEOUT,)                                                       
         GOTO1 AENDREQ                                                          
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  PUT RECORD TO DATA SET                                             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PTREC    NTR1                                                                   
         LA    R6,RECOUT+4                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
                                                                                
         CLI   RECTYP,C'B'         IS IT A BUY?                                 
         BE    PTREC10                                                          
         NI    1(R6),X'FF'-X'F0'   TURN OFF KA AGENCY BIT                       
         OI    1(R6),X'10'         TURN ON WESTERN AGENCY                       
         B     PTREC20                                                          
                                                                                
PTREC10  DS    0H                                                               
         NI    0(R6),X'FF'-X'F0'   TURN OFF KA AGENCY BIT                       
         OI    0(R6),X'10'         TURN ON WESTERN AGENCY                       
                                                                                
PTREC20  ZICM  R1,13(R6),2         GET RECORD LENGTH                            
         LA    R1,4(R1)            ADD (4)                                      
         STCM  R1,3,RECOUT         PUT LENGTH OF RECORD FOR SORT                
         GOTO1 HEXOUT,DMCB,RECOUT+4,P,13,=C'TOG'                                
         GOTO1 REPORT                                                           
         PUT   TAPEOUT,RECOUT                                                   
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**************************** MISCELLANEOUS ****************************         
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*                                                                               
ADDKEYS  DS    0H                                                               
         DC    XL13'007180F3000000000000000000'   T/AHT                         
         DC    XL13'007180F3C1C4E5000000000000'   T/AHT/ADV                     
         DC    XL13'007180F3C1C4E53E0000000000'   T/AHT/ADV/62                  
         DC    XL13'007180F3C1C4E53F0000000000'   T/AHT/ADV/63                  
         DC    XL13'007180F3C1C4E5400000000000'   T/AHT/ADV/64                  
         DC    XL13'007180F3D7D6D3000000000000'   T/AHT/POL                     
         DC    XL13'007180F3D7D6D33E0000000000'   T/AHT/POL/62                  
         DC    XL13'007180F3D7D6D33F0000000000'   T/AHT/POL/63                  
         DC    XL13'007180F3D7D6D3400000000000'   T/AHT/POL/64                  
         DC    XL13'007280F3000000000000000000'   R/AHT                         
         DC    XL13'007280F3C1C4E5000000000000'   R/AHT/ADV                     
         DC    XL13'007280F3C1C4E53E0000000000'   R/AHT/ADV/62                  
         DC    XL13'007280F3D7D6D3000000000000'   R/AHT/POL                     
         DC    XL13'007280F3D7D6D33E0000000000'   R/AHT/POL/62                  
         DC    X'FF'                                                            
*                                                                               
** COUNTERS **                                                                  
COUNT    DS    F                   # OF RECORDS ON TAPE                         
*                                                                               
SAGY     DS    CL2                 AGENCY                                       
SCTRY    DS    C                   COUNTRY CODE                                 
RECTYP   DS    C                   B=BUY                                        
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
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040SPREPFXKWI01/05/97'                                      
         END                                                                    
