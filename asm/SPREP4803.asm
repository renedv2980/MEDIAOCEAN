*          DATA SET SPREP4803  AT LEVEL 023 AS OF 02/10/95                      
*PHASE SP4803A,*                                                                
         TITLE 'SP4803 - STATION FILE PRINT - SUBCONTROLLER'                    
SP4803   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4803                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         CLI   MODE,REQFRST                                                     
         BNE   SP05                                                             
* NEED TO DO DUMMY CALL TO STAPACK SO THAT WHEN CANADIAN TABLE IS               
* BUILT FROM THE X POINTERS, WE DO NOT LOSE SEQUENTIAL READ SEQUENCE            
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),QAGY                                                    
         GOTO1 READ                                                             
         GOTO1 GETAGY                                                           
         L     R6,ADAGY                                                         
         USING AGYHDR,R6                                                        
         CLI   AGYPROF+7,C'C'      TFC (TEST CANADA)                            
         BNE   SP05                                                             
         DROP  R6                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,RCAGENCY                                                 
         MVI   STAPCTRY,C'C'       FORCE TABLE BUILD                            
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'     MARKET                                     
         MVC   STAPQSTA(4),=C'CFTOT' STATION                                    
         GOTO1 VSTAPACK,(R1)         NOTE NO TEST FOR ERRORS   !                
         SPACE 1                                                                
SP05     CLI   QOPT1,C' '          PRINT ALL REPORT TYPES?                      
         BE    SP10                 YES.                                        
         MVI   SP10+5,X'70'         NO.                                         
         MVI   SP70+5,X'70'                                                     
         CLI   QOPT2,C'Y'          PRINT MKT/STA/REP LISTING?                   
         BE    SP10                                                             
         MVI   SP100+5,X'70'                                                    
         EJECT                                                                  
* MARKET RECORDS *                                                              
         SPACE 1                                                                
SP10     DS    0H                                                               
         CLI   QOPT1,C'M'                                                       
         BC    0,SP40                                                           
         L     R8,ADMARKET         MARKET RECORDS                               
         USING MKTREC,R8                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   RECTYPE,KEY                                                      
         GOTO1 HIGHMKT                                                          
         SPACE 1                                                                
SP20     DS    0H                                                               
         CLC   RECTYPE,0(R8)       IS THIS MKT REC OF RIGHT MEDIA?              
         BNE   SP40                 NO.                                         
         CLC   MKTKAGY,QAGY        IS THIS THE RIGHT AGENCY?                    
         BNE   SP30                 NO.                                         
         MVI   MODE,MKTFRST                                                     
         GOTO1 GO                                                               
         SPACE 1                                                                
SP30     DS    0H                                                               
         GOTO1 SEQMKT                                                           
         B     SP20                                                             
         DROP  R8                                                               
         SPACE 1                                                                
* STATION RECORDS *                                                             
         SPACE 1                                                                
SP40     DS    0H                                                               
         L     R8,ADSTAT           MASTER STATION RECORDS                       
         USING STAREC,R8                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   RECTYPE,KEY                                                      
         CLI   QSTA+4,C'/'         ONLY CABLE STATIONS                          
         BNE   *+10                                                             
         MVC   KEY+2(4),=C'0000'                                                
         SPACE 2                                                                
         GOTO1 HIGHSTA                                                          
         SPACE 1                                                                
SP50     DS    0H                                                               
         CLC   RECTYPE,0(R8)       IS THIS STA REC OF THE RIGHT MEDIA?          
         BNE   SP70                 NO.                                         
         CLI   QSTA+4,C'-'         IF ONLY BROADCAST                            
         BNE   SP55                                                             
         CLC   STAKCALL(4),=C'0000'   STOP WHEN REACH CABLE                     
         BH    SP70                                                             
         SPACE 1                                                                
SP55     CLC   STAKAGY,QAGY        IS THIS THE RIGHT AGENCY?                    
         BNE   SP60                 NO.                                         
         MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
         SPACE 1                                                                
SP60     DS    0H                                                               
         GOTO1 SEQSTA                                                           
         B     SP50                                                             
         DROP  R8                                                               
         EJECT                                                                  
* STATION ADDRESS RECORDS *                                                     
         SPACE 1                                                                
SP70     DS    0H                                                               
         CLI   QOPT1,C'S'                                                       
         BC    0,SP100                                                          
         L     R8,ADSTATAD         STATION ADDRESS RECORDS                      
         USING ADDRREC,R8                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'A'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   RECTYPE,KEY                                                      
         GOTO1 HIGHSTAD                                                         
         SPACE 1                                                                
SP80     DS    0H                                                               
         CLC   RECTYPE,0(R8)       IS THIS ADDR REC OF THE RIGHT MEDIA?         
         BNE   SP100                NO.                                         
         CLC   ADDKAGY,QAGY        IS THIS THE RIGHT AGENCY?                    
         BNE   SP90                 NO.                                         
         MVI   MODE,PROCBUY                                                     
         GOTO1 GO                                                               
         SPACE 1                                                                
SP90     DS    0H                                                               
         GOTO1 SEQSTAD                                                          
         B     SP80                                                             
         DROP  R8                                                               
         SPACE 1                                                                
* REP RECORDS *                                                                 
         SPACE 1                                                                
SP100    DS    0H                                                               
         CLI   QOPT1,C'R'                                                       
         BC    0,SP130                                                          
         L     R8,ADREP            REP RECORDS                                  
         USING REPREC,R8                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   RECTYPE,KEY                                                      
         GOTO1 HIGHREP                                                          
         SPACE 1                                                                
SP110    DS    0H                                                               
         CLC   RECTYPE,0(R8)       IS THIS REP REC OF THE RIGHT MEDIA?          
         BNE   SP130                NO.                                         
         CLC   REPKAGY,QAGY        IS THIS THE RIGHT AGENCY?                    
         BNE   SP120                NO.                                         
         MVI   MODE,PROCGOAL                                                    
         GOTO1 GO                                                               
         SPACE 1                                                                
SP120    DS    0H                                                               
         GOTO1 SEQREP                                                           
         B     SP110                                                            
         DROP  R8                                                               
         EJECT                                                                  
* CONTROL PRINTING & CHECK IF THERE ARE ANY MORE MEDIA TO DO *                  
         SPACE 1                                                                
SP130    DS    0H                                                               
         MVI   MODE,MKTLAST                                                     
         GOTO1 GO                                                               
         SPACE 1                                                                
         MVI   SP10+5,0                                                         
         MVI   SP70+5,0                                                         
         MVI   SP100+5,0                                                        
         XIT1                                                                   
         SPACE 2                                                                
* DATA *                                                                        
         SPACE 1                                                                
         DS    0H                                                               
RECTYPE  DS    H                   RECORD TYPE/MEDIA                            
         SPACE 1                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPREP4803 02/10/95'                                      
         END                                                                    
