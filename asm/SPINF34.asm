*          DATA SET SPINF34    AT LEVEL 004 AS OF 05/18/07                      
*PHASE T21A34A                                                                  
         TITLE 'T21A34 - DAYPART MENU DISPLAY'                                  
* UPON ENTRY DAYPART HEADER IS IN REC 2                                         
*                                                                               
T21A34   CSECT                                                                  
         NMOD1 0,T21A34                                                         
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    RF,REC2             SET UP FOR BUCKETING                         
         ST    RF,USER6                                                         
         XC    USER7,USER7                                                      
         LA    RE,REC                                                           
         LA    RF,4000                                                          
         XCEF                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DPTHDRD,R4                                                       
         MVI   DPTKTYPE,8                                                       
         MVC   DPTKAGY,AGYALPHA                                                 
         MVC   DPTKMED,SVEBCMED                                                 
         MVC   DPTKMENU,PREVKEY                                                 
         CLI   DPTKMENU,0          GET PAST OLD MENUS                           
         BNE   *+8                                                              
         MVI   DPTKMENU,1                                                       
GR00     GOTO1 HIGH                                                             
*                                                                               
         CLI   PREVKEY,0           FIRST TIME FOR THIS AGY                      
         BE    GR1A                                                             
         GOTO1 SEQ                  NO - GET NEXT MENU                          
GR1A     DS    0C                                                               
*                                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BE    RECOK                                                            
         LA    R4,KEYSAVE          MENU NOT FOUND - TRY FOR DEFAULTS            
         CLC   DPTKAGY,=C'00'                                                   
         BE    GR2A                                                             
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   DPTKTYPE,8                                                       
         MVC   DPTKAGY,=C'00'                                                   
         MVC   DPTKMED,SVEBCMED                                                 
         MVC   DPTKMENU,PREVKEY                                                 
         B     GR00                                                             
GR2A     DS    0H                                                               
         CLI   PREVKEY,0           DID WE DISPLAY ANYTHING                      
         BZ    GR2X                NO -                                         
         XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
*                                                                               
GR2X     MVI   ERRCD,NOFNDERR                                                   
         GOTO1 ERROR                                                            
         EJECT                                                                  
RECOK    GOTO1 GETREC                                                           
         LA    R4,REC                                                           
         L     R6,USER6                                                         
         L     R9,USER7                                                         
         LA    R5,DPTEL                                                         
         USING DPTEL,R5                                                         
*                                                                               
* BUILD DAYPART TABLE                                                           
*                                                                               
HAVEL    LA    R7,DPTCODES                                                      
HAVEL2   CLI   0(R7),0                                                          
         BE    ENDEL                                                            
         MVC   0(1,R6),DPTKMENU                                                 
         MVC   1(1,R6),1(R7)       SET MS CODE                                  
         MVC   2(1,R6),0(R7)       SET DAYPART LETTER                           
         MVC   3(3,R6),2(R7)       SET DAYPART DESCRIPTION                      
         LA    R6,6(R6)            NEXT SLOT                                    
         LA    R9,1(R9)            BUMP COUNT                                   
         ST    R6,USER6                                                         
         ST    R9,USER7                                                         
         LA    R7,5(R7)            NEXT DAYPART                                 
         B     HAVEL2                                                           
*                                                                               
ENDEL    GOTO1 SEQ                 TRY FOR NEXT DPT MENU                        
         CLC   KEY(4),KEYSAVE                                                   
         BE    RECOK                                                            
         SPACE 1                                                                
*                                                                               
* SET COLUMN STARTS AND NO OF ENTRIES                                           
         XC    DMWORK(28),DMWORK                                                
         MVI   DMWORK+28,X'FF'                                                  
         LA    R6,REC2                                                          
         LA    R7,DMWORK                                                        
SC1      CLI   0(R6),0             END                                          
         BE    SEND                 YES - SEND SCREEN                           
         ST    R6,0(R7)            SET BEGIN                                    
         LA    RF,1                SET INITIAL COUNT                            
SC2      CLC   0(1,R6),6(R6)                                                    
         BNE   SC3                                                              
         LA    RF,1(RF)                                                         
         LA    R6,6(R6)                                                         
         CH    RF,=H'15'           MAX OF 15 IN ONE SLOT                        
         BE    SC3                                                              
         B     SC2                                                              
*                                                                               
SC3      STC   RF,0(R7)                                                         
         CLC   0(1,R6),6(R6)       WAIT TILL MENU COMPLETED                     
         BE    *+10                                                             
         MVC   PREVKEY(1),0(R6)    SAVE PROCESSED DAYPART CODE FOR NXT          
         LA    R7,4(R7)                                                         
         LA    R6,6(R6)                                                         
         CLI   0(R7),X'FF'                                                      
         BNE   SC1                                                              
         B     SEND2                                                            
         EJECT                                                                  
SEND     XC    PREVKEY,PREVKEY                                                  
SEND2    LA    R7,DMWORK           SET UP HEADLINES                             
         LA    R2,SINHDRH                                                       
         LA    RE,FLDDATA+1                                                     
         LA    RF,LINLEN(RE)                                                    
*                                                                               
SEND3    CLI   0(R7),0             BUILD HEADLINES                              
         BE    SEND4                                                            
         CLI   0(R7),X'FF'                                                      
         BE    SEND4                                                            
         MVC   0(4,RE),=C'MENU'                                                 
         L     R6,0(R7)                                                         
         MVC   5(1,RE),0(R6)       SET MENU NUMBER                              
         MVC   0(6,RF),DASH                                                     
         LA    R7,4(R7)                                                         
         LA    RE,9(RE)                                                         
         LA    RF,9(RF)                                                         
         B     SEND3                                                            
*                                                                               
SEND4    FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         EJECT                                                                  
*                                                                               
* SEND DATA LINES                                                               
SDATA    LA    RF,FLDDATA+1                                                     
         LA    R7,DMWORK                                                        
         MVI   FULL,0                                                           
SDATA1   CLI   0(R7),0                                                          
         BNE   SDATA2                                                           
         LA    RF,9(RF)                                                         
         LA    R7,4(R7)                                                         
         B     SDATA1                                                           
SDATA2   CLI   0(R7),X'FF'                                                      
         BE    SDATA3                                                           
         MVI   FULL,1                                                           
         L     R6,0(R7)                                                         
         MVC   0(1,RF),2(R6)       DAYPART CODE                                 
         MVI   1(RF),C'/'                                                       
         SR    RE,RE               CONVERT MS CODE                              
         IC    RE,1(R6)                                                         
         SRL   RE,4                                                             
         IC    RE,ALPHATAB(RE)                                                  
         STC   RE,2(RF)                                                         
*                                                                               
         IC    RE,1(R6)                                                         
         N     RE,=X'0000000F'                                                  
         IC    RE,ALPHATAB(RE)                                                  
         STC   RE,3(RF)                                                         
*                                                                               
         MVI   4(RF),C'/'                                                       
         MVC   5(3,RF),3(R6)                                                    
         LA    R6,6(R6)                                                         
         IC    RE,0(R7)                                                         
         BCTR  RE,0                                                             
         ST    R6,0(R7)                                                         
         STC   RE,0(R7)                                                         
         LA    R7,4(R7)                                                         
         LA    RF,9(RF)                                                         
         B     SDATA1                                                           
SDATA3   CLI   FULL,1              END                                          
         BNE   MODEXIT              YES - EXIT                                  
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     SDATA                                                            
*                                                                               
ALPHATAB DC    C'0123456789ABCDEF'                                              
ENDREC   XC    PREVKEY,PREVKEY                                                  
*                                                                               
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
DASH     DC    40C'-'                                                           
         LTORG                                                                  
LINLEN   EQU   88                                                               
         EJECT                                                                  
*SPINFWORK                                                                      
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
DPTHDRD  DSECT                                                                  
*SPGENDAYPT                                                                     
       ++INCLUDE SPGENDAYPT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPINF34   05/18/07'                                      
         END                                                                    
