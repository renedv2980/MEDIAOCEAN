*          DATA SET SPREPFXGL  AT LEVEL 032 AS OF 02/26/98                      
*          DATA SET SPREPFXCBX AT LEVEL 049 AS OF 01/30/98                      
*PHASE SPFX02G                                                                  
*                                                                               
SPFX02G  TITLE 'SPFX02G - CONVERT COKE RADIO GOALS TO 60 SEC'                   
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPFX02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX2                                                              
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=====================================================================*         
* REQFRST PROCESSING                                                  *         
*=====================================================================*         
         SPACE 1                                                                
FX2      DS    0H                                                               
*                                                                               
* LOOK FOR COUNTER IN COL 50(6)                                                 
*                                                                               
         CLI   QAREA+49,C'0'       TEST ANY MAX COUNT                           
         BL    FX4                                                              
         PACK  MAXCOUNT,QAREA+49(6)                                             
*                                                                               
FX4      OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R8,ADGOAL                                                        
         USING GOALRECD,R8         MAPPING GOAL RECORD DSECT                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'02B2'                                                  
         GOTO1 HIGH                                                             
         B     FX25                                                             
*                                                                               
FX20     GOTO1 SEQ                                                              
*                                                                               
FX25     CLC   KEY(2),=X'02B2'     LOOP EXISTING CONDITIONS                     
         BNE   FX100                                                            
*                                  LAST SAVED REC IS HANDLED IN FX100           
         CP    MAXCOUNT,RECOUNT                                                 
         BNH   FX100                                                            
*                                                                               
         CLI   KEY+9,X'32'                                                      
         BNE   FX26                                                             
         AP    RECBAD,=PL1'1'      # OF BAD RECORDS                             
*                                                                               
         B     FX20                                                             
*                                                                               
FX26     GOTO1 GETGOAL                                                          
         AP    RECOUNT,=PL1'1'     TOTAL # OF RECORDS                           
*                                                                               
         CLC   KEY(9),LASTKEY      FIRST 9 BYTES TO BE COMPARED                 
         BNE   FX40                                                             
*                                                                               
         CLI   SAVEDREC+9,X'3C'    GOT TO BE 60                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
******** ZICM  R3,SAVEDREC+13,2    REC LENGTH IS 13 BYTES AFTER KEY             
******** GOTO1 PRNTBL,DMCB,=C'SAVEDREC ',SAVEDREC,C'DUMP',(R3),=C'1D'           
******** ZICM  R3,GLENGTH,2        RECORD LENGTH                                
******** GOTO1 PRNTBL,DMCB,=C'CUR REC *',(R8),C'DUMP',(R3),=C'1D'               
*                                                                               
         L     R6,ADGOAL                                                        
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   FX35                                                             
*                                                                               
* FIND THE ELEMENT IN ADGOAL AND POINT R6 TO IT                                 
* ABOUT TO INSERT ELEMENT IN SAVEDREC                                           
*                                                                               
         LA    R7,SAVEDREC+24                                                   
*                                                                               
FX30     SR    R0,R0                                                            
         ICM   R0,1,1(R7)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BNE   FX30                                                             
         GOTO1 RECUP,DMCB,(0,SAVEDREC),(R6),(R7)                                
*                                                                               
* GO BACK AND GET NEXT ELEMENT TO INSERT                                        
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    FX30                                                             
*                                                                               
FX35     DS    0H                                                               
*                                                                               
******** ZICM  R3,SAVEDREC+13,2    REC LENGTH IS 13 BYTES AFTER KEY             
******** GOTO1 PRNTBL,DMCB,=C'MERGED **',SAVEDREC,C'DUMP',(R3),=C'1D'           
*                                                                               
* PUTTING RECORD TO OUTFILE                                                     
*                                                                               
         LA    RE,SAVEDREC                                                      
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         RECORD LENGTH                                
         LA    RF,4(RF)            SET LEN FOR PUT                              
         SLL   RF,16                                                            
         SH    RE,=H'4'                                                         
         ST    RF,0(RE)                                                         
         LA    R1,FILEOUT                                                       
         LA    R0,SAVEDREC                                                      
         SH    R0,=H'4'                                                         
         PUT   (1),(0)                                                          
*                                                                               
         AP    RECPUT,=PL1'1'      # OF RECORDS PUT INTO OUTFILE                
         AP    RECMERG,=PL1'1'     # OF RECORDS MERGED                          
         MVI   PREVREC,C'N'        PREVIOUS RECORD NOW DOESN'T EXIST            
*                                                                               
         B     FX20                READ NEXT RECORD                             
*                                                                               
FX40     DS    0H                                                               
*                                                                               
         MVC   LASTKEY,KEY         SAVE OFF KEY FOR COMPARISON                  
         CLI   PREVREC,C'Y'                                                     
         BNE   FX50                                                             
*                                  PUTTING RECORD TO PRINT LINE/OUTFILE         
         LA    RE,SAVEDREC                                                      
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         RECORD LENGTH                                
         LA    RF,4(RF)            SET LEN FOR PUT                              
         SLL   RF,16                                                            
         SH    RE,=H'4'                                                         
         ST    RF,0(RE)                                                         
         LA    R1,FILEOUT                                                       
         LA    R0,SAVEDREC                                                      
         SH    R0,=H'4'                                                         
         PUT   (1),(0)                                                          
*                                                                               
         AP    RECPUT,=PL1'1'      # OF RECORDS PUT INTO OUTFILE                
*                                                                               
******** ZICM  R3,SAVEDREC+13,2    REC LENGTH IS 13 BYTES AFTER KEY             
******** GOTO1 PRNTBL,DMCB,=C'NO CHA  *',SAVEDREC,C'DUMP',(R3),=C'1D'           
*                                                                               
FX50     DS    0H                                                               
*                                                                               
         CLI   GKEYSLN,X'1E'       30 SECONDS?                                  
         BE    FX51                                                             
         CLI   GKEYSLN,X'3C'       60 SECONDS?                                  
         BE    FX52                                                             
*                                                                               
         DC    H'0'                                                             
*                                                                               
FX51     MVI   GKEYSLN,X'3C'                                                    
         MVI   GKEYSEC,X'3C'                                                    
*                                                                               
FX52     XCEF  SAVEDREC,2000       CLEARING SAVEDREC                            
*                                                                               
         ZICM  R3,GLENGTH,2        RECORD LENGTH                                
         LA    R4,SAVEDREC         USED FOR SAVING RECORD                       
*                                                                               
FX54     CH    R3,=H'256'          MAX LENGTH FOR EX INSTRUCTION IS 256         
         BL    FX55                                                             
         MVC   0(256,R4),0(R8)     MOVING 256 BYTES AT ONE TIME                 
         LA    R4,256(R4)                                                       
         LA    R8,256(R8)                                                       
         SH    R3,=H'256'                                                       
         B     FX54                                                             
*                                                                               
FX55     LTR   R3,R3                                                            
         BZ    FX56                IF ZERO, THEN RECORD HAS BEEN COPIED         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R8)       BUILDING RECORD USING EX INSTRUCTION         
*                                                                               
FX56     L     R8,ADGOAL           RESTORE WHERE R8 INITIALLY POINTS TO         
*                                                                               
         MVI   PREVREC,C'Y'        PREVIOUS RECORD NOW EXIST                    
         B     FX20                READ NEXT RECORD                             
*                                                                               
FX100    DS    0H                                                               
*                                  PUT LAST REC IN MEMORY INTO OUTFILE          
         CLI   PREVREC,C'Y'                                                     
         BNE   FX150                                                            
*                                  PUT LAST REC IN MEMORY INTO OUTFILE          
         LA    RE,SAVEDREC                                                      
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         RECORD LENGTH                                
         LA    RF,4(RF)            SET LEN FOR PUT                              
         SLL   RF,16                                                            
         SH    RE,=H'4'                                                         
         ST    RF,0(RE)                                                         
         LA    R1,FILEOUT                                                       
         LA    R0,SAVEDREC                                                      
         SH    R0,=H'4'                                                         
         PUT   (1),(0)                                                          
*                                                                               
         AP    RECPUT,=PL1'1'      # OF RECORDS PUT INTO OUTFILE                
*                                                                               
******** ZICM  R3,SAVEDREC+13,2                                                 
******** GOTO1 PRNTBL,DMCB,=C'LAST REC*',SAVEDREC,C'DUMP',(R3),=C'1D'           
*                                                                               
FX150    DS    0H                                                               
*                                                                               
         GOTO1 REPORT              BLANK LINE                                   
         MVC   P(80),=80C'*'                                                    
         GOTO1 REPORT                                                           
         MVC   P(25),=C'# OF RECORD GET        = '                              
         OI    RECOUNT+3,X'0F'                                                  
         UNPK  P+25(8),RECOUNT                                                  
         GOTO1 REPORT                                                           
         MVC   P(25),=C'# OF RECORD MERGED     = '                              
         OI    RECMERG+3,X'0F'                                                  
         UNPK  P+25(8),RECMERG                                                  
         GOTO1 REPORT                                                           
         MVC   P(25),=C'# OF RECORD ON OUTFILE = '                              
         OI    RECPUT+3,X'0F'                                                   
         UNPK  P+25(8),RECPUT                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(25),=C'# OF RECORD BAD        = '                              
         OI    RECBAD+3,X'0F'                                                   
         UNPK  P+25(8),RECBAD                                                   
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R8                  GOAL RECORD DSECT                            
         CLOSE FILEOUT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
RECBAD   DC    PL4'0'                                                           
RECPUT   DC    PL4'0'                                                           
RECMERG  DC    PL4'0'                                                           
RECOUNT  DC    PL4'0'                                                           
MAXCOUNT DC    PL4'9999999'                                                     
SEQNUM   DC    PL4'0'                                                           
LASTKEY  DC    XL13'00'            WORKING KEY, FOR RESTORING RECORDS           
PREVREC  DC    CL1'N'              PREVIOUS REC FLAG, Y/N                       
*                                                                               
ELCODE   DC    XL1'00'                                                          
         DC    XL4'00'                                                          
SAVEDREC DC    2000X'00'           STORAGE AREA FOR SAVING RECORD               
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=2004,            X        
               BLKSIZE=8000,MACRF=PM                                            
         EJECT                                                                  
       ++INCLUDE SPCBLLST                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPREPFXGL 02/26/98'                                      
         END                                                                    
