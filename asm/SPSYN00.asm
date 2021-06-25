*          DATA SET SPSYN00    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T21C00A                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE MEDGET                                                                 
         TITLE 'T21C00 - SPOTPAK SYNDICATION - BASE'                            
         PRINT NOGEN                                                            
T21C00   CSECT                                                                  
         NMOD1 600,T21C00                                                       
*                                                                               
         USING GENOLD,RC                                                        
         USING T21CFFD,RA                                                       
*                                                                               
         BAS   RE,INITL                                                         
*                                                                               
         XC    DMCB(8),DMCB        GET A(MSPACK)                                
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QMSPACK                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VMSPACK,0(R1)                                                    
*                                                                               
         XC    SYNMSG,SYNMSG       CLEAR MESSAGE AREA                           
         MVI   SYNMSGH+7,60        SET LEN TO MAX                               
         FOUT  SYNMSGH                                                          
         EJECT                                                                  
         TM    SYNPRH+4,X'20'      TEST NEW PROPERTY                            
         BZ    SB2                                                              
         TM    SYNSTH+4,X'20'      OR NEW STATION                               
         BZ    SB2                                                              
         B     EDTACTN                                                          
*                                                                               
SB2      FOUT  SYNPRNMH,SPACES,24                                               
         FOUT  SYNSTNMH,SPACES,31                                               
         NI    SYNSTH+4,X'DF'      RESET EDIT FLAGS                             
         NI    SYNPRH+4,X'DF'                                                   
         NI    SYNACTH+4,X'DF'                                                  
         NI    SYNLINH+4,X'DF'                                                  
         XC    SVDATA,SVDATA       CLEAR SAVE DATA                              
*                                                                               
*                                                                               
* EDIT PROPERTY                                                                 
         LA    R2,SYNPRH                                                        
         GOTO1 ANY                                                              
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         GOTO1 PACK                                                             
         MVI   ERRCD,INVERR                                                     
         LTR   R0,R0                                                            
         BZ    SBERR                                                            
         CH    R0,=H'999'                                                       
         BH    SBERR                                                            
         STH   R0,SVREP                                                         
*                                                                               
         EJECT                                                                  
* EDIT STATION                                                                  
*                                                                               
         LA    R2,SYNSTH                                                        
         GOTO1 ANY                                                              
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
SB5      XC    REC(64),REC         CLEAR SCANNER TABLE                          
         GOTO1 =V(SCANNER),DMCB,(R2),(2,REC),C',=,-',RR=RB                      
*                                                                               
         MVI   ERRCD,INVERR                                                     
         LA    R4,REC                                                           
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BO    SB8                                                              
         CLI   0(R4),3                                                          
         BL    SBERR                                                            
         CLI   0(R4),4                                                          
         BH    SBERR                                                            
         TM    2(R4),X'40'         TEST VALID ALPHA                             
         BZ    SBERR                                                            
         MVC   SVSTA,12(R4)                                                     
*                                                                               
         MVI   SVMED,C'T'          ASSUME TV                                    
         LA    R4,32(R4)                                                        
         CLI   0(R4),0             TEST MEDIA ENTERED                           
         BE    SB10                                                             
         CLI   0(R4),2                                                          
         BH    SBERR                                                            
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         EX    R5,*+8                                                           
         BCTR  R5,0                                                             
         B     *+10                                                             
         CLC   12(0,R4),=C'TV'                                                  
         BE    SB10                                                             
*                                                                               
         MVI   SVMED,C'R'                                                       
         MVC   SVSTA+4(1),12(R4)                                                
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'AM'                                                  
         BE    SB10                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'FM'                                                  
         BNE   SBERR                                                            
         B     SB10                                                             
         EJECT                                                                  
* NUMERIC DATA IN STATION FIELD IS MARKET NUMBER                                
* AND MAY BE FOLLOWED BY MEDIA CODE                                             
*                                                                               
SB8      L     R0,4(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB            SAVE MARKET HERE                             
         MVI   SVMED,C'T'          DEFAULT MEDIA IS TV                          
         CLI   1(R4),0             TEST MEDIA ENTERED                           
         BE    SB12                                                             
         CLI   1(R4),1                                                          
         BH    SBERR                                                            
         CLI   22(R4),C'T'                                                      
         BE    SB12                                                             
         MVI   SVMED,C'R'                                                       
         CLI   22(R4),C'R'                                                      
         BNE   SBERR                                                            
         B     SB12                                                             
         EJECT                                                                  
* READ SPECIAL REP RECORD                                                       
*                                                                               
SB10     DS    0H                                                               
         CLI   SVSTA+4,C' '                                                     
         BNE   *+8                                                              
         MVI   SVSTA+4,C'T'                                                     
SB12     LA    R2,SYNPRH           SET CURSOR IF ERROR                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),SVMED                                                   
         LH    R0,SVREP                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(3),DUB                                                     
         MVC   KEY+5(2),AGYALPHA                                                
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING REPREC,R8                                                        
         GOTO1 RDSTA                                                            
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         FOUT  SYNPRNMH,RNAME,22                                                
*                                                                               
         MVI   ERRCD,NOTSYND                                                    
         CLI   RSYND,C'S'          TEST SPECIAL REP IND                         
         BNE   SBERR                                                            
         MVC   SVGOAL,RGDEMO       SAVE TARGET AUDIENCE                         
*                                                                               
         CLI   SVSTA,0             TEST MARKET INPUT                            
         BE    SB20                YES - READ MARKET REC                        
         EJECT                                                                  
* READ STATION RECORD (FOR MARKET NUM)                                          
         LA    R2,SYNSTH           SET CURSOR IF ERROR                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVMED                                                   
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING STAREC,R8                                                        
         GOTO1 RDSTA                                                            
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         MVC   FULL,SMKT           SAVE MKT                                     
         SPACE 2                                                                
* READ MARKET RECORD                                                            
*                                                                               
SB20     MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),SVMED                                                   
         MVC   KEY+2(4),FULL       MARKET CODE SAVED HERE                       
         MVC   KEY+6(2),AGYALPHA                                                
         GOTO1 RDSTA                                                            
         CLI   ERRAREA,0                                                        
         BNE   EXIT                                                             
*                                                                               
         USING MKTREC,R8                                                        
         MVC   SYNSTNM,SPACES                                                   
         MVC   SYNSTNM(4),FULL     MARKET NUM                                   
         MVI   SYNSTNM+5,C'-'                                                   
         MVC   SYNSTNM+7(24),MKTNAME                                            
         FOUT  SYNSTNMH                                                         
*                                                                               
         MVI   ERRCD,NOSHR                                                      
         CLC   =C'000',MKTSHR                                                   
         BH    SBERR                                                            
*                                                                               
         PACK  DUB,MKTSHR                                                       
         CVB   R0,DUB                                                           
         STH   R0,SVSHR                                                         
         EJECT                                                                  
* GET AGENCY MEDIA BYTE VALUE                                                   
         GOTO1 =V(MEDGET),DMCB,(SVMED,AGYALPHA),VDATAMGR,WORK,RR=RB             
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
* BUILD COVERAGE RECORD KEY                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,3               TYPE                                         
         MVC   KEY+1(2),SVREP      REP                                          
         NI    WORK,X'F0'          DROP MEDIA BITS                              
         OC    KEY+1(1),WORK       'OR' IN AGY BITS                             
* PACK MKT/STA                                                                  
         GOTO1 VMSPACK,DMCB,FULL,SVSTA,KEY+3                                    
*                                                                               
         CLI   SVSTA,0             TEST MARKET INPUT                            
         BNE   SB30X               NO                                           
* FOR MARKET INPUT, MUST SET MSPACK MEDIA CODE IN LAST BYTE OF STATN            
         XC    KEY+5(3),KEY+5      CLEAR GARBAGE FROM MSPACK                    
         MVI   BYTE,X'02'                                                       
         CLI   SVMED,C'T'                                                       
         BE    *+8                                                              
         MVI   BYTE,X'01'                                                       
         OC    SVSTA+2(1),BYTE                                                  
         MVC   KEY+5(3),SVSTA                                                   
SB30X    DC    0H'0'                                                            
         MVC   SVKEY,KEY                                                        
         SPACE 2                                                                
         OI    SYNPRH+4,X'20'      SET 'EDITED' FLAGS                           
         OI    SYNSTH+4,X'20'                                                   
*                                                                               
         B     EDTACTN                                                          
         SPACE 2                                                                
         EJECT                                                                  
EDTACTN  LA    R2,SYNACTH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTACT5                                                          
         CLI   SVACT,C'D'          WAS LAST ACTION DISPLAY                      
         BNE   EDTACT2             NO                                           
         BAS   RE,GETACTN                                                       
         CLI   SVACT,C'C'          IS NEW ACTION CHANGE                         
         BE    EDTACT5                                                          
         B     EDTACT4                                                          
*                                                                               
* OLD ACTION NOT DISPLAY                                                        
*                                                                               
EDTACT2  CLI   SVACT,C'A'          WAS LAST ACTION ADD                          
         BNE   EDTACT4                                                          
         BAS   RE,GETACTN                                                       
         CLI   SVACT,C'C'          NEW ACTION CHANGE                            
         BE    EDTACT5                                                          
*                                                                               
* NEW AND OLD ACTIONS NOT RELATED                                               
*                                                                               
EDTACT4  NI    SYNLINH+4,X'DF'      INDICATE LINE NOT EDITED                    
         MVI   SVLIN,0                                                          
         BAS   RE,GETACTN          EDIT ACTION                                  
         EJECT                                                                  
* CHECK RIGHT SCREEN                                                            
EDTACT5  MVI   BYTE,X'FC'          T21CFC = DSM SCREEN                          
         CLI   SVACT,C'M'                                                       
         BE    EDTACT6                                                          
         MVI   BYTE,X'FD'          T21CFD = ADD/CHA BAL/FWD                     
         CLC   =C'BF',SYNLIN                                                    
         BE    EDTACT6                                                          
         MVI   BYTE,X'FE'          T21CFE = ADD/CHA LIN                         
EDTACT6  DS    0H                                                               
         CLC   SVSCRN,BYTE                                                      
         BE    EDTLIN                                                           
*                                                                               
         NI    SYNLINH+4,X'DF'     FORCE EDIT OF LINE                           
         MVC   SVSCRN,BYTE                                                      
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB+4,C'R'                                                      
         MVC   DMCB+5(2),=X'021C'                                               
         MVC   DMCB+7(1),SVSCRN                                                 
*                                                                               
         GOTO1 VCALLOV,DMCB,SYNORGH                                             
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDTLIN                                                           
*                                                                               
         MVC   SYNMSG(17),=C'ENTER RECORD DATA'                                 
         LA    R2,SYNORGH          POINT TO FIRST UNP FIELD                     
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
GETACTN  MVI   ERRCD,INVERR                                                     
         CLI   5(R2),3                                                          
         BH    SBERR                                                            
*                                                                               
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
*                                                                               
         MVI   SVACT,C'A'                                                       
         EX    R3,TSTADD                                                        
         BE    GETACTX                                                          
         MVI   SVACT,C'D'                                                       
         EX    R3,TSTDIS                                                        
         BE    GETACTX                                                          
         MVI   SVACT,C'C'                                                       
         EX    R3,TSTCHA                                                        
         BE    GETACTX                                                          
         MVI   SVACT,C'M'                                                       
         EX    R3,TSTDSM                                                        
         BE    GETACTX                                                          
         B     SBERR                                                            
*                                                                               
GETACTX  OI    4(R2),X'20'         SET VALIDATED                                
         BR    RE                                                               
*                                                                               
TSTADD   CLC   8(0,R2),=C'ADD'                                                  
TSTDIS   CLC   8(0,R2),=C'DIS'                                                  
TSTCHA   CLC   8(0,R2),=C'CHA'                                                  
TSTDSM   CLC   8(0,R2),=C'DSM'                                                  
         EJECT                                                                  
* NOTE THAT 'EDITED' BIT IS SET BY                                              
* DISPLAY OVERLAY - NOT HERE                                                    
*                                                                               
EDTLIN   LA    R2,SYNLINH                                                       
         TM    4(R2),X'20'                                                      
         BO    GETOVLY                                                          
*                                                                               
         CLI   SVACT,C'M'                                                       
         BNE   EDTLN1                                                           
         MVI   ERRCD,INVERR                                                     
         CLC   =C'BF',8(R2)                                                     
         BE    SBERR                                                            
*                                                                               
EDTLN1   MVI   SVLIN,0                                                          
         CLC   =C'BF',8(R2)        CHECK BAL/FWD                                
         BE    GETOVLY                                                          
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDTLN2                                                           
         MVC   SYNLIN,SPACES       BLANK LINE FOR ADDS                          
         FOUT  (R2)                                                             
         B     GETOVLY                                                          
*                                                                               
EDTLN2   CLI   5(R2),0                                                          
         BNE   EDTLN4                                                           
         MVI   SYNLIN,C'1'         SET FIELD TO 1                               
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    4(R2),X'08'         SET VALID NUM                                
         FOUT  (R2)                                                             
*                                                                               
EDTLN4   GOTO1 PACK                                                             
         MVI   ERRCD,INVERR                                                     
         LTR   R0,R0                                                            
         BZ    SBERR                                                            
         STC   R0,SVLIN                                                         
         EJECT                                                                  
GETOVLY  DS    0H                                                               
         CLI   SVACT,C'A'          ADDS ALWAYS DO EDIT                          
         BE    GETOV2                                                           
         TM    SYNLINH+4,X'20'      ELSE TEST LINE EDITED                       
         BZ    GETOV4              NO-DO DISPLAY                                
*                                                                               
* FETCH EDIT OVERLAY                                                            
*                                                                               
GETOV2   GOTO1 VCALLOV,DMCB,(1,0),VTWA                                          
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA)                                              
*                                                                               
         CLI   ERRAREA,0                                                        
         BE    GETOV3                                                           
         CLI   SVACT,C'A'          IF CURRENT ACTION IS ADD                     
         BNE   *+8                   MAKE SURE LINE GETS EDITED                 
         NI    SYNLINH+4,X'DF'                                                  
         B     EXXMOD                                                           
*                                                                               
GETOV3   CLI   SVACT,C'M'          SEE IF ACTION NOW DSM                        
         BNE   GETOV4                                                           
* NEED DSM SCREEN                                                               
         MVC   DMCB+4(4),=X'D9021CFC'                                           
         GOTO1 VCALLOV,DMCB,SYNORGH                                             
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SVSCRN,X'FC'                                                     
*                                                                               
* FETCH DISPLAY OVERLAY                                                         
*                                                                               
GETOV4   GOTO1 VCALLOV,DMCB,(2,0),VTWA                                          
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA)                                              
*                                                                               
         B     EXXMOD                                                           
         SPACE 2                                                                
SBERR    GOTO1 ERROR                                                            
         B     EXXMOD                                                           
*                                                                               
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* SPGENEROL                                                                     
       ++INCLUDE SPGENEROL                                                      
         LTORG                                                                  
         EJECT                                                                  
* SPSYNWRK                                                                      
       ++INCLUDE SPSYNWRK                                                       
         EJECT                                                                  
RECD     DSECT                                                                  
* SPGENREP                                                                      
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
         ORG   RECD                                                             
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         SPACE 2                                                                
         ORG   RECD                                                             
* SPGENMKT                                                                      
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
 END                                                                            
