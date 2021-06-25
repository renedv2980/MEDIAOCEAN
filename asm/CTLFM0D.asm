*          DATA SET CTLFM0D    AT LEVEL 004 AS OF 05/01/02                      
*PHASE TA020DA                                                                  
         TITLE 'CTLFM0D - CONTROL FILE MAINT - TEINFO RECORDS'                  
CTLFM0D  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**LFMD*                                                
         USING WRKD,RC             RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTTREC,R4           R4=A(IO)                                     
         L     R5,PARM+12          R5=A(COMFACS)                                
         USING COMFACSD,R5                                                      
         MVC   ATERMVAL,CTERMVAL                                                
         EJECT                                                                  
* VALIDATE KEY FIELDS AND BUILD KEY                                             
*                                                                               
KEYVAL   XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKEY,C'T'                                                      
* VALIDATE TERMINAL-ID - CAN EITHER BE - UTLNUM/LINEADDR                        
*                                                                               
         LA    R1,TRMIDH                                                        
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'        NUMERIC-ASSUME EXISTING UTL NUM              
         BO    KEYV2                                                            
         CLI   FLDH+5,4            MORE THAT 4 CHRS ASSUME LINEADDR             
         BH    KEYV3                                                            
         B     EFTS                                                             
*                                                                               
KEYV2    GOTO1 ATERMVAL,DMCB,FLDH  VALIDATE NUMERIC TERMINAL NUMBER             
         L     R5,DMCB+4                                                        
         LTR   R5,R5               ERROR IF UTL NUMBER NOT FOUND                
         BZ    EIIF                                                             
         L     RE,DMCB             POINT TO RETURN LLLLCUDV                     
         MVC   CTTKLINE(8),0(RE)                                                
         XC    TRMID,TRMID                                                      
         MVC   TRMID(8),CTTKLINE   ECHO BACK FULL TERMINAL LINEADDR             
         MVI   TRMIDH+5,8                                                       
         MVI   TRMIDH+7,8                                                       
         OI    TRMIDH+6,X'80'                                                   
         B     KEYVA                                                            
*                                                                               
KEYV3    GOTO1 ATERMVAL,DMCB,FLDH  VALIDATE LINEADDR TERMINAL ID                
         CLI   DMCB,0                                                           
         BNE   EIIF                EXIT IF INVALID FORMAT                       
         L     R5,DMCB+4                                                        
         LTR   R5,R5               TEST IF TERMINAL IS IN UTL                   
         NOP   EIIF                *NOP* BZ TO IGNORE NOT IN UTL                
         L     RE,DMCB             POINT TO RETURN LLLLCUDV                     
         MVC   CTTKLINE(8),0(RE)                                                
         XC    TRMID,TRMID                                                      
         MVC   TRMID(8),CTTKLINE   ECHO BACK FULL TERMINAL LINEADDR             
         MVI   TRMIDH+5,8                                                       
         MVI   TRMIDH+7,8                                                       
         OI    TRMIDH+6,X'80'                                                   
         B     KEYVA                                                            
         SPACE 1                                                                
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
KEYVA    DS    0H                                                               
KEYVE    MVC   KEY,CTTKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,TRMIDH                                                        
         ST    R1,FADR                                                          
         CLI   ACTN,CHANGE                                                      
         BNE   KEYVG                                                            
         MVI   ACTN,DISPLAY                                                     
         CLC   KEY,LKEY            KEY AND SYSTEM MUST NOT CHANGE               
         BNE   KEYVG                                                            
         MVI   ACTN,CHANGE         IF KEYS ARE THE SAME RESET ACTION            
*                                                                               
KEYVG    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        CHECK RECORD FOUND                           
         BO    ERNF                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        DELETED REC CAN ONLY BE DISPLAYED            
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY TERMINAL INFO                                            
*                                                                               
DISPREC  LA    R1,TRMVENDH         CLEAR TWA                                    
         BAS   RE,CLEAR                                                         
         XC    TRMDATE,TRMDATE                                                  
         OI    TRMDATEH+6,X'80'                                                 
         LA    R5,CTTDATA                                                       
*                                                                               
DISPRE2  CLI   0(R5),0                                                          
         BE    DISPEND                                                          
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    DISPDESC                                                         
         CLI   0(R5),X'27'         TERMINAL INFO ELEMENT                        
         BE    DISPINFO                                                         
*                                                                               
DISPRE4  SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISPRE2                                                          
         EJECT                                                                  
*              DISPLAY DESCRIPTION ELEMENT                                      
*                                                                               
DISPDESC SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         SH    R6,=H'2'                                                         
         LA    R7,2(R5)                                                         
         LA    R1,TRMDSCAH                                                      
         CH    R6,=H'60'                                                        
         BNH   DISPDES2                                                         
         MVC   8(60,R1),0(R7)                                                   
         LA    R7,60(R7)                                                        
         SH    R6,=H'60'                                                        
         LA    R1,TRMDSCBH                                                      
*                                                                               
DISPDES2 BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),0(R7)                                                    
         B     DISPRE4                                                          
*                                                                               
*              DISPLAY TERMINAL INFO ELEMENT                                    
*                                                                               
DISPINFO DS    0H                                                               
         USING CTTIND,R5                                                        
         MVC   TRMSENO,CTTINSER                                                 
         OC    CTTINDAT,CTTINDAT                                                
         BZ    DISPINF2                                                         
         MVC   TRMDATE(15),=C'LAST CHANGED ON'                                  
         GOTO1 VDATCON,DMCB,(3,CTTINDAT),(8,TRMDATE+16)                         
*                                                                               
DISPINF2 SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         SH    R6,=H'21'                                                        
         BM    DISPRE4                                                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   TRMVEND(0),CTTINVEN                                              
         B     DISPRE4                                                          
*                                                                               
*                                                                               
DISPEND  TM    CTTSTAT,X'80'       SET NEXT ACTION & EXIT                       
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,TRMVENDH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKCHA                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              CHANGE TERMINAL RECORD                                           
*                                                                               
DATAVAL  LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
         XC    OLDSER,OLDSER                                                    
         XC    OLDDAT,OLDDAT                                                    
*                                                                               
DATAV2   CLI   0(R5),0                                                          
         BE    DATAV4                                                           
         CLI   0(R5),X'03'                                                      
         BNE   *+10                                                             
         MVC   TERMNUM,2(R5)       SAVE TERMINAL NUMBER                         
         CLI   0(R5),X'25'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAV2                                                           
*                                                                               
         USING CTTIND,R5                                                        
         MVC   OLDSER,CTTINSER     SAVE OLD SERIAL NUMBER                       
         MVC   OLDDAT,CTTINDAT                                                  
*                                                                               
*                                  DELETE ELEMENTS THAT MAY CHANGE              
DATAV4   MVI   TEMP,X'01'          ACTIVITY                                     
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'02'          DESCRIPTION                                  
         BASR  RE,RF                                                            
         MVI   TEMP,X'27'          INFO                                         
         BASR  RE,RF                                                            
         GOTO1 ABLDACT             BUILD ACTIVITY                               
         MVC   SAVEACT,TEMP                                                     
*                                  BUILD INFO ELEMENT                           
         XC    TEMP,TEMP                                                        
         LA    R5,TEMP                                                          
         USING CTTIND,R5                                                        
         MVC   TEMP(2),=X'2714'                                                 
         LA    R1,TRMVENDH                                                      
         GOTO1 AFVAL                                                            
         BZ    DATAV6                                                           
         SR    R6,R6                                                            
         IC    R6,FLDH+5                                                        
         LA    R6,20(R6)                                                        
         STC   R6,1(R5)                                                         
         MVC   CTTINVEN,FLD                                                     
*                                                                               
DATAV6   LA    R1,TRMSENOH                                                      
         GOTO1 AFVAL                                                            
         BZ    DATAV8                                                           
         MVC   CTTINDAT,OLDDAT                                                  
         MVC   CTTINSER,FLD                                                     
         CLC   OLDSER,FLD          IF SERIAL NUMBER HAS CHANGED                 
         BE    *+10                                                             
         MVC   CTTINDAT,SAVEACT+2  POP-IN TODAYS DATE                           
*                                                                               
DATAV8   OC    CTTINSER(20),CTTINSER                                            
         BZ    DATAVA                                                           
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
DATAVA   LA    R1,TRMDSCAH         BUILD DSECRIPTION ELEMENT                    
         GOTO1 AFVAL                                                            
         BZ    DATAVE                                                           
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'0202'                                                 
         SR    R6,R6                                                            
         IC    R6,FLDH+5                                                        
         MVC   2(60,R5),FLD                                                     
         LA    R1,TRMDSCBH                                                      
         GOTO1 AFVAL                                                            
         BZ    DATAVC                                                           
         IC    R6,FLDH+5                                                        
         LA    R6,60(R6)                                                        
         MVC   62(60,R5),FLD                                                    
*                                                                               
DATAVC   LA    R6,2(R6)                                                         
         STC   R6,1(R5)                                                         
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
DATAVE   EQU   *                                                                
         MVC   TEMP,SAVEACT        ADD ACTIVITY ELEMENT                         
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
DATAEND  LA    R1,TRMIDH           POSN TO 1ST KEY FLD & SET OK                 
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         MVC   KEYSAVE,KEY         WRITE TERM REC                               
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         XC    CTTKEY,CTTKEY       BUILD & WRITE POINTER REC                    
         MVI   CTTKEY,C'T'                                                      
         MVC   CTTKPASS+8(2),TERMNUM                                            
         MVI   TEMP,X'03'                                                       
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'0314'                                                 
         MVC   TEMP+2(18),KEYSAVE+7                                             
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEY,CTTKEY                                                       
         LA    R5,REC              DO RFU ON PASSIVE TERM RECORD                
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         ST    R4,AREC                                                          
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKCHA                                                      
         OC    AUTL,AUTL           CANT DELETE IF IN UTL                        
         BNZ   *+8                                                              
         OI    NACTN,OKDEL                                                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMCODE                                                                     
       ++INCLUDE CTLFMCODE                                                      
         EJECT                                                                  
*              LITERALS                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WRKD     DSECT                                                                  
AUTLX    DS    F                                                                
ATERMVAL DS    A                                                                
TERMNUM  DS    H                                                                
OLDSER   DS    CL15                                                             
OLDDAT   DS    CL3                                                              
SAVEACT  DS    CL5                                                              
REC      DS    1000C                                                            
WRKX     EQU   *                                                                
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF2D                                                                      
       ++INCLUDE CTLFMF2D                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTLFM0D   05/01/02'                                      
         END                                                                    
