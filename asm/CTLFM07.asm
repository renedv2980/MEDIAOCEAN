*          DATA SET CTLFM07    AT LEVEL 005 AS OF 05/01/02                      
*PHASE TA0207A                                                                  
         TITLE 'CTLFM07 - CONTROL FILE MAINT - OUTPUT RECORDS'                  
CTLFM07  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**LFM7**                                                     
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTOREC,R4                                                        
         EJECT                                                                  
KEYVAL   XC    CTOKEY,CTOKEY       BUILD A KEY                                  
         MVI   CTOKEY,C'O'                                                      
         SPACE 2                                                                
         LA    R1,OUTIDH                                                        
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         CLI   FLDH+5,1            MINIMUM LENGTH                               
         BL    EFTS                                                             
         CLI   FLD,C'@'            RESERVED FOR REPORT REFORM ID                
         BE    EIIF                                                             
         MVC   CTOKID,FLD                                                       
         MVC   KEY,CTOKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE                                                      
         BNE   *+18                                                             
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY        SET ACTN TO DISP IF KEY CHANGED              
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST N/F                                     
         BZ    KEYV2                                                            
         CLI   ACTN,ADD            N/F ONLY VALID FOR ADD                       
         BE    DATAVAL                                                          
         B     ERNF                                                             
*                                                                               
KEYV2    CLI   ACTN,ADD            CANT EXIST FOR ADD                           
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        DELETED RECORD CAN ONLY BE DISP              
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         EJECT                                                                  
*DISPLAY RECORD                                                                 
*                                                                               
         TWAXC OUTTYPEH                                                         
         LA    R5,CTODATA                                                       
         SR    R6,R6                                                            
DISP2    CLI   0(R5),0             END OF RECORD                                
         BE    DISPEND                                                          
         CLI   0(R5),X'38'         OUTPUT DETAIL                                
         BE    DISP4                                                            
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISP2                                                            
DISP4    DS    0H                                                               
         USING CTOUTD,R5                                                        
         MVC   OUTCLAS,CTOUTCLS    JES OUTPUT CLASS                             
         MVC   OUTPRTY,CTOUTPRI    PRIORITY                                     
         MVC   OUTPWRC,CTOUTPOW    FORM NO                                      
         MVC   OUTTAPE,CTOUTCC     CC TAPE                                      
         MVC   OUTCPYS,CTOUTCPY    COPIES                                       
         MVC   OUTSTAT,CTOUTDIS    DISPOSITION                                  
         MVC   OUTPWRD,CTOUTSEP    JES JOB SEPARATORS                           
         MVC   OUTTDET,CTOUTTAP    TAPE DETAILS                                 
         MVI   OUTUREQ,C'N'        REQUESTABLE                                  
*                                                                               
         MVC   OUTTYPE,=C'PRINT'   DESTINATION                                  
         CLI   CTOUTCLS,C'Q'                                                    
         BNE   *+10                                                             
         MVC   OUTTYPE,=C'QUEUE'                                                
*                                                                               
DISP5    CLI   CTOUTLEN,22         TEST VERSION 1 ELEMENT                       
         BE    DISPEND                                                          
         TM    CTOUTSTA,X'80'                                                   
         BZ    *+8                                                              
         MVI   OUTUREQ,C'Y'        REQUSTABLE                                   
         MVC   OUTCHRS,CTOUTCHR    CHARACTER SET                                
         LA    R1,TYPETAB                                                       
DISP6    CLC   0(1,R1),CTOUTTYP                                                 
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     DISP6                                                            
         MVC   OUTTYPE,1(R1)                                                    
*                                                                               
DISP7    CLI   CTOUTLEN,32         TEST VERSION 2 ELEMENT                       
         BE    DISPEND                                                          
         MVC   OUTFCH,CTOUTFCH     FICHE CLASS                                  
         MVC   OUTCHR2,CTOUTCH2    CHARS#2                                      
         MVC   OUTCHR3,CTOUTCH3    CHARS#3                                      
         MVC   OUTCHR4,CTOUTCH4    CHARS#4                                      
         SR    R0,R0                                                            
         ICM   R0,1,CTOUTFMN       NUMBER OF FORM DEFINITIONS                   
         BZ    DISPEND                                                          
         EDIT  (R0),(3,OUTFMN),ALIGN=LEFT                                       
         MVC   OUTFMD,CTOUTFMD     FORM DEFINITION                              
         SR    R0,R0                                                            
         ICM   R0,1,CTOUTPGN       NUMBER OF PAGE DEFINITIONS                   
         BZ    DISPEND                                                          
         EDIT  (R0),(3,OUTPGN),ALIGN=LEFT                                       
         MVC   OUTPGD,CTOUTPGD     PAGE DEFINITION                              
*                                                                               
DISPEND  TM    CTOSTAT,X'80'                                                    
         BO    DISPEN2                                                          
         MVI   NACTN,OKCHA+OKDEL                                                
         B     DISPXX                                                           
DISPEN2  MVI   NACTN,OKRES                                                      
*                                                                               
DISPXX   LA    R1,OUTTYPEH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*ADD/UPDATE                                                                     
*                                                                               
DATAVAL  CLI   ACTN,CHANGE         ADD                                          
         BE    DATAV2                                                           
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC                                                          
         B     DATAV4                                                           
DATAV2   MVI   TEMP,X'01'          CHANGE                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'38'                                                       
         GOTO1 ADELEL                                                           
DATAV4   GOTO1 ABLDACT             BUILD ACTIVITY ELEMENT                       
         GOTO1 APUTEL              AND ADD IT                                   
         BZ    EXIT                                                             
         LA    R5,TEMP                                                          
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'3840'                                                 
         LA    R1,OUTTYPEH         OUTPUT TYPE                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         LA    R1,TYPETAB                                                       
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
DATAV4A  CLI   0(R1),0                                                          
         BE    EIIF                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R1),FLD                                                      
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     DATAV4A                                                          
         MVC   CTOUTTYP,0(R1)                                                   
*                                                                               
         LA    R1,OUTCLASH         JES OUTPUT CLASS                             
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         LA    R7,TABCLA                                                        
         BAS   RE,DATAVT                                                        
         BNE   EXIT                                                             
         MVC   CTOUTCLS,FLD                                                     
*                                                                               
         LA    R1,OUTPRTYH         JES PRIORITY                                 
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVC   CTOUTPRI,FLD                                                     
*                                                                               
         LA    R1,OUTPWRCH         POWER CODE                                   
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVC   CTOUTPOW,FLD                                                     
*                                                                               
         LA    R1,OUTTAPEH         CC/TAPE                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVC   CTOUTCC,FLD                                                      
*                                                                               
         LA    R1,OUTCPYSH         COPIES                                       
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVI   WORK,C'0'                                                        
         MVZ   WORK(1),FLD                                                      
         CLI   WORK,C'0'                                                        
         BNE   EFNN                                                             
         MVC   CTOUTCPY,OUTCPYS                                                 
*                                                                               
         LA    R1,OUTSTATH         DISPOSITION                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         LA    R7,TABSTA                                                        
         BAS   RE,DATAVT                                                        
         BNE   EXIT                                                             
         MVC   CTOUTDIS,OUTSTAT                                                 
*                                                                               
         LA    R1,OUTPWRDH         JES JOB SEPARATORS                           
         MVI   CTOUTSEP,C'Y'                                                    
         GOTO1 AFVAL                                                            
         BZ    *+10                                                             
         MVC   CTOUTSEP,FLD                                                     
         CLI   CTOUTSEP,C'Y'                                                    
         BE    *+12                                                             
         CLI   CTOUTSEP,C'N'                                                    
         BNE   EIIF                                                             
*                                                                               
         LA    R1,OUTTDETH                                                      
         GOTO1 AFVAL                                                            
         MVC   CTOUTTAP,FLD        TAPE DETAILS                                 
*                                                                               
         LA    R1,OUTUREQH         USER REQUESTABLE                             
         GOTO1 AFVAL                                                            
         BZ    DATAV4B                                                          
         CLI   OUTUREQ,C'N'                                                     
         BE    DATAV4B                                                          
         CLI   OUTUREQ,C'Y'                                                     
         BNE   EIIF                                                             
         OI    CTOUTSTA,X'80'                                                   
*                                                                               
DATAV4B  LA    R1,OUTFCHH          FICHE CLASS                                  
         GOTO1 AFVAL                                                            
         BZ    DATAV5                                                           
         CLI   FLDH+5,1                                                         
         BL    EFTS                                                             
         MVC   CTOUTFCH,FLD                                                     
*                                                                               
DATAV5   LA    R1,OUTCHRSH         3800 CHARS                                   
         GOTO1 AFVAL                                                            
         BZ    DATAV5B                                                          
         CLI   FLDH+5,4                                                         
         BL    EFTS                                                             
         MVC   CTOUTCHR,FLD                                                     
DATAV5B  LA    R1,OUTCHR2H         CHARS#2                                      
         GOTO1 AFVAL                                                            
         BZ    DATAV5C                                                          
         CLI   FLDH+5,4                                                         
         BL    EFTS                                                             
         MVC   CTOUTCH2,FLD                                                     
DATAV5C  LA    R1,OUTCHR3H         CHARS#3                                      
         GOTO1 AFVAL                                                            
         BZ    DATAV5D                                                          
         CLI   FLDH+5,4                                                         
         BL    EFTS                                                             
         MVC   CTOUTCH3,FLD                                                     
DATAV5D  LA    R1,OUTCHR4H         CHARS#4                                      
         GOTO1 AFVAL                                                            
         BZ    DATAV6                                                           
         CLI   FLDH+5,4                                                         
         BL    EFTS                                                             
         MVC   CTOUTCH4,FLD                                                     
*                                                                               
DATAV6   LA    R1,OUTFMNH          NUMBER OF FORM DEFINITIONS                   
         GOTO1 AFVAL                                                            
         BZ    DATAV7                                                           
         TM    FLDH+4,X'08'        MUST BE NUMERIC                              
         BZ    EFNN                                                             
         ICM   R0,15,FLDH          VALUE 1-35                                   
         BZ    EFLM                                                             
         CH    R0,=H'35'                                                        
         BH    EFTB                                                             
         STC   R0,CTOUTFMN                                                      
*                                                                               
DATAV7   LA    R1,OUTFMDH          FORM DEFINITION                              
         GOTO1 AFVAL                                                            
         BNZ   DATAV7A                                                          
         CLI   CTOUTFMN,0          REQUIRED IF NUMBER INPUT                     
         BE    DATAV8                                                           
         B     EMIF                                                             
DATAV7A  CLI   CTOUTFMN,0          INVALID IF NUMBER ISNT INPUT                 
         BE    EIIF                                                             
         CLI   FLDH+5,5            MUST BE 5 BYTES LONG                         
         BL    EFTS                                                             
         BH    EFTB                                                             
         MVC   CTOUTFMD,FLD                                                     
*                                                                               
DATAV8   LA    R1,OUTPGNH          NUMBER OF PAGE DEFINITIONS                   
         GOTO1 AFVAL                                                            
         BNZ   DATAV8A                                                          
         CLI   CTOUTFMN,0          MUST BE INPUT IF FORMS IS                    
         BNE   EMIF                                                             
         B     DATAV9                                                           
DATAV8A  TM    FLDH+4,X'08'        MUST BE NUMERIC                              
         BZ    EFNN                                                             
         ICM   R0,15,FLDH          VALUE 1-35                                   
         BZ    EFLM                                                             
         CH    R0,=H'35'                                                        
         BH    EFTB                                                             
         STC   R0,CTOUTPGN                                                      
DATAV8B  CLC   CTOUTPGN,CTOUTFMN   TEST NUM OF PAGEDEFS TO FORMDEFS             
         BE    DATAV9                                                           
         BH    EFTB                CANT BE HIGHER                               
         CLI   CTOUTPGN,1          MUST BE ONE IF LOWER                         
         BNE   EIIF                                                             
*                                                                               
DATAV9   LA    R1,OUTPGDH          PAGE DEFINITION                              
         GOTO1 AFVAL                                                            
         BNZ   DATAV9A                                                          
         CLI   CTOUTPGN,0          REQUIRED IF NUMBER INPUT                     
         BE    DATAVA                                                           
         B     EMIF                                                             
DATAV9A  CLI   CTOUTPGN,0          INVALID UNLESS NUMBER INPUT                  
         BE    EIIF                                                             
         CLI   FLDH+5,5            MUST BE FIVE BYTES LONG                      
         BL    EFTS                                                             
         BH    EFTB                                                             
         MVC   CTOUTPGD,FLD                                                     
*                                                                               
DATAVA   DS    0H                                                               
         GOTO1 APUTEL              ADD ELEMENT                                  
         BZ    EXIT                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVB                                                           
         GOTO1 AADD                ADD RECORD                                   
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     DATAVX                                                           
*                                                                               
DATAVB   GOTO1 AWRITE              UPDATE RECORD                                
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
DATAVX   MVI   FERN,X'FF'                                                       
         MVI   NACTN,OKCHA+OKDEL                                                
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
DATAVT   MVI   FERN,2                                                           
DATAVT2  CLI   0(R7),X'FF'                                                      
         BE    DATAVTX                                                          
         CLC   0(1,R7),FLD                                                      
         BE    *+12                                                             
         LA    R7,1(R7)                                                         
         B     DATAVT2                                                          
         MVI   FERN,X'FF'                                                       
DATAVTX  CLI   FERN,X'FF'                                                       
         BR    RE                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
TABCLA   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'FF'                    
*                                                                               
TABSTA   DC    C'DHKLINT',X'FF'                                                 
*                                                                               
TYPETAB  DS    0CL6                                                             
         DC    C'PPRINT'                                                        
         DC    C'QQUEUE'                                                        
         DC    C'BBOTH '                                                        
         DC    X'00'                                                            
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF8D                                                                      
       ++INCLUDE CTLFMF8D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTLFM07   05/01/02'                                      
         END                                                                    
