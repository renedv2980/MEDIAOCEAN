*          DATA SET CTCONVERR  AT LEVEL 025 AS OF 09/15/87                      
*PHASE CONVERR,*                                                                
         TITLE 'CONVERT CONTROL FILE ERROR MESSAGES TO GENFIL'                  
CONVERR  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CONVER                                                       
         USING CONWORKD,R6         R6=A(GLOBAL W/S)                             
         L     R2,AIOAREA          R2=A(INPUT RECORD)                           
         LA    R5,4(R2)            R5=A(KEY)                                    
         L     RA,VCPRINT                                                       
         USING DPRINT,RA           RA=A(PRINT CSECT)                            
         CLI   OVSWITCH,0                                                       
         BE    CONVF                                                            
         CLI   OVSWITCH,1                                                       
         BE    CONVP                                                            
         CLI   OVSWITCH,X'FF'                                                   
         BE    CONVL                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* FIRST TIME CODE                                                               
*                                                                               
CONVF    DS    0H                                                               
         MVI   OVSWITCH,1          SET PROCESS NEXT TIME                        
         LA    R2,GENTAPE                                                       
         OPEN  ((2),OUTPUT)                                                     
         B     EXIT                                                             
         EJECT                                                                  
* PROCESS A RECORD                                                              
*                                                                               
CONVP    DS    0H                                                               
         AP    RECCOUNT,=P'1'                                                   
         USING CTEREC,R5                                                        
         CLI   CTEKTYP,C'E'                                                     
         BNE   EXIT                                                             
         TM    CTESTAT,X'80'                                                    
         BO    EXIT                                                             
*                                                                               
         LA    R0,TAPEBUF          CLEAR TAPE BUFFER                            
         L     R1,=AL4(L'TAPEBUF)                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R3,TAPEBUF          DEFINE NEW RECORD FORMAT                     
         USING GMSGD,R3                                                         
         MVI   GMKREC,GMKRECQ      REC TYPE                                     
         MVC   GMKSYS,CTEKSYS      SYSTEM                                       
         MVI   GMKTYP,GMKTERR      MESSAGE TYPE                                 
         MVC   GMKMSG+1(1),CTEKNUM ERR NUMBER                                   
         MVI   GMKLANG,X'FF'       DEFAULT LANGUAGE                             
         MVI   GMSGEL,GMSGELC      ELEMENT CODE                                 
         ZIC   R1,CTEDATA+1        ELEMENT LENGTH                               
         SH    R1,=H'3'                                                         
         LA    RF,CTEDATA+2                                                     
         LR    RE,RF                                                            
         LA    R0,6                                                             
         CLC   0(5,RF),=C'ERROR'                                                
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         B     *+16                                                             
         AR    R1,R0                                                            
         SH    R1,=H'11'                                                        
         LA    RF,5(RF)                                                         
         LR    RE,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMSGTXT(0),0(RE)                                                 
         LA    RF,GMSGTXT(R1)                                                   
         LR    R0,R1                                                            
CNV05    CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         CLI   0(RF),C'*'                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         BCT   R0,CNV05                                                         
         LA    R0,CTEDATA+2                                                     
         CR    RE,R0                                                            
         BNH   CNV30                                                            
         AP    FNDERR,=P'1'                                                     
*                                                                               
CNV10    CLI   GMSGTXT,C' '                                                     
         BE    CNV20                                                            
         CLI   GMSGTXT,C'*'                                                     
         BE    CNV20                                                            
         CLI   GMSGTXT,C'0'                                                     
         BL    CNV40                                                            
CNV20    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMSGTXT(0),GMSGTXT+1                                             
         BCT   R1,CNV10                                                         
         DC    H'0'                ODD ERROR MESSAGE 'ERROR 999'                
CNV30    AP    NOTFND,=P'1'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),GMSGTXT                                                  
         MVC   P(9),=C'NONSTD - '                                               
         GOTO1 VPRINTER                                                         
*                                                                               
CNV40    EX    R1,*+8              R1=EX LEN OF MSG-1                           
         B     *+10                                                             
         TR    GMSGTXT+1(0),TRTAB  TRANSLATE 2ND CHR ONWARD TO L/CASE           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),GMSGTXT                                                  
*                                                                               
         LA    R1,4(R1)            FULL ELEM=3FXD+TXT                           
         STC   R1,GMSGELL          ELEMENT LENGTH                               
         LA    RE,GMSGEL(R1)                                                    
         MVI   0(RE),0                                                          
         LA    R1,GMFIRST+1(R1)                                                 
         STCM  R1,3,GMFLEN                                                      
         LA    R1,4(R1)                                                         
         SLL   R1,16                                                            
         ST    R1,TAPELEN                                                       
*                                                                               
         CLC   GMKSYS,LSYS                                                      
         BE    *+16                                                             
         MVC   LSYS,GMKSYS                                                      
         ZAP   LINE,=P'99'                                                      
         MVC   P(9),=C'NEWMSG - '                                               
         XOUT  GMKSYS,P+9,1                                                     
         MVI   P+11,C'/'                                                        
         SR    R0,R0                                                            
         ICM   R0,3,GMKMSG                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+12(4),DUB                                                      
         GOTO1 VPRINTER                                                         
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* APPLY ALL U.K. FUDGES                                               *         
***********************************************************************         
*                                                                               
FUDGE01  CLI   GMKSYS,5            MOVE GENCON MSGS TO SYSTEM ZERO              
         BNE   FUDGE03                                                          
         CLC   GMKMSG,=Y(60)                                                    
         BH    FUDGE02                                                          
         MVI   GMKSYS,0                                                         
         AP    GENCON,=P'1'                                                     
         B     FUDGEEND                                                         
FUDGE02  MVI   GMKSYS,14           IAMBIC IS PART OF THE PERSON SYSTEM          
         AP    IAMBIC,=P'1'                                                     
         B     FUDGEEND                                                         
         SPACE 1                                                                
FUDGE03  CLI   GMKSYS,21           MOVE MEDIA PLANNING/NRS MSG TO SYS 5         
         BNE   FUDGE04                                                          
         MVI   GMKSYS,5                                                         
         AP    MPLNRS,=P'1'                                                     
         B     FUDGEEND                                                         
         SPACE 1                                                                
FUDGE04  CLI   GMKSYS,22           MOVE MEDIA PLANNING BUDGET TO SYS 5          
         BNE   FUDGE05                                                          
         MVI   GMKSYS,5                                                         
         OI    GMKMSG,1            ADD 256 TO MESSAGE NO                        
         AP    MPLBUD,=P'1'                                                     
         B     FUDGEEND                                                         
         SPACE 1                                                                
FUDGE05  EQU   *                                                                
         SPACE 1                                                                
FUDGEEND EQU   *                                                                
*&&                                                                             
*&&US                                                                           
***********************************************************************         
* APPLY ALL U.S. FUDGES                                               *         
***********************************************************************         
*                                                                               
FUDGE01  CLI   GMKSYS,5            MOVE GENCON MSGS TO SYSTEM ZERO              
         BNE   FUDGEEND                                                         
         CLC   GMKMSG,=Y(60)                                                    
         BH    FUDGEEND                                                         
         MVI   GMKSYS,0                                                         
         AP    GENCON,=P'1'                                                     
         SPACE 1                                                                
FUDGEEND EQU   *                                                                
*&&                                                                             
         DROP  R5,R3                                                            
*                                                                               
         EJECT                                                                  
PUTREC   LA    R2,GENTAPE                                                       
         PUT   (2),TAPELEN                                                      
         AP    ERRRECS,=P'1'                                                    
         B     EXIT                                                             
         EJECT                                                                  
* LAST TIME CODE                                                                
*                                                                               
CONVL    DS    0H                                                               
         LA    R2,GENTAPE                                                       
         CLOSE ((2))                                                            
         MVC   P(19),=C'999999 RECORDS READ'                                    
         EDIT  (P4,RECCOUNT),(6,P)                                              
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'999999 ERROR RECORDS O/P'                               
         EDIT  (P4,ERRRECS),(6,P)                                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),=C'999999 ERROR WITH STD MSG'                              
         EDIT  (P4,FNDERR),(6,P)                                                
         GOTO1 VPRINTER                                                         
         MVC   P(28),=C'999999 ERROR WITHOUT STD MSG'                           
         EDIT  (P4,NOTFND),(6,P)                                                
         GOTO1 VPRINTER                                                         
         MVC   P(22),=C'999999 GENCON MESSAGES'                                 
         EDIT  (P4,GENCON),(6,P)                                                
         GOTO1 VPRINTER                                                         
         MVC   P(22),=C'999999 MPLNRS MESSAGES'                                 
         EDIT  (P4,MPLNRS),(6,P)                                                
         GOTO1 VPRINTER                                                         
         MVC   P(22),=C'999999 MPLBUD MESSAGES'                                 
         EDIT  (P4,MPLBUD),(6,P)                                                
         GOTO1 VPRINTER                                                         
         MVC   P(22),=C'999999 IAMBIC MESSAGES'                                 
         EDIT  (P4,IAMBIC),(6,P)                                                
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
LSYS     DC    X'FF'                                                            
GENTAPE  DCB   DDNAME=GENTAPE,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2100                                          
         SPACE 1                                                                
TAPELEN  DS    F                                                                
TAPEBUF  DS    XL2000                                                           
RECCOUNT DC    PL4'0'                                                           
ERRRECS  DC    PL4'0'                                                           
NOTFND   DC    PL4'0'                                                           
FNDERR   DC    PL4'0'                                                           
GENCON   DC    PL4'0'                                                           
MPLNRS   DC    PL4'0'                                                           
MPLBUD   DC    PL4'0'                                                           
IAMBIC   DC    PL4'0'                                                           
         SPACE 2                                                                
TRTAB    DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040407A7B7C7D7E7F' 70-7F                     
         DC    XL16'40818283848586878889404040404040' 80-8F                     
         DC    XL16'40919293949596979899404040404040' 90-9F                     
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' B0-BF                     
         DC    XL16'40818283848586878889404040404040' C0-CF                     
         DC    XL16'40919293949596979899404040404040' D0-DF                     
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                     
         EJECT                                                                  
* CTCONWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENMSG                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENMSG                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025CTCONVERR 09/15/87'                                      
         END                                                                    
