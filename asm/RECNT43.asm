*          DATA SET RECNT43    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T80243A,+0                                                               
         TITLE 'T80243 - REP UNIVISION DISPLAY/EDIT'                            
***********************************************************************         
*        RECNT43 (T80243)  -- UNI DISPLAY/EDIT                        *         
*_____________________________________________________________________*         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* JUN15/89(SNS)-CHANGE CASH/TRADE/NON CM # DATA FIELDS                *         
*              -TO ALLOW EITHER Y/N OR NUMERIC INPUT                  *         
*                                                                     *         
* CNT HISTORY:                                                        *         
*                                                                     *         
* 04APR90 (EFJ) --- USE GETEL MACRO                                   *         
*                                                                     *         
* 03AUG90 (EFJ) --- COMBINE WITH CNT44                                *         
*                                                                     *         
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                               *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T80243   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80243,RR=R5                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R9,RA                                                            
         AH    R9,=H'4096'         4K                                           
         USING TWAWORK,R9                                                       
         SPACE 3                                                                
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
DISP     DS    0H                                                               
         GOTO1 VFOUTBLK,DMCB,UNICSHH,UNILAST,0                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A1'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         USING RUNIEL,R6                                                        
         LA    R2,UNICSH          CASH #                                        
         CLI   RUNICSH,0                                                        
         BE    DISP10                                                           
         MVI   UNICSH,C'Y'        OLD WAY -CHARACTER Y/N                        
         CLI   RUNICSH,C'Y'                                                     
         BE    DISP10                                                           
         ZIC   R1,RUNICSH                                                       
         EDIT  (R1),(2,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         SPACE 1                                                                
DISP10   LA    R2,UNITRDE         DISPLAY TRADE #                               
         CLI   RUNITRDE,0                                                       
         BE    DISP20                                                           
         MVI   UNITRDE,C'Y'                                                     
         CLI   RUNITRDE,C'Y'                                                    
         BE    DISP20                                                           
         ZIC   R1,RUNITRDE                                                      
         EDIT  (R1),(2,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         SPACE 1                                                                
DISP20   LA    R2,UNINON          DISPLAY NON CM #                              
         CLI   RUNINON,0                                                        
         BE    DISP30                                                           
         MVI   UNINON,C'Y'                                                      
         CLI   RUNINON,C'Y'                                                     
         BE    DISP30                                                           
         ZIC   R1,RUNINON                                                       
         EDIT  (R1),(2,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         SPACE 1                                                                
DISP30   DS    0H                                                               
         LA    R3,RUNILOC                                                       
         LA    R2,UNILOC          LOCAL                                         
         MVI   0(R2),C'N'         DEFAULT                                       
         CLI   0(R3),1                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         LA    R2,UNINAT          NATIONAL                                      
         MVI   0(R2),C'N'                                                       
         CLI   0(R3),2                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         LA    R2,UNILOCP         LOCAL POLITICAL                               
         MVI   0(R2),C'N'         DEFAULT                                       
         CLI   0(R3),3                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         LA    R2,UNINATP         NATIONAL POLITICAL                            
         MVI   0(R2),C'N'                                                       
         CLI   0(R3),4                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         SPACE                                                                  
         LA    R3,RUNIBILL        DISPLAY BILLING                               
         LA    R2,UNISTND         STANDARD                                      
         MVI   0(R2),C'N'         DEFAULT                                       
         CLI   0(R3),C'S'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'Y'         IT IS STANDARD                                
         LA    R2,UNICAL          CALANDER                                      
         MVI   0(R2),C'N'         DEFAULT                                       
         CLI   0(R3),C'C'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'Y'         IT IS CALANDER                                
         LA    R2,UNIWKLY         WEEKLY                                        
         MVI   0(R2),C'N'         DEFAULT                                       
         CLI   0(R3),C'W'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'Y'         IT IS WEEKLY                                  
         SPACE                                                                  
         LA    R2,UNIPAMT         DISPLAY PCC                                   
         OC    RUNIPAMT,RUNIPAMT                                                
         BZ    *+10                                                             
         MVC   0(3,R2),RUNIPAMT                                                 
         SPACE 1                                                                
         LA    R2,UNISAMT         DISPLAY SCP                                   
         OC    RUNISAMT,RUNISAMT                                                
         BZ    *+10                                                             
         MVC   0(3,R2),RUNISAMT                                                 
         SPACE 1                                                                
         LA    R2,UNIMPRD         MULTIPLE PRODUCTS?                            
         LA    R3,RUNIMPRD                                                      
         MVI   0(R2),C'N'                                                       
         CLI   0(R3),C'Y'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         LA    R2,UNISEPI         SEPARATE INVOICES?                            
         LA    R3,RUNISEPI                                                      
         MVI   0(R2),C'N'                                                       
         CLI   0(R3),C'Y'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         SPACE                                                                  
         LA    R2,UNISPTP         SPOT PRICES?                                  
         LA    R3,RUNISPTP                                                      
         MVI   0(R2),C'N'                                                       
         CLI   0(R3),C'Y'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'Y'                                                       
         SPACE 1                                                                
         LA    R2,WORK3           DISPLAY REMARK #                              
         XC    WORK3(80),WORK3                                                  
         LA    R3,RUNIRMK                                                       
         DROP  R6                                                               
         LA    R4,4                                                             
DISP40   CLI   0(R3),0                                                          
         BE    DISP50                                                           
         CH    R4,=H'4'           FIRST TIME?                                   
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         EDIT  (B1,0(R3)),(2,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R2,R0                                                            
         LA    R3,1(R3)                                                         
         BCT   R4,DISP40                                                        
*                                                                               
DISP50   MVC   UNIRMK,WORK3                                                     
         SPACE 1                                                                
         GOTO1 VFOUTBLK,DMCB,UNICSHH,UNILAST,1                                  
         B     EXXMOD                                                           
         EJECT                                                                  
EDIT     DS    0H                                                               
*          DATA SET RECNT44    AT LEVEL 119 AS OF 07/13/90                      
         MVC   KEY+28(4),TWAKADDR CONTRACT DISK ADDRESS                         
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         EJECT                                                                  
         USING RUNIEL,R4                                                        
         LA    R4,WORK2                                                         
         XC    RUNIEL(200),RUNIEL                                               
         MVI   RUNICO,X'A1'                                                     
         MVI   RUNILEN,28                                                       
         SPACE 1                                                                
         MVI   ONEFLAG,0                                                        
         LA    R2,UNICSHH                                                       
         CLI   5(R2),0             NO INPUT IS REQUIRED                         
         BE    EDIT10                                                           
         CLI   8(R2),C'N'         SAME AS NO INPUT                              
         BE    EDIT10                                                           
         MVI   RUNICSH,C'Y'       SET CHARACTER DEFAULT                         
         CLI   8(R2),C'Y'                                                       
         BE    EDIT10                                                           
         LA    R3,2               INVALID INPUT                                 
         TM    4(R2),X'08'        INVALID INPUT IF NOT Y/N AND NOT              
         BZ    ERROR              NUMERIC                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,UNICSH(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,RUNICSH                                                       
         MVI   ONEFLAG,1                                                        
         SPACE 1                                                                
EDIT10   LA    R2,UNITRDEH         EDIT TRADE #                                 
         CLI   5(R2),0             NO INPUT IS REQUIRED                         
         BE    EDIT20                                                           
         CLI   8(R2),C'N'         SAME AS NO INPUT                              
         BE    EDIT20                                                           
         LA    R3,224             ONLY ONE ALLOWED                              
         CLI   ONEFLAG,1                                                        
         BE    ERROR                                                            
         MVI   RUNITRDE,C'Y'      DEFAULT CHARACTER                             
         CLI   8(R2),C'Y'                                                       
         BE    EDIT20                                                           
         LA    R3,2                INVALID INPUT                                
         TM    4(R2),X'08'        IF NOT Y/N - MUST BE NUMERIC                  
         BZ    ERROR                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,UNITRDE(0)                                                   
         CVB   R1,DUB                                                           
         STC   R1,RUNITRDE                                                      
         MVI   ONEFLAG,1                                                        
         SPACE 1                                                                
EDIT20   LA    R2,UNINONH          EDIT NON CM #                                
         CLI   5(R2),0             NO INPUT IS REQUIRED                         
         BE    EDIT30                                                           
         CLI   8(R2),C'N'                                                       
         BE    EDIT30                                                           
         LA    R3,224             ONLY ONE ALLOWED                              
         CLI   ONEFLAG,1                                                        
         BE    ERROR                                                            
         MVI   RUNINON,C'Y'                                                     
         CLI   8(R2),C'Y'                                                       
         BE    EDIT30                                                           
         LA    R3,2               INVALID INPUT                                 
         TM    4(R2),X'08'        IF NOT Y/N - MUST BE NUMERIC                  
         BZ    ERROR                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,UNINON(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,RUNINON                                                       
         SPACE 1                                                                
EDIT30   LA    R2,UNILOCH          EDIT LOCAL                                   
         MVI   ONEFLAG,0                                                        
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         MVI   RUNILOC,1                                                        
         MVI   ONEFLAG,1                                                        
         LA    R2,UNINATH                                                       
         CLI   8(R2),C'Y'                                                       
         BNE   EDIT40                                                           
         CLI   ONEFLAG,1                                                        
         BE    ERROR                                                            
         MVI   RUNILOC,2                                                        
         MVI   ONEFLAG,1                                                        
EDIT40   LA    R2,UNILOCPH        EDIT LOCAL POLITICAL                          
         CLI   8(R2),C'Y'                                                       
         BNE   EDIT50                                                           
         CLI   ONEFLAG,1                                                        
         BE    ERROR                                                            
         MVI   RUNILOC,3                                                        
         MVI   ONEFLAG,1                                                        
EDIT50   LA    R2,UNINATPH        EDIT NATIONAL POLITICAL                       
         CLI   8(R2),C'Y'                                                       
         BNE   EDIT60                                                           
         CLI   ONEFLAG,1                                                        
         BE    ERROR                                                            
         MVI   RUNILOC,4                                                        
         MVI   ONEFLAG,1                                                        
*                                                                               
EDIT60   LA    R2,UNIPAMTH        EDIT PCC AMOUNT                               
         CLI   5(R2),0                                                          
         BE    *+10               NO INPUT IS REQUIRED                          
         MVC   RUNIPAMT,8(R2)                                                   
         SPACE 1                                                                
         LA    R2,UNISAMTH        EDIT SPC AMOUNT                               
         CLI   5(R2),0                                                          
         BE    *+10               NO INPUT IS REQUIRED                          
         MVC   RUNISAMT,8(R2)                                                   
         SPACE 1                                                                
         LA    R5,RUNIBILL                                                      
         MVI   ONEFLAG,0                                                        
         LA    R3,224             ONLY ONE ALLOWED                              
         LA    R2,UNISTNDH                                                      
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         MVI   0(R5),C'S'                                                       
         MVI   ONEFLAG,1                                                        
         LA    R2,UNICALH                                                       
         CLI   8(R2),C'Y'                                                       
         BNE   EDIT70             NO INPUT IS REQUIRED                          
         CLI   ONEFLAG,1                                                        
         BE    ERROR              INVALID INPUT                                 
         MVI   0(R5),C'C'                                                       
         MVI   ONEFLAG,1                                                        
EDIT70   LA    R2,UNIWKLYH                                                      
         CLI   8(R2),C'Y'                                                       
         BNE   EDIT80             NO INPUT IS REQUIRED                          
         CLI   ONEFLAG,1                                                        
         BE    ERROR              INVALID INPUT                                 
         MVI   0(R5),C'W'                                                       
         MVI   ONEFLAG,1                                                        
         SPACE 1                                                                
EDIT80   LA    R2,UNIPAMTH        EDIT PCC                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   RUNIPAMT,8(R2)     DATA TO ELEMENT                               
         SPACE                                                                  
         DS    0H                                                               
         LA    R2,UNISAMTH        EDIT SCP                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   RUNISAMT,8(R2)     DATA TO ELEMENT                               
         SPACE                                                                  
         LA    R2,UNIMPRDH        MULTIPLE PRODUCTS?                            
         LA    R3,RUNIMPRD                                                      
         CLI   8(R2),C'Y'         YES                                           
         BNE   *+8                                                              
         MVI   0(R3),C'Y'                                                       
         SPACE                                                                  
         LA    R2,UNISEPIH        SEPARATE INVOICES?                            
         LA    R3,RUNISEPI                                                      
         CLI   8(R2),C'Y'         YES                                           
         BNE   *+8                                                              
         MVI   0(R3),C'Y'                                                       
*                                                                               
         LA    R2,UNISPTPH        SPOT PRICERS?                                 
         LA    R3,RUNISPTP                                                      
         CLI   8(R2),C'Y'         YES                                           
         BNE   *+8                                                              
         MVI   0(R3),C'Y'                                                       
         SPACE 1                                                                
         LA    R8,RUNIRMK                                                       
         LA    R2,UNIRMKH         EDIT REMARK #'S                               
         CLI   5(R2),0                                                          
         BE    EDIT100                                                          
*                                                                               
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CSCANNER,DMCB,(R2),IOAREA,0                                      
         LA    R6,IOAREA                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    EDIT100                                                          
         LA    R3,INVINP                                                        
         CH    R5,=H'4'                                                         
         BH    ERROR                                                            
*                                                                               
EDIT90   CLI   1(R6),0            SECOND HALF?                                  
         BNE   ERROR                                                            
         CLI   0(R6),2            2 LONG                                        
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,R8),7(R6)                                                    
         LA    R6,32(R6)                                                        
         LA    R8,1(R8)                                                         
         BCT   R5,EDIT90                                                        
         DC    0H'0'                                                            
EDIT100  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A1'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT110                                                          
         SPACE 1                                                                
         GOTO1 VDELELEM,DMCB,(X'A1',RCONREC)                                    
EDIT110  GOTO1 VADDELEM,DMCB,RCONREC,(R4)                                       
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
ONEFLAG  DS    XL1                                                              
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTEED                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004RECNT43   05/01/02'                                      
         END                                                                    
