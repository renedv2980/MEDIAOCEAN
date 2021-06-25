*          DATA SET ACBAT64    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T61B64A                                                                  
BAT64    TITLE '- BATCH PROGRAM BATCH TYPE RECORDS'                             
BAT64    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BA64**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         LH    R5,=Y(BSDICTU-TWAD)                                              
         LA    R5,TWAD(R5)                                                      
         USING BSDICTU,R5                                                       
         L     RC,AOVERWRK                                                      
         USING OVERWRK,RC                                                       
         ST    RE,BORELO                                                        
*                                                                               
         CLI   TWASCRN,TYPESCRN    TEST FOR FIRST TIME                          
         BE    TYP02                                                            
         GOTO1 AOVRSCR,BOPARM,('TYPESCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$ENTKY)                                           
         LA    RE,TYPBTYH                                                       
         ST    RE,FVADDR                                                        
         B     XIT                                                              
*                                                                               
TYP02    BAS   RE,VALKEY           VALIDATE THE KEY                             
         BNE   XIT                                                              
         BAS   RE,READBTY                                                       
         BNE   TYP10                                                            
         CLC   OSKEY,OSKEYSAV      TEST CHANGE OF KEY                           
         BNE   TYP04                                                            
         BAS   RE,TSTINP           TEST ANY CHANGES INPUT                       
         BNE   TYP06                                                            
         BAS   RE,VALREC           VALIDATE THE RECORD                          
         BNE   XIT                                                              
         BAS   RE,WRITEBTY         WRITE RECORD TO FILE                         
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RCENK)                                           
         LA    RE,TYPBTYH                                                       
         ST    RE,FVADDR                                                        
         B     XIT                                                              
*                                                                               
TYP04    BAS   RE,DISREC           DISPLAY RECORD                               
TYP06    MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         LA    RE,TYPACC1H                                                      
         ST    RE,FVADDR                                                        
         B     XIT                                                              
*                                                                               
TYP10    BAS   RE,VALREC           ADD NEW RECORD                               
         BNE   XIT                                                              
         BAS   RE,ADDBTY                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RAENX)                                           
         LA    RE,TYPBTYH                                                       
         ST    RE,FVADDR                                                        
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   NTR1  ,                                                                
         LA    R2,OSKEY            INITIALIZE KEY                               
         USING BTYREC,R2                                                        
         MVC   OSKEYSAV,OSKEY                                                   
         XC    BTYKEY,BTYKEY                                                    
         MVI   BTYKTYP,BTYKTYPQ                                                 
*                                                                               
         MVI   FVMINL,1            VALIDATE BATCH TYPE                          
         GOTO1 AVALBTY,TYPBTYH                                                  
         BNE   VALKEYN                                                          
         MVC   TYPBTY,BCWORK                                                    
         OI    TYPBTYH+FHOID,FHOITR                                             
         MVC   BTYKBTY,CSBTYP                                                   
*                                                                               
         OI    TYPCTRH+FHOID,FHOITR                                             
         MVI   FVMINL,2            VALIDATE COUNTRY                             
         GOTO1 AFVAL,TYPCTRH                                                    
         BNE   VALKEYN                                                          
         LA    R4,CTRYTAB1                                                      
         USING CTRYTABD,R4         R4=A(COUNTRY TABLE)                          
*                                                                               
VKEY02   CLI   CTRYTABD,FF         TEST EOT                                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYN                                                          
*                                                                               
         CLC   CTRYSHR(2),FVIFLD                                                
         BE    VKEY10                                                           
         CLC   CTRYNAM(2),FVIFLD                                                
         BE    VKEY10                                                           
         CLC   CTRYSHRN(2),FVIFLD                                               
         BE    VKEY10                                                           
         CLC   CTRYNAMN(2),FVIFLD                                               
         BE    VKEY10                                                           
*                                                                               
         LA    R4,CTRYTABL(R4)                                                  
         B     VKEY02                                                           
*                                                                               
VKEY10   MVC   TYPCTR,CTRYNAMN                                                  
         MVC   BTYKCTRY,CTRYCODE                                                
*                                                                               
VALKEYY  CR    RB,RB                                                            
         B     XIT                                                              
VALKEYN  XC    OSKEY,OSKEY                                                      
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO DISPLAY RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISREC   NTR1  ,                                                                
         GOTO1 DISLUL,BOPARM,('LULTACC1',TYPACC1H)                              
         GOTO1 (RF),(R1),('LULTACC2',TYPACC2H)                                  
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO VALIDATE RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALREC   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING BTYREC,R2                                                        
         MVC   BTYKEY,OSKEY                                                     
         LA    R0,BTYDATA+1-BTYREC                                              
         STCM  R0,3,BTYLEN                                                      
         MVI   BTYSTAT,0                                                        
         MVI   BTYDATA,0                                                        
         GOTO1 VALLUL,BOPARM,('LULTACC1',TYPACC1H)                              
         BNE   XIT                                                              
         GOTO1 (RF),(R1),('LULTACC2',TYPACC2H)                                  
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST OF UNIT LEDGERS                             *         
*                                                                     *         
* NTRY: P1=(LIST TYPE, A(FIELD HEADER))                               *         
***********************************************************************         
         SPACE 1                                                                
DISLUL   NTR1  ,                                                                
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)                                                       
         USING FHD,R4              R4=A(FIELD HEADER)                           
         OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA                                                      
         IC    RF,FHLN             CLEAR FIELD                                  
         SH    RF,=Y(FHDAD+1)                                                   
         EX    RF,*+4                                                           
         XC    FHDA(0),FHDA                                                     
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,BTYDATA-BTYREC(R2)                                            
         USING LULELD,R2           R2=A(ELEMENT)                                
         XR    RF,RF                                                            
DLUL02   CLI   LULEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   LULEL,LULELQ                                                     
         BNE   DLUL08                                                           
         CLC   LULTYPE,0(R1)                                                    
         BE    DLUL12                                                           
         CLI   LULTYPE,LULTFRST                                                 
         BE    DLUL10                                                           
DLUL08   IC    RF,LULLN                                                         
         BXH   R2,RF,DLUL02                                                     
*                                                                               
DLUL10   CLI   0(R1),LULTACC1                                                   
         BNE   DISLULX                                                          
         MVC   FHDA(L'UC@FIRST),UC@FIRST                                        
         B     DISLULX                                                          
*                                                                               
DLUL12   LA    R6,L'LULDATA        R6=L'SUB-EL.                                 
         XR    R7,R7                                                            
         IC    R7,LULLN                                                         
         BCTR  R7,0                                                             
         LA    R7,LULELD(R7)       R7=A(END OF ELEMENT)                         
         LA    R3,LULDATA                                                       
         USING LULDATA,R3                                                       
         LA    R1,FHDA                                                          
*                                                                               
DLUL14   MVC   0(L'LULDUL,R1),LULDUL                                            
         LA    R1,L'LULDUL(R1)                                                  
         CLI   LULDTYPE,LULDTCAC                                                
         BNE   DLUL16                                                           
         MVC   0(L'BCEQUAL,R1),BCEQUAL                                          
         MVI   L'BCEQUAL(R1),LULDTCAC                                           
         LA    R1,L'BCEQUAL+L'LULDTCAC(R1)                                      
*                                                                               
DLUL16   MVC   0(L'BCCOMMA,R1),BCCOMMA                                          
         LA    R1,L'BCCOMMA(R1)                                                 
         BXLE  R3,R6,DLUL14                                                     
         BCTR  R1,0                                                             
         MVI   0(R1),0                                                          
*                                                                               
DISLULX  B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LIST OF UNIT LEDGERS                            *         
*                                                                     *         
* NTRY: P1=(LIST TYPE, A(FIELD HEADER))                               *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLUL   NTR1  ,                                                                
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)                                                       
         USING FHD,R4              R4=A(FIELD HEADER)                           
*                                                                               
         TM    OWINDS,OWIFRST                                                   
         BZ    VLUL02                                                           
         IC    RF,FHLN             CLEAR FIELD                                  
         SH    RF,=Y(FHDAD+1)                                                   
         EX    RF,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         B     VALLULY                                                          
*                                                                               
VLUL02   XC    BOELEM,BOELEM                                                    
         LA    R2,BOELEM                                                        
         USING LULELD,R2           R2=A(ELEMENT)                                
         MVI   LULEL,LULELQ                                                     
         MVI   LULLN,LULLNQ                                                     
         MVC   LULTYPE,0(R1)                                                    
*                                                                               
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,FHD                                                        
         BNE   VALLULN2                                                         
*                                                                               
         LA    R3,LULDATA                                                       
         USING LULDATA,R3                                                       
         CLI   LULTYPE,LULTACC1                                                 
         BNE   VLUL04                                                           
         CLI   FVILEN,3                                                         
         BL    VLUL04                                                           
         CLI   FVILEN,L'UC@FIRST                                                
         BH    VLUL04                                                           
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),UC@FIRST                                               
         BNE   VLUL04                                                           
         MVC   FHDA(L'UC@FIRST),UC@FIRST                                        
         OI    OWINDS,OWIFRST                                                   
         MVI   LULTYPE,LULTFRST                                                 
         B     VLUL20                                                           
*                                                                               
VLUL04   LA    R1,FVIFLD                                                        
         XR    R0,R0                                                            
         IC    R0,FVXLEN                                                        
         AR    R0,R1               R0=A(END OF FIELD)                           
*                                                                               
VLUL12   CLI   0(R1),C' '                                                       
         BNH   VLUL20                                                           
         CLI   1(R1),C' '                                                       
         BNH   VALLULN                                                          
*                                                                               
         MVI   LULDTYPE,LULDTACC                                                
         MVC   LULDUL,0(R1)                                                     
         LA    R1,L'LULDUL(R1)                                                  
         CLC   BCEQUAL,0(R1)                                                    
         BNE   VLUL14                                                           
         MVC   LULDTYPE,L'BCEQUAL(R1)                                           
         LA    R1,L'BCEQUAL+L'LULDTYPE(R1)                                      
         CLI   LULDTYPE,LULDTACC                                                
         BE    VLUL14                                                           
         CLI   LULDTYPE,LULDTCAC                                                
         BNE   VALLULN                                                          
*                                                                               
VLUL14   LA    R3,L'LULDATA(R3)                                                 
         CR    R1,R0                                                            
         BH    VLUL20                                                           
         CLC   BCCOMMA,0(R1)                                                    
         BNE   VALLULN                                                          
         LA    R1,L'BCCOMMA(R1)                                                 
         B     VLUL12                                                           
         DROP  R3                                                               
*                                                                               
VLUL20   SR    R3,R2                                                            
         STC   R3,LULLN                                                         
         GOTO1 VHELLO,BOPARM,(C'P',CTFILE),AIO1,LULELD,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALLULY  OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA                                                      
         CR    RB,RB                                                            
         B     XIT                                                              
VALLULN  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALLULN2 NI    FHII,FF-FHIIVA                                                   
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST ANY CHANGES IN RECORD DATA FIELDS                   *         
***********************************************************************         
         SPACE 1                                                                
TSTINP   NTR1  ,                                                                
         LA    R2,TYPACC1H                                                      
         USING FHD,R2                                                           
         XR    RF,RF                                                            
*                                                                               
TINP02   ICM   RF,1,FHLN                                                        
         BZ    TSTINPN                                                          
         TM    FHAT,FHATPR                                                      
         BO    *+12                                                             
         TM    FHII,FHIIVA                                                      
         BZ    TSTINPY                                                          
         BXH   R2,RF,TINP02                                                     
*                                                                               
TSTINPY  CR    RB,RB               CHANGES                                      
         B     XIT                                                              
TSTINPN  LTR   RB,RB               NO CHANGES                                   
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ BATCH TYPE RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
READBTY  NTR1  ,                                                                
         MVC   IOKEY(L'OSKEY),OSKEY                                             
         GOTO1 AIO,IORD+IO1+IOCTFILE                                            
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO WRITE BATCH TYPE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
WRITEBTY NTR1  ,                                                                
         BAS   RE,SWITCH                                                        
         MVC   IOKEY(L'OSKEY),OSKEY                                             
         GOTO1 AIO,IORDUP+IO2+IOCTFILE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOPUT+IO1+IOCTFILE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD BATCH TYPE RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDBTY   NTR1  ,                                                                
         BAS   RE,SWITCH                                                        
         GOTO1 AIO,IOADD+IO1+IOCTFILE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUITNE TO SWITCH TO CONTROL SYSTEM                                 *         
***********************************************************************         
         SPACE 1                                                                
SWITCH   NTR1  ,                                                                
         GOTO1 VSWITCH,BOPARM,CTL,0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COUNTRY TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE FACTRYTAB                                                      
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
CTL      DC    C'CTL'                                                           
         EJECT                                                                  
       ++INCLUDE CTGENBTY                                                       
         EJECT                                                                  
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATA7D                                                       
         ORG   OSVALS                                                           
OSKEY    DS    XL(L'BTYKEY)                                                     
OSKEYSAV DS    XL(L'BTYKEY)                                                     
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* FACTRY                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
         EJECT                                                                  
WORKD    DSECT                     OVERLAY W/S                                  
         ORG   OVERWRK                                                          
OWINDS   DS    XL1                 * INDICATORS *                               
OWIFRST  EQU   X'80'               USE FIRST ASKEL                              
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACBAT64   08/10/00'                                      
         END                                                                    
