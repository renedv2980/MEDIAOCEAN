*          DATA SET SPNWS99    AT LEVEL 038 AS OF 11/11/02                      
*PHASE T20799C                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'NWS99 T20799  BUYER''S WORK SHEET - TEST REPORT'                
T20799   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20799**,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
         L     RA,AGLOBAL                                                       
         USING GLOBALD,RA                                                       
*                                                                               
         CLI   APMODE,APMVALQ        VALIDATE THE REQUEST                       
         BE    VALQ                                                             
         CLI   APMODE,APMDRINI       INITIALIZE FOR DRIVER/DROOL                
         BE    INIT                                                             
         CLI   APMODE,APMDRINP       INPUT FOR DRIVER/DROOL                     
         BE    INPUT                                                            
         CLI   APMODE,APMDROUT       OUTPUT FOR DRIVER/DROOL                    
         BE    OUT                                                              
         CLI   APMODE,APMDRHK        HOOK FOR DRIVER/DROOL                      
         BE    DRHOOK                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE REQUEST                                                *         
***********************************************************************         
*                                                                               
VALQ     MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH                                                    
         BNE   VALQX                                                            
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
         CLI   ASONOFF,ASON        TEST ONLINE                                  
         BNE   VALQ2                                                            
*                                                                               
         GOTO1 AVALWHEN,REPWENH    VALIDATE WHEN                                
         BNE   VALQX                                                            
*                                                                               
         GOTO1 AVALDEST,REPDIDH    VALIDATE DESTINATION ID                      
         BNE   VALQX                                                            
*                                                                               
         MVC   GLAGENCY,CUAALF                                                  
         MVC   GLOPTS(L'REPOPTS),REPOPTS                                        
*                                                                               
VALQ2    B     VALQX                                                            
*                                                                               
VALQX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR DRIVER/DROOL                                     *         
* SETS GLAPROG=A(DPG PROGRAM)                                         *         
*      GLASYSDR=A(SYSTEM DRIVER)                                      *         
***********************************************************************         
*                                                                               
INIT     CLI   ASONOFF,ASOFF                                                    
         BNE   *+8                                                              
         MVI   GLTWORKD,GLTSPBWS   OFFLINE ONLY                                 
*                                                                               
         L     RF,=V(DUMMY)                                                     
         A     RF,APRELO                                                        
         ST    RF,GLAPROG          A(DPG PHASE)                                 
         LA    RF,SPARE                                                         
         ST    RF,GLADTAB                                                       
         LH    RF,=Y(SPAREX-SPARE)                                              
         ST    RF,GLSIZE                                                        
         LA    RF,REPTXTH                                                       
         ST    RF,APPARM                                                        
*                                                                               
         CLI   APACTN,ACTDRP       ON-SCREEN REPORT?                            
         BE    *+12                YES -- NO SPECS                              
         LA    RF,SPECS                                                         
         ST    RF,GLASPECS                                                      
*                                                                               
         GOTO1 VCOLY,DMCB,(X'97',GLAPROG),0,0                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,DATATABL         TABLE OF 'RECORDS'                           
         SH    R1,=Y(DATALENQ)                                                  
         ST    R1,ATHISREC                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INPUT FOR DRIVER/DROOL                                              *         
* SETS APMODE=APMDREND WHEN FINISHED READING RECORDS                  *         
***********************************************************************         
*                                                                               
INPUT    L     R4,ATHISREC                                                      
         LA    R4,DATALENQ(R4)                                                  
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    *+12                                                             
         ST    R4,ATHISREC                                                      
         B     EXIT                                                             
*                                                                               
         MVI   APMODE,APMDREND                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT FOR DRIVER/DROOL                                             *         
* CALLED BEFORE DRIVER/DROOL IS CALLED FOR OUTPUT                     *         
***********************************************************************         
*                                                                               
OUT      CLI   APACTN,ACTDRP       ON-SCREEN REPORT?                            
         BE    EXIT                YES                                          
*                                                                               
         LA    R2,REPTXTH          FIRST TEXT FIELD                             
         OI    6(R2),X'80'         XMIT                                         
         L     R3,APPARM                                                        
         LA    R3,8(R3)            BUMP PAST TSAR BUFFER HEADER                 
*                                                                               
NEXTFLD  LA    R5,8(R2)            BUMP PAST TWA FIELD HEADER                   
         MVC   HALF,0(R3)                                                       
         OC    HALF,HALF                                                        
         BZ    EXIT                NO MORE RECORDS                              
*                                                                               
         LH    RF,HALF                                                          
         SH    RF,=H'2'            DON'T DISPLAY VARIABLE RECLEN                
         ST    RF,DMCB+8                                                        
         SLA   RF,1                NUMBER OF EBCDIC CHARACTERS                  
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'8'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         CR    RE,RF                                                            
         BNL   *+8                                                              
         SRA   RE,1                                                             
         ST    RE,DMCB+8                                                        
         GOTO1 VHEXOUT,DMCB,2(R3),(R5),,=C'TOG'                                 
         OC    DMCB+16(4),DMCB+16                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RF,HALF             BUMP TO NEXT TSAR RECORD                     
         AR    R3,RF                                                            
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF TWA?                                  
         BE    EXIT                YES                                          
*                                                                               
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            FOR EX AS WELL                               
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR THE NEXT FIELD                         
         OI    6(R2),X'80'         XMIT                                         
         B     NEXTFLD                                                          
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     EXECUTE INTERNAL COMPUTE                     
         BE    EXEC                                                             
         CLI   GLHOOK,GLHEAD                                                    
         BE    HEADHK                                                           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* RESOLVE ROUTINE ADDRESSES                                                     
*                                                                               
RESOLVE  LA    R1,ROUTLIST         TEST ROUTINE IN THIS OVERLAY                 
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    RESOLVEX                                                         
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
*                                                                               
         L     RF,8(R1)                                                         
         A     RF,APRELO                                                        
         ST    RF,GLAROUT          ROUTINE ADDRESS                              
*                                                                               
RESOLVEX B     EXIT                RETURN TO DROOL                              
         SPACE 2                                                                
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    C'ICLTU   ',A(ICLTU)                                             
         DC    C'IESTU   ',A(IESTU)                                             
         DC    C'IMKTU   ',A(IMKTU)                                             
         DC    C'IPRDU   ',A(IPRDU)                                             
         DC    C'IGLDOLU ',A(IGLDOLU)                                           
         DC    C'IGLDEMU ',A(IGLDEMU)                                           
*********DC    C'IGLCPPU ',A(IGLCPPU)                                           
*********DC    C'IBYDOLU ',A(IBYDOLU)                                           
*********DC    C'IBYPDEMU',A(IBYPDEMU)                                          
*********DC    C'IBYPCPPU',A(IBYPCPPU)                                          
         DC    C'OCLTU   ',A(OCLTU)                                             
         DC    C'OESTU   ',A(OESTU)                                             
         DC    C'OMKTU   ',A(OMKTU)                                             
         DC    C'OPRDU   ',A(OPRDU)                                             
         DC    C'OGLDOLU ',A(OGLDOLU)                                           
         DC    C'OGLDEMU ',A(OGLDEMU)                                           
         DC    C'OCOMP   ',A(OCOMP)                                             
         DC    C'HCLTU   ',A(HCLTU)                                             
         DC    C'HGLDEMU ',A(HGLDEMU)                                           
         DC    C'HGLDOLU ',A(HGLDOLU)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* EXECUTING ROUTINES                                                            
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          RF=A(ROUTINE)                                
         L     R4,ATHISREC                                                      
         USING DATATABD,R4                                                      
         BASR  RE,RF               EXECUTE THE ROUTINE                          
         B     EXIT                RETURN TO DROOL                              
         SPACE 3                                                                
ICLTU    MVC   0(L'TABCLT,R2),TABCLT                                            
         BR    RE                                                               
         SPACE 2                                                                
IESTU    MVC   0(L'TABEST,R2),TABEST                                            
         BR    RE                                                               
         SPACE 2                                                                
IMKTU    MVC   0(L'TABMKT,R2),TABMKT                                            
         BR    RE                                                               
         SPACE 2                                                                
IPRDU    MVC   0(L'TABPRDP,R2),TABPRDP                                          
         BR    RE                                                               
         SPACE 2                                                                
IGLDOLU  MVC   0(L'TABGLDOL,R2),TABGLDOL                                        
         BR    RE                                                               
         SPACE 2                                                                
IGLDEMU  MVC   0(L'TABGLDEM,R2),TABGLDEM                                        
         MVC   4(L'TABGLDEM,R2),TABGLDEM+4                                      
         BR    RE                                                               
         SPACE 2                                                                
***IGLCPPU  MVC   0(L'TABGLCPP,R2),TABGLCPP                                     
***         MVC   4(L'TABGLCPP,R2),TABGLCPP+4                                   
***         MVC   8(L'TABGLCPP,R2),TABGLCPP+8                                   
***         BR    RE                                                            
***         SPACE 2                                                             
***IBYDOLU  MVC   0(L'TABBYDOL,R2),TABBYDOL                                     
***         BR    RE                                                            
***         SPACE 2                                                             
***IBYPDEMU MVC   0(L'TABBYPDM,R2),TABBYPDM                                     
***         MVC   4(L'TABBYPDM,R2),TABBYPDM+4                                   
***         BR    RE                                                            
***         SPACE 2                                                             
***IBYPCPPU MVC   0(L'TABBYPCP,R2),TABBYPCP                                     
***         MVC   4(L'TABBYPCP,R2),TABBYPCP+4                                   
***         MVC   8(L'TABBYPCP,R2),TABBYPCP+8                                   
***         BR    RE                                                            
         SPACE 2                                                                
OCLTU    MVC   0(L'TABCLT,R3),0(R2)                                             
         BR    RE                                                               
         SPACE 2                                                                
OESTU    SR    R0,R0                                                            
         ICM   R0,7,0(R2)                                                       
         EDIT  (R0),(3,(R3))                                                    
         BR    RE                                                               
         SPACE 2                                                                
OMKTU    SR    R0,R0                                                            
         ICM   R0,7,0(R2)                                                       
         EDIT  (R0),(4,(R3))                                                    
         BR    RE                                                               
         SPACE 2                                                                
OPRDU    MVC   0(L'TABPRD,R3),0(R2)                                             
         BR    RE                                                               
         SPACE 2                                                                
OGLDOLU  L     R0,0(R2)                                                         
         EDIT  (R0),(8,(R3))                                                    
         BR    RE                                                               
         SPACE 2                                                                
OGLDEMU  CLI   GLHOOK,GLINCOMP     EXECUTE INTERNAL COMPUTE?                    
         BNE   OGLDEMU2            NO                                           
         SPACE                                                                  
         L     R0,0(R2)            YES -- ADD TWO COLUMNS FOR RANK              
         A     R0,4(R2)                                                         
         ST    R0,0(R2)                                                         
         MVI   GLHOOK,GLIDID                                                    
         BR    RE                                                               
         SPACE 2                                                                
OGLDEMU2 L     R0,0(R2)                                                         
         EDIT  (R0),(8,(R3))       SUM OF TWO DEMOS                             
         BR    RE                                                               
         SPACE 2                                                                
OCOMP    L     R0,0(R2)                                                         
         EDIT  (R0),(8,(R3)),2                                                  
         BR    RE                                                               
         SPACE 2                                                                
HCLTU    MVC   0(3,R3),=C'CLT'                                                  
         BR    RE                                                               
         SPACE 2                                                                
HGLDOLU  MVC   0(6,R3),=C'DOLLAR'                                               
         BR    RE                                                               
         SPACE 2                                                                
HGLDEMU  MVC   0(5,R3),=C'DEMOS'                                                
         BR    RE                                                               
         SPACE 3                                                                
HEADHK   L     R9,AREP                                                          
         USING REPD,R9                                                          
*                                                                               
*********MVC   REPH1+40(6),=C'TITLE:'                                           
*                                                                               
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT                                                                  
SPECS    DS    0X                                                               
         SPEC  H1,50,C'TEST DROOL REPORT'                                       
         SPEC  END                                                              
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
DATATABL DS    0D                                                               
         DC    C'TOY',AL3(3,0321),C'PUZ   *',F'01',F'03',F'10'                  
         DC    C'PG ',AL3(1,0321),C'BO    *',F'02',F'04',F'50'                  
         DC    C'PG ',AL3(1,1521),C'GLM   *',F'03',F'07',F'30'                  
         DC    C'TOY',AL3(1,1521),C'YO    *',F'04',F'00',F'09'                  
         DC    C'TOY',AL3(1,1521),C'PUZ   *',F'05',F'08',F'02'                  
         DC    C'PG ',AL3(3,0321),C'BO    *',F'06',F'04',F'13'                  
         DC    C'PG ',AL3(3,1521),C'BO    *',F'07',F'07',F'11'                  
         DC    C'TOY',AL3(1,0321),C'PUZ   *',F'08',F'10',F'04'                  
         DC    C'PG ',AL3(1,1521),C'GLM   *',F'09',F'00',F'00'                  
         DC    C'TOY',AL3(1,1521),C'YO    *',F'10',F'14',F'05'                  
         DC    C'PG ',AL3(3,1521),C'GLM   *',F'11',F'00',F'20'                  
         DC    C'TOY',AL3(3,1521),C'PUZ   *',F'12',F'22',F'09'                  
         DC    C'TOY',AL3(3,1521),C'YO    *',F'13',F'11',F'07'                  
         DC    C'PG ',AL3(1,1521),C'BO    *',F'14',F'09',F'11'                  
         DC    X'FF'                                                            
         SPACE 3                                                                
SPARE    DS    5000X                                                            
SPAREX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
*                                                                               
LOCALD   DSECT                                                                  
         DS    0D                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
ATHISREC DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL17                                                             
         SPACE 3                                                                
DATATABD DSECT                                                                  
TABCLT   DS    CL3                                                              
TABEST   DS    XL3                                                              
TABMKT   DS    XL3                                                              
TABPRDP  DS    0CL6                                                             
TABPRD   DS    CL3                                                              
TABPRTNR DS    CL3                                                              
         DS    CL1                                                              
TABGLDOL DS    XL4                                                              
TABGLDEM DS    2XL4                                                             
***TABGLCPP DS    3XL4                                                          
***TABBYDOL DS    XL4                                                           
***TABBYPDM DS    2XL4                                                          
***TABBYPCP DS    3XL4                                                          
DATALENQ EQU   *-DATATABD                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         EJECT                                                                  
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF1D                                                       
         SPACE 2                                                                
         ORG   SAVOVER                                                          
*                                  CAN SAVE DATA HERE                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         SPACE 3                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPNWS99   11/11/02'                                      
         END                                                                    
