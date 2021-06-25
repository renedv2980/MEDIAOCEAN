*          DATA SET SPNWS40    AT LEVEL 072 AS OF 05/01/02                      
*PHASE T20740A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T20740 - WORKSHEET SCHEDULE'                                    
T20740   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20740,RA,RR=R2,CLEAR=YES                                  
         LR    R6,RC                                                            
         USING WORKD,R6                                                         
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         ST    RD,SAVERD01         SAVE A(REGISTER SAVE AREA)                   
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A8B'                                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDROOL,DMCB                                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A46'                                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSYSDRIV,DMCB                                                    
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PREP                                                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PREP     L     R4,AIO2             A(GLOBAL STORAGE)                            
         USING GLOBALD,R4                                                       
*                                                                               
         L     RF,=V(DUMMY)                                                     
         A     RF,RELO                                                          
         ST    RF,GLAPROG          A(DPG PHASE)                                 
         GOTO1 CALLOV,DMCB,(X'98',GLAPROG),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   GLSIZE,=F'4000'     USE AIO2+AIO3 FOR GLOBALS+DRIVETABLE         
         MVC   GLASYSDR,VSYSDRIV   A(SYSTEM DRIVER)                             
         LA    R1,DRHOOK                                                        
         ST    R1,GLAHOOK          A(APPLICATION HOOK)                          
         ST    RC,GLAWORKD         A(CONTROLLER WORKD)                          
         MVC   GLCOMFAC,ACOMFACS   A(COMFACS)                                   
         MVI   GLMODE,GLINIT       INITIALIZE DROOL                             
         GOTO1 VDROOL,DMCB,(R4),ASPOOLD,TSARBLK,TSARBUFF                        
*                                                                               
         LA    R5,DATATABL         TABLE OF 'RECORDS'                           
         USING DATATABD,R5                                                      
         MVI   GLMODE,GLINPUT      PROCESS AN INPUT RECORD                      
         MVI   INDATA,1            SO TIM'S DRIVER DOESN'T FUCK ME              
*                                                                               
NEXTREC  GOTO1 VDROOL,DMCB,(R4),ASPOOLD,TSARBLK,TSARBUFF                        
         LA    R5,DATALENQ(R5)     BUMP TO NEXT 'RECORD'                        
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   NEXTREC             NO                                           
*                                                                               
* NOW COMES OUTPUT LOGIC                                                        
*                                                                               
*********MVI   FORCEHED,C'Y'                                                    
         LA    R2,DROTXTH          FIRST TEXT FIELD                             
         LA    R3,TSARBUFF                                                      
         LA    R3,8(R3)            BUMP PAST TSAR BUFFER HEADER                 
*                                                                               
NEXTFLD  LA    R5,8(R2)            BUMP PAST TWA FIELD HEADER                   
         GOTO1 HEXOUT,DMCB,(R3),(R5),31,=C'TOG'                                 
         OC    DMCB+16(4),DMCB+16                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)          BUMP TO NEXT TSAR RECORD                     
         AR    R3,RF                                                            
*                                                                               
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF TWA?                                  
         BNE   NEXTFLD             NO                                           
*                                                                               
         MVI   GLMODE,GLOUTPUT     GENERATE THE REPORT                          
         GOTO1 VDROOL,DMCB,(R4),ASPOOLD,TSARBLK,TSARBUFF                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*==============*                                                                
* DRIVER HOOK  *                                                                
*==============*                                                                
*                                                                               
DRHOOK   NTR1                                                                   
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     RECORD IS ABOUT TO BE TSARED                 
         BE    PUTSRT                                                           
*                                                                               
         B     XIT                                                              
*============================*                                                  
* RESOLVE ROUTINE ADDRESSES  *                                                  
*============================*                                                  
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
         A     RF,RELO                                                          
         ST    RF,GLAROUT          ROUTINE ADDRESS                              
*                                                                               
RESOLVEX B     XIT                 RETURN TO DROOL                              
         SPACE 2                                                                
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    C'IMED    ',A(IMED)                                              
         DC    C'OMED    ',A(OMED)                                              
         DC    C'IBYR    ',A(IBYR)                                              
         DC    C'OBYR    ',A(OBYR)                                              
         DC    C'ICAMP   ',A(ICAMP)                                             
         DC    C'OCAMP   ',A(OCAMP)                                             
         DC    C'IMKT    ',A(IMKT)                                              
         DC    C'OMKT    ',A(OMKT)                                              
         DC    C'IWORK   ',A(IWORK)                                             
         DC    C'OWORK   ',A(OWORK)                                             
         DC    C'ISPTWK  ',A(ISPTWK)                                            
         DC    C'OSPTWK  ',A(OSPTWK)                                            
         DC    C'IDEMO   ',A(IDEMO)                                             
         DC    C'ODEMO   ',A(ODEMO)                                             
         DC    C'IDPTLN  ',A(IDPTLN)                                            
         DC    C'ODPTLN  ',A(ODPTLN)                                            
         DC    C'IGPNTS  ',A(IGPNTS)                                            
         DC    C'OGPNTS  ',A(OGPNTS)                                            
         DC    C'IGDOLLS ',A(IGDOLLS)                                           
         DC    C'OGDOLLS ',A(OGDOLLS)                                           
         DC    C'IGCPP   ',A(IGCPP)                                             
         DC    C'OGCPP   ',A(OGCPP)                                             
         DC    C'IBPNTS  ',A(IBPNTS)                                            
         DC    C'OBPNTS  ',A(OBPNTS)                                            
         DC    C'IBDOLLS ',A(IBDOLLS)                                           
         DC    C'OBDOLLS ',A(OBDOLLS)                                           
         DC    C'IBCPP   ',A(IBCPP)                                             
         DC    C'OBCPP   ',A(OBCPP)                                             
         DC    C'ISPOTS  ',A(ISPOTS)                                            
         DC    C'OSPOTS  ',A(OSPOTS)                                            
         DC    C'IAVGPTS ',A(IAVGPTS)                                           
         DC    C'OAVGPTS ',A(OAVGPTS)                                           
         DC    C'IBVGPTS ',A(IBVGPTS)                                           
         DC    C'OBVGPTS ',A(OBVGPTS)                                           
         DC    C'IBVGDOL ',A(IBVGDOL)                                           
         DC    C'OBVGDOL ',A(OBVGDOL)                                           
         DC    C'IDOLLS  ',A(IDOLLS)                                            
         DC    C'ODOLLS  ',A(ODOLLS)                                            
         DC    C'WKSRTN  ',A(WKSRTN)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*======================*                                                        
* EXECUTING ROUTINES   *                                                        
*======================*                                                        
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          RF=A(ROUTINE)                                
         BASR  RE,RF               EXECUTE THE ROUTINE                          
         B     XIT                 RETURN TO DROOL                              
*                                                                               
*===========================*                                                   
* INPUT AND OUTPUT ROUTINES *                                                   
*===========================*                                                   
*                                                                               
***************ROWS***************************                                  
*                                                                               
IMED     DS    0H                 MEDIA INPUT                                   
         BR    RE                                                               
*                                                                               
OMED     DS    0H                 MEDIA OUTPUT                                  
         BR    RE                                                               
         SPACE 2                                                                
IBYR     DS    0H                 BUYER INPUT                                   
         BR    RE                                                               
*                                                                               
OBYR     DS    0H                 BUYER OUTPUT                                  
         BR    RE                                                               
         SPACE 2                                                                
ICAMP    DS    0H                 CAMPAIGN INPUT                                
         BR    RE                                                               
*                                                                               
OCAMP    DS    0H                 CAMPAIGN OUTPUT                               
         BR    RE                                                               
         SPACE 2                                                                
IMKT     DS    0H                 MARKET INPUT                                  
         BR    RE                                                               
*                                                                               
OMKT     DS    0H                 MARKET OUTPUT                                 
         BR    RE                                                               
         SPACE 2                                                                
IDPTLN   DS    0H                 DAYPART-LENGTH INPUT                          
         BR    RE                                                               
*                                                                               
ODPTLN   DS    0H                 DAYPART-LENGTH OUTPUT                         
         BR    RE                                                               
         SPACE 2                                                                
IWORK    DS    0H                 HEADER STUFF (MAIN REPORT)                    
         BR    RE                                                               
*                                                                               
OWORK    DS    0H                 HEADER STUFF (MAIN REPORT)                    
         BR    RE                                                               
         SPACE 2                                                                
*************COLUMNS*************************                                   
IGPNTS   DS    0H                 GOAL POINTS INPUT                             
         BR    RE                                                               
*                                                                               
OGPNTS   DS    0H                 GOAL POINTS OUTPUT                            
         BR    RE                                                               
         SPACE 2                                                                
IGDOLLS  DS    0H                 GOAL DOLLARS INPUT                            
         BR    RE                                                               
*                                                                               
OGDOLLS  DS    0H                 GOAL DOLLARS OUTPUT                           
         BR    RE                                                               
         SPACE 2                                                                
IGCPP    DS    0H                 GOAL COST PER POINT INPUT                     
         BR    RE                                                               
*                                                                               
OGCPP    DS    0H                 GOAL COST PER POINT OUTPUT                    
         BR    RE                                                               
         SPACE 2                                                                
IBPNTS   DS    0H                 BOUGHT POINTS INPUT                           
         BR    RE                                                               
*                                                                               
OBPNTS   DS    0H                 BOUGHT POINTS OUTPUT                          
         BR    RE                                                               
         SPACE 2                                                                
IBDOLLS  DS    0H                 BOUGHT DOLLARS INPUT                          
         BR    RE                                                               
*                                                                               
OBDOLLS  DS    0H                 BOUGHT DOLLARS OUTPUT                         
         BR    RE                                                               
         SPACE 2                                                                
IBCPP    DS    0H                 BOUGHT COST PER POINT INPUT                   
         BR    RE                                                               
*                                                                               
OBCPP    DS    0H                 BOUGHT COST PER POINT OUTPUT                  
         BR    RE                                                               
         SPACE 2                                                                
ISPOTS   DS    0H                 SPOTS INPUT                                   
         BR    RE                                                               
*                                                                               
OSPOTS   DS    0H                 SPOTS OUTPUT                                  
         BR    RE                                                               
         SPACE 2                                                                
IAVGPTS  DS    0H                 AVERAGE POINTS INPUT                          
         BR    RE                                                               
*                                                                               
OAVGPTS  DS    0H                 AVERAGE POINTS OUTPUT                         
         BR    RE                                                               
         SPACE 2                                                                
IBVGPTS  DS    0H                 BOUGHT VS GOAL POINTS INPUT                   
         BR    RE                                                               
*                                                                               
OBVGPTS  DS    0H                 BOUGHT VS GOAL POINTS OUTPUT                  
         BR    RE                                                               
         SPACE 2                                                                
IBVGDOL  DS    0H                 BOUGHT VS GOAL DOLLARS INPUT                  
         BR    RE                                                               
*                                                                               
OBVGDOL  DS    0H                 BOUGHT VS GOAL DOLLARS OUTPUT                 
         BR    RE                                                               
         SPACE 2                                                                
ISPTWK   DS    0H                 SPOTS PER WEEK INPUT                          
         BR    RE                                                               
*                                                                               
OSPTWK   DS    0H                 SPOTS PER WEEK OUTPUT                         
         BR    RE                                                               
         SPACE 2                                                                
IDEMO    DS    0H                 DEMO INPUT                                    
         BR    RE                                                               
*                                                                               
ODEMO    DS    0H                 DEMO OUTPUT                                   
         BR    RE                                                               
         SPACE 2                                                                
**************ROUTINES******************                                        
*                                                                               
WKSRTN   NTR1                                                                   
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
PUTSRT   L     R2,GLAIO            A(TSAR RECORD)                               
*********MVC   P(31),0(R2)                                                      
*********GOTO1 HEXOUT,DMCB,(R2),P2,31,=C'TOG'                                   
*********OC    DMCB+16(4),DMCB+16                                               
*********BNZ   *+6                                                              
*********DC    H'0'                                                             
*********GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
*                                                                               
         LTORG                                                                  
         SPACE 5                                                                
DATATABL DS    0D                                                               
         DC    C'TOY',AL3(3,0321),C'PUZ   *',F'01',F'03',F'10'                  
         DC    C'PG ',AL3(1,0321),C'BO    *',F'02',F'04',F'08'                  
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
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
VDROOL   DS    V                   A(DROOL)                                     
VSYSDRIV DS    V                   A(SYSTEM DRIVER)                             
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
TSARBUFF DS    6144X               TSAR CORE BUFFER                             
*                                                                               
WORKL    EQU   *-WORKD                                                          
         SPACE 5                                                                
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
         SPACE 5                                                                
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIEFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPWRIWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072SPNWS40   05/01/02'                                      
         END                                                                    
